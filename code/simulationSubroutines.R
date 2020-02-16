### Title:    Subroutines for Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-11-16
### Modified: 2019-11-11

###--------------------------------------------------------------------------###

## Simulated some toy data:
simData <- function(parms) {
    ## Generate predictor covariance matrix:
    p        <- length(parms$varNames) - 1
    sX       <- matrix(parms$covX, p, p)
    diag(sX) <- 1.0

    ## Simulate predictor data:
    X <- rmvnorm(n = parms$nObs, mean = rep(0, p), sigma = sX)

    ## Extract coefficients:
    alpha  <- parms$coefs[1, ]
    beta   <- matrix(parms$coefs[-1, ])
    
    ## Define residual variance:
    signal <- t(beta) %*% cov(X) %*% beta
    sY     <- (signal / parms$r2) - signal

    ## Simulate outcome data:
    y <- alpha + X %*% beta + rnorm(parms$nObs, 0, sqrt(sY))

    outData           <- data.frame(y, X)
    colnames(outData) <- parms$varNames

    outData
}# END simData()

###--------------------------------------------------------------------------###

## Impose missing data:
imposeMissing <- function(data, parms) {
    for(v in 1 : length(parms$incompVars)) {
        ## Are we simulating MNAR missingness?
        mnar <- with(parms,
                     length(auxVars[v]) == 1 && incompVars[v] == auxVars[v]
                     ) 

        if(mnar) mdBeta <- 1
        else     mdBeta <- parms$auxWts
        
        ## Generate nonresponse vector:
        rVec <- with(parms, 
                     simMissingness(pm    = pm,
                                    data  = data,
                                    preds = auxVars[[v]],
                                    type  = missType[v],
                                    beta  = mdBeta)
                     )
        data[rVec, parms$incompVars[v]] <- NA
    }
    data
}# END imposeMissing()

###--------------------------------------------------------------------------###

## MI-based analyses:
fitModels <- function(data, parms) {
    if(is.data.frame(data)) { # We're analyzing a single dataset
        lmFit <- try(
            lm(parms$model, data = data, na.action = "na.omit"),
            silent = TRUE
        )
    }
    else { # We're analyzing multiply imputed data
        fitList <- try(
            lapply(data,
                   FUN = function(miData, parms) lm(parms$model, data = miData),
                   parms = parms),
            silent = TRUE
        )

        if(class(fitList) != "try-error") lmFit <- MIcombine(fitList)
        else                              lmFit <- fitList
    }
    lmFit
}# END fitModels()

###--------------------------------------------------------------------------###

getStats <- function(lmOut, parms) {
    if(class(lmOut) != "try-error") {
        ## Compute Wald stats and significance:
        waldVec <- coef(lmOut) / sqrt(diag(vcov(lmOut)))
        sigVec  <- 2 * pt(abs(waldVec), df = lmOut$df, lower = FALSE) < 0.05
        
        ## Compute CIs:
        ciVec <- as.vector(confint(lmOut))

        ## Aggregate and return output:
        outVec <- c(coef(lmOut), ciVec, sigVec)

        names(outVec) <- c("int", "x", "z1",
                           "intLb", "xLb", "z1Lb",
                           "intUb", "xUb", "z1Ub",
                           "intSig", "xSig", "z1Sig")
    }
    else
        outVec <- list("LM_CONVERGENCE_FAILURE", lmOut)
    
    outVec
}# END getStats()

###--------------------------------------------------------------------------###

## Do all computations for a single set of crossed conditions:
runCell <- function(rp, compData, missData, parms) {
    ## Impute the missingess:
    miceOut <- with(parms,
                    mice(data      = missData,
                         m         = nImps,
                         maxit     = miceIters,
                         method    = "norm",
                         printFlag = verbose)
                    )
    
    ## Fill the imputed data sets:
    rVec    <- is.na(missData$y)
    impList <- impList2 <- list()
    for(m in 1 : parms$nImps) {
        impList[[m]] <- complete(miceOut, m)
        ## Implement the MID deletion:
        impList2[[m]] <- impList[[m]][!rVec, ]
    }

    ## Fit complete data model:
    compFit <- fitModels(compData, parms)
    compOut <- coef(compFit)

    ## Fit listwise deleted models:
    ldFit <- fitModels(missData, parms)
    ldOut <- getStats(ldFit, parms)

    ## Fit MI models
    miFit <- fitModels(impList, parms)
    miOut <- getStats(miFit, parms)

    ## Fit MID models:
    midFit <- fitModels(impList2, parms)
    midOut <- getStats(midFit, parms)

    ## Create a condition tag to label output objects:
    tag1 <- with(parms,
                 paste0("_n", nrow(compData),
                        "_rs", 100 * r2,
                        "_cx", 100 * covX)
                 )
    tag2 <- with(parms,
                 paste0(tag1,
                        "_ap", 100 * auxWts[1],
                        "_pm", 100 * pm)
                 )

    ## Save the current parameter set:
    if(rp == 1)
        saveRDS(parms,
                file = paste0(parms$outDir,
                              "parms",
                              tag1,
                              ".rds")
                )

    ## Save the results:
    if(parms$newData | parms$newN)
        saveRDS(compOut,
                file = paste0(parms$outDir,
                              "compOut",
                              tag1,
                              "_rep",
                              rp,
                              ".rds")
                )

    saveRDS(ldOut,
            file = paste0(parms$outDir,
                          "ldOut",
                          tag2,
                          "_rep",
                          rp,
                          ".rds")
            )

    saveRDS(miOut,
            file = paste0(parms$outDir,
                          "miOut",
                          tag2,
                          "_rep",
                          rp,
                          ".rds")
            )

    saveRDS(midOut,
            file = paste0(parms$outDir,
                          "midOut",
                          tag2,
                          "_rep",
                          rp,
                          ".rds")
            )
}# END runCell()

###--------------------------------------------------------------------------###

## Run a single replication of the simulation:
doRep <- function(rp, conds, parms) {
    ## Setup the PRNG:
    .lec.SetPackageSeed(rep(parms$seed, 6))
    if(!rp %in% .lec.GetStreams())
        .lec.CreateStream(c(1 : parms$nStreams))
    .lec.CurrentStream(rp)
    
    ## Loop over conditions:
    for(i in 1 : nrow(conds)) {
        ## Save the current values (possibly NULL) of covX and r2 to check if
        ## we need to simulate new data:
        cx <- parms$covX
        r2 <- parms$r2
        
        ## Update the values of covX and r2: 
        parms$covX <- conds[i, "cx"]
        parms$r2   <- conds[i, "r2"]
        
        ## Update parms with number of imputations
        parms$nImps <- conds[i, "nImps"]
        
        ## Simulate new complete data, if covX or r2 have changed:
        check <-
            (is.null(cx) | is.null(r2)) || (cx != parms$covX | r2 != parms$r2)
        if(check) {
            compData      <- simData(parms)
            parms$newData <- TRUE
        }
        else parms$newData <- FALSE
        
        ## Subset the complete data, if the sample size has changed:
        n0 <- ifelse(i > 1, n, 0)
        n  <- conds[i, "n"]
        if(n != n0) {
            compData   <- compData[1 : n, ]
            parms$newN <- TRUE
        }
        else parms$newN <- FALSE
        
        ## Define the missing data-related design parameters:
        ap           <- conds[i, "ap"]
        parms$auxWts <- matrix(c(ap, (1 - ap)))
        parms$pm     <- conds[i, "pm"]
        
        ## Generate missing data:
        missData <- imposeMissing(compData, parms)

        ## Run the computations for the current condition:
        runCell(rp       = rp,
                compData = compData,
                missData = missData,
                parms    = parms)
    }

    rp # return rep index
}

###--------------------------------------------------------------------------###

## Broadcast the library function of a list of packages:
applyLib <- function(pkgList)
    lapply(pkgList, library, character.only = TRUE, logical = TRUE)

###--------------------------------------------------------------------------###
