### Title:    Subroutines for Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-11-16
### Modified: 2019-11-08

###--------------------------------------------------------------------------###

## Simulated some toy data:
simData <- function(parms) {
    alpha <- parms$coefs[1, ]
    beta  <- parms$coefs[-1, ]

    X <- rmvnorm(n     = parms$nObs,
                 mean  = rep(0, ncol(parms$sigmaX)),
                 sigma = parms$sigmaX)

    signal <- t(beta) %*% cov(X) %*% beta
    sigma2 <- (signal / parms$rSquared) - signal

    y <- rep(alpha, parms$nObs) + X %*% beta +
        rnorm(parms$nObs, 0, sqrt(sigma2))

    outData           <- data.frame(y, X)
    colnames(outData) <- parms$varNames

    outData
}# END simData()

###--------------------------------------------------------------------------###

## Impose missing data:
imposeMissing <- function(inData, parms) {
    incompVars <- parms$incompVars
    auxVars    <- parms$auxVars
    auxWts     <- parms$auxWts
    pm         <- parms$pm
    missType   <- parms$missType

    for(v in 1 : length(incompVars)) {
        if(length(auxVars[[v]]) > 1)
            linPred <- as.matrix(inData[ , auxVars[[v]]]) %*% auxWts
        else
            linPred <- inData[ , auxVars[[v]]]

        pVec <- pnorm(linPred, mean(linPred), sd(linPred))

        if(missType[v] == "tails") {
            rVec <- pVec < (pm / 2) | pVec > (1 - (pm/2))
        } else if(missType[v] == "center") {
            rVec <- pVec > (0.5 - (pm / 2)) & pVec < (0.5 + (pm / 2))
        } else if(missType[v] == "lower") {
            rVec <- pVec < pm
        } else if(missType[v] == "upper") {
            rVec <- pVec > (1 - pm)
        } else {
            stop("Please provide a valid 'missType'")
        }

        inData[rVec, incompVars[v]] <- NA
    }
    inData
}# END imposeMissing()

###--------------------------------------------------------------------------###

## MI-based analyses:
fitModels <- function(inData, parms) {
    if(is.data.frame(inData)) {# We're analyzing a single dataset
        lmFit <- try(
            lm(parms$model, data = inData, na.action = "na.omit"),
            silent = TRUE
        )
    } else {# We're analyzing multiply imputed data
        fitList <- try(
            lapply(inData,
                   FUN = function(impData, parms)
                       lm(parms$model, data = impData),
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
    } else {
        outVec <- list("LM_CONVERGENCE_FAILURE", lmOut)
    }
    outVec
}# END getStats()

###--------------------------------------------------------------------------###

## Do all computations for a single set of crossed conditions:
runCell <- function(rp, compData, missData, parms) {
    ## Impute the missingess:
    miceOut <- mice(data      = missData,
                    m         = parms$nImps,
                    maxit     = parms$miceIters,
                    method    = "norm",
                    printFlag = parms$verbose)

    ## Fill the imputed data sets:
    rVec <- is.na(missData$y)
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

    ## Save Results:
    if(parms$testing) {
        fnCore <- ""
    } else {
        fnCore <- paste0("_n", nrow(missData),
                         "_rs", parms$rSquared,
                         "_cl", parms$sigmaX[2, 1],
                         "_ap", parms$auxWts[1],
                         "_pm", parms$pm)
    }

    if(rp == 1) saveRDS(parms,
                        file =
                            paste0(parms$outDir,
                                   "parms", fnCore,
                                   ".rds")
                        )

    saveRDS(compOut,
            file =
                paste0(parms$outDir,
                       "compOut", fnCore,
                       "_rep", rp,
                       ".rds")
            )

    saveRDS(ldOut,
            file =
                paste0(parms$outDir,
                       "ldOut", fnCore,
                       "_rep", rp,
                       ".rds")
            )

    saveRDS(miOut,
            file =
                paste0(parms$outDir,
                       "miOut", fnCore,
                       "_rep", rp,
                       ".rds")
            )

    saveRDS(midOut,
            file =
                paste0(parms$outDir,
                       "midOut", fnCore,
                       "_rep", rp,
                       ".rds")
            )
}# END runCell()

###--------------------------------------------------------------------------###

## Run a single replication of the simulation:
doRep <- function(rp, conds, parms) {
    ## Setup the PRNG:
    .lec.SetPackageSeed(rep(parms$mySeed, 6))
    if(!rp %in% .lec.GetStreams())
        .lec.CreateStream(c(1 : parms$maxStreams))
    .lec.CurrentStream(rp)
    
    ## Loop over conditions:
    for(i in 1 : nrow(conds)) {
        ## Extract design parameters for the current replication:
        cl <- conds[i, "cl"]
        n  <- conds[i, "n"]

        ap           <- conds[i, "prop"]
        parms$auxWts <- matrix(c(ap, (1 - ap)))
        
        parms$r2 <- conds[i, "r2"]
        parms$pm <- conds[i, "pm"]

        ## Define the predictors' covariance matrix:
        sigmaX <-
            matrix(cl, length(parms$varNames) - 1, length(parms$varNames) - 1)
        diag(sigmaX) <- 1.0
        parms$sigmaX <- sigmaX

        ## Simulate and subset the complete data:
        compData <- simData(parms)
        compData <- compData[1 : n, ]
        
        ## Generate missing data:
        missData <- imposeMissing(compData, parms)

        ## Run the computations for the current condition:
        runCell(rp = rp, compData = compData, missData = missData, parms = parms)
    }

    rp # return rep index
}

###--------------------------------------------------------------------------###

## Broadcast the library function of a list of packages:
applyLib <- function(pkgList)
    lapply(pkgList, library, character.only = TRUE, logical = TRUE)

###--------------------------------------------------------------------------###

## Estimate the mode of a continuous variable:
getMode <- function(x) {
    d <- density(x)
    d$x[which.max(d$y)]
}

###--------------------------------------------------------------------------###
