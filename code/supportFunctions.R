### Title:    Supporting Functions for Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-11-16
### Modified: 2019-11-05

###--------------------------------------------------------------------------###

## Simulated some toy data:
simRegData <- function(parms) {
    alpha <- parms$coefs[1, ]
    beta  <- parms$coefs[-1, ]

    X <- rmvnorm(n     = parms$nObs,
                 mean  = rep(0, ncol(parms$sigmaX)),
                 sigma = parms$sigmaX)

    signal <- t(beta) %*% cov(X) %*% beta
    sigma2 <- (signal / parms$rSquared) - signal

    y <- rep(alpha, parms$nObs) + X %*% beta +
        rnorm(parms$nObs, 0, sqrt(sigma2))

    simData           <- data.frame(y, X)
    colnames(simData) <- parms$varNames

    simData
}# END simRegData()

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
runCell <- function(rp, simData, missData, parms) {
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
    compFit <- fitModels(simData, parms)
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

## Summarize the simulation findings:
calcOutcomeMeasures <- function(outList, trueStats = NULL) {
    colNames <- c("int", "x1", "z1")
    rowNames <- c("lwd", "mi", "mid")

    compOutMat <- do.call("rbind", outList$comp)

    if(is.null(trueStats))
        trueStats  <- colMeans(compOutMat)

    ldOutMat  <- do.call("rbind", outList$ld)
    miOutMat  <- do.call("rbind", outList$mi)
    midOutMat <- do.call("rbind", outList$mid)

    ldMeans  <- colMeans(ldOutMat)
    miMeans  <- colMeans(miOutMat)
    midMeans <- colMeans(midOutMat)

    sigMat <-
        rbind(ldMeans[grep("sig", names(ldMeans), ignore = TRUE)],
              miMeans[grep("sig", names(miMeans), ignore = TRUE)],
              midMeans[grep("sig", names(midMeans), ignore = TRUE)]
              )
    colnames(sigMat) <- colNames
    rownames(sigMat) <- rowNames

    ldStatMat <-
        ldOutMat[ , -grep("lb|ub|sig", colnames(ldOutMat), ignore = TRUE)]
    miStatMat <-
        miOutMat[ , -grep("lb|ub|sig", colnames(miOutMat), ignore = TRUE)]
    midStatMat <-
        midOutMat[ , -grep("lb|ub|sig", colnames(midOutMat), ignore = TRUE)]

    ldStats  <- colMeans(ldStatMat)
    miStats  <- colMeans(miStatMat)
    midStats <- colMeans(midStatMat)

    upperNames <-
        colnames(ldOutMat)[grep("ub", colnames(ldOutMat), ignore = TRUE)]
    lowerNames <-
        colnames(ldOutMat)[grep("lb", colnames(ldOutMat), ignore = TRUE)]

    ciwMat <-
        rbind(colMeans(ldOutMat[ , upperNames] - ldOutMat[ , lowerNames]),
              colMeans(miOutMat[ , upperNames] - miOutMat[ , lowerNames]),
              colMeans(midOutMat[ , upperNames] - midOutMat[ , lowerNames])
              )
    colnames(ciwMat) <- colNames
    rownames(ciwMat) <- rowNames

    trueStatMat <-
        matrix(trueStats, nrow(miOutMat), length(trueStats), byrow = TRUE)

    cicMat <-
        rbind(colMeans(ldOutMat[ , upperNames] > trueStatMat &
                           ldOutMat[ , lowerNames] < trueStatMat),
              colMeans(miOutMat[ , upperNames] > trueStatMat &
                           miOutMat[ , lowerNames] < trueStatMat),
              colMeans(midOutMat[ , upperNames] > trueStatMat &
                           midOutMat[ , lowerNames] < trueStatMat)
              )
    colnames(cicMat) <- colNames
    rownames(cicMat) <- rowNames

    ldBias  <- ldStats - trueStats
    miBias  <- miStats - trueStats
    midBias <- midStats - trueStats

    ## PRB:
    prbMat <- rbind(100 * ldBias / trueStats,
                    100 * miBias / trueStats,
                    100 * midBias / trueStats)
    rownames(prbMat) <- rowNames

    ## MCSD:
    mcsdMat <- rbind(apply(ldStatMat, 2, sd),
                     apply(miStatMat, 2, sd),
                     apply(midStatMat, 2, sd))
    rownames(mcsdMat) <- rowNames

    ## SB:
    sbMat <- rbind(ldBias / mcsdMat["lwd", ],
                   miBias / mcsdMat["mi", ],
                   midBias / mcsdMat["mid", ])
    rownames(sbMat) <- rowNames

    list(prb  = prbMat,
         sb   = sbMat,
         mcsd = mcsdMat,
         sig  = sigMat,
         cic  = cicMat,
         ciw  = ciwMat)
}# END calcOutcomeMeasures()

###--------------------------------------------------------------------------###

## Stack the simulation findings for analysis:
stackResults <- function(outList, conds) {
    compOutMat <- do.call("rbind", outList$comp)
    ldOutMat   <- do.call("rbind", outList$ld)
    miOutMat   <- do.call("rbind", outList$mi)
    midOutMat  <- do.call("rbind", outList$mid)

    ## Fill-out design matrix block:
    dMat <- matrix(rep(conds, 500), nrow = 500, byrow = TRUE)

    ## Point estimate deviances:
    iDevFrame <- data.frame(dMat,
                            miOutMat[ , "int"]  - compOutMat[ , "(Intercept)"],
                            midOutMat[ , "int"] - compOutMat[ , "(Intercept)"],
                            ldOutMat[ , "int"]  - compOutMat[ , "(Intercept)"]
                            )

    xDevFrame <- data.frame(dMat,
                            miOutMat[ , "x"]  - compOutMat[ , "x1"],
                            midOutMat[ , "x"] - compOutMat[ , "x1"],
                            ldOutMat[ , "x"]  - compOutMat[ , "x1"]
                            )

    zDevFrame <- data.frame(dMat,
                            miOutMat[ , "z1"]  - compOutMat[ , "z1"],
                            midOutMat[ , "z1"] - compOutMat[ , "z1"],
                            ldOutMat[ , "z1"]  - compOutMat[ , "z1"]
                            )

    colnames(iDevFrame) <- colnames(zDevFrame) <- colnames(zDevFrame) <-
        c(names(conds), "mi", "mid", "lwd")

    ## Coverage flags:
    iSigFrame <- data.frame(dMat,
                            miOutMat[ , "intSig"],
                            midOutMat[ , "intSig"],
                            ldOutMat[ , "intSig"])

    xSigFrame <- data.frame(dMat,
                            miOutMat[ , "xSig"],
                            midOutMat[ , "xSig"],
                            ldOutMat[ , "xSig"])

    zSigFrame <- data.frame(dMat,
                            miOutMat[ , "z1Sig"],
                            midOutMat[ , "z1Sig"],
                            ldOutMat[ , "z1Sig"])

    colnames(iSigFrame) <- colnames(zSigFrame) <- colnames(zSigFrame) <-
        c(names(conds), "mi", "mid", "lwd")

    ## CI Widths:
    w <- diag(c(1, -1))
    iCiwFrame <- data.frame(dMat,
                            rowSums(miOutMat[ , c("intUb", "intLb")]  %*% w),
                            rowSums(midOutMat[ , c("intUb", "intLb")] %*% w),
                            rowSums(ldOutMat[ , c("intUb", "intLb")]  %*% w)
                            )

    xCiwFrame <- data.frame(dMat,
                            rowSums(miOutMat[ , c("xUb", "xLb")]  %*% w),
                            rowSums(midOutMat[ , c("xUb", "xLb")] %*% w),
                            rowSums(ldOutMat[ , c("xUb", "xLb")]  %*% w)
                            )

    zCiwFrame <- data.frame(dMat,
                            rowSums(miOutMat[ , c("z1Ub", "z1Lb")]  %*% w),
                            rowSums(midOutMat[ , c("z1Ub", "z1Lb")] %*% w),
                            rowSums(ldOutMat[ , c("z1Ub", "z1Lb")]  %*% w)
                            )

    colnames(iCiwFrame) <- colnames(zCiwFrame) <- colnames(zCiwFrame) <-
        c(names(conds), "mi", "mid", "lwd")

    ## Update the hash-map:
    map$iDevFrame <<- rbind.data.frame(map$iDevFrame, iDevFrame)
    map$xDevFrame <<- rbind.data.frame(map$xDevFrame, xDevFrame)
    map$zDevFrame <<- rbind.data.frame(map$zDevFrame, zDevFrame)

    map$iSigFrame <<- rbind.data.frame(map$iSigFrame, iSigFrame)
    map$xSigFrame <<- rbind.data.frame(map$xSigFrame, xSigFrame)
    map$zSigFrame <<- rbind.data.frame(map$zSigFrame, zSigFrame)

    map$iCiwFrame <<- rbind.data.frame(map$iCiwFrame, iCiwFrame)
    map$xCiwFrame <<- rbind.data.frame(map$xCiwFrame, xCiwFrame)
    map$zCiwFrame <<- rbind.data.frame(map$zCiwFrame, zCiwFrame)
} # END stackResults()

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
