### Title:    Analysis Subroutines for Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-11-16
### Modified: 2019-11-08

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

## Estimate the mode of a continuous variable:
getMode <- function(x) {
    d <- density(x)
    d$x[which.max(d$y)]
}

###--------------------------------------------------------------------------###
