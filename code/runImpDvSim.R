### Title:    Run Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-09-21
### Modified: 2019-11-05

rm(list = ls(all = TRUE))

t0 <- proc.time() # store start time

args <- commandArgs(trailingOnly = TRUE) # extract commandline arguments

library(parallel)
source("supportFunctions.R")

## Which study are we running?
studyNo <- as.numeric(args[1])

## Setup parallelization environment:
clusterSize <- as.numeric(args[2])
startRep    <- as.numeric(args[3])
stopRep     <- as.numeric(args[4])

## Define levels of variable simulation parameters:
varList <- list()
varList$nVec       <- c(500, 250, 100)
varList$pmVec      <- c(0.1, 0.2, 0.4)
varList$r2Vec      <- c(0.15, 0.3, 0.6)
varList$clVec      <- c(0.0, 0.1, 0.3, 0.5)
varList$auxPropVec <- c(1.0, 0.75, 0.5, 0.25, 0.0)

## Define the fixed simulation parameters:
parms <- list()
parms$verbose    <- FALSE
parms$nImps      <- 100
parms$miceIters  <- 10
parms$outDir     <- args[5]
parms$incompVars <- c("y", "x1")
parms$auxVars    <- switch(studyNo,
                           list(c("z1", "z2"), c("z1", "z2")),
                           list("y", c("z1", "z2"))
                           )
parms$missType   <- c("upper", "lower")
parms$coefs      <- matrix(c(1.0, 0.33, 0.33, 0.33))
parms$varNames   <- c("y", "x1", "z1", "z2")
parms$model      <- as.formula("y ~ x1 + z1")
parms$mySeed     <- 235711
parms$maxStreams <- 500
parms$testing    <- FALSE
parms$nReps      <- stopRep - startRep + 1


goBabyGo <- function(rp, varList, parms)
{
    ## Setup the PRNG:
    .lec.SetPackageSeed(rep(parms$mySeed, 6))
    if( !rp %in% .lec.GetStreams() )
        .lec.CreateStream(c(1 : parms$maxStreams))
    .lec.CurrentStream(rp)

    ## Loop over conditions:
    for(cl in varList$clVec) {
        sigmaX <-
            matrix(cl, length(parms$varNames) - 1, length(parms$varNames) - 1)
        diag(sigmaX) <- 1.0
        parms$sigmaX <- sigmaX

        for(r2 in varList$r2Vec) {
            parms$rSquared <- r2
            parms$nObs <- max(varList$nVec)
            simData <- simRegData(parms)

            for(n in varList$nVec) {
                simData <- simData[1 : n, ]

                for(ap in varList$auxPropVec) {
                    parms$auxWts <- matrix(c(ap, (1 - ap)))

                    for(pm in varList$pmVec) {
                        parms$pm <- pm
                        missData <- imposeMissing(simData, parms)

                        runCell(rp       = rp,
                                simData  = simData,
                                missData = missData,
                                parms    = parms)
                    }# CLOSE for(pm in varList$pmVec)
                }# CLOSE for(ap in varList$auxPropVec)
            }# CLOSE for(n in varList$nVec)
        }# CLOSE for(r2 in varList$r2Vec)
    }# CLOSE for(cl in varList$clVec)

    rp # return rep index
}# END goBabyGo()


## Run in parallel:
cl <- makeCluster(clusterSize, type = "MPI")

clusterCall(cl = cl, fun = source, file = "supportFunctions.R")
clusterCall(cl      = cl,
            fun     = applyLib,
            pkgList = c("rlecuyer", "mvtnorm", "mice", "mitools")
            )

parLapply(cl      = cl,
          X       = c(startRep : stopRep),
          fun     = goBabyGo,
          varList = varList,
          parms   = parms)

stopCluster(cl)

## Calculate and save the overall run time:
runTime <- proc.time() - t0
saveRDS(runTime, "runTime.rds")
