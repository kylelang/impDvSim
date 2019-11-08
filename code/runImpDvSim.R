### Title:    Run Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-09-21
### Modified: 2019-11-08

rm(list = ls(all = TRUE))

t0 <- proc.time() # store start time

args <- commandArgs(trailingOnly = TRUE) # extract commandline arguments

library(parallel)
source("simMissingness.R")
source("supportFunctions.R")

## Which study are we running?
studyNo <- as.numeric(args[1])

## Setup parallelization environment:
clusterSize <- as.numeric(args[2])
startRep    <- as.numeric(args[3])
stopRep     <- as.numeric(args[4])

## Define levels of variable simulation parameters:
n       <- c(500, 250, 100)
pm      <- c(0.1, 0.2, 0.4)
r2      <- c(0.15, 0.3, 0.6)
cl      <- c(0.0, 0.1, 0.3, 0.5)
auxProp <- c(1.0, 0.75, 0.5, 0.25, 0.0)

conds <- expand.grid(cl = cl, r2 = r2, n = n, prop = auxProp, pm)

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
parms$nObs       <- max(conds$n)



## Run in parallel:
cl <- makeCluster(clusterSize, type = "MPI")

clusterCall(cl = cl, fun = source, file = "supportFunctions.R")
clusterCall(cl      = cl,
            fun     = applyLib,
            pkgList = c("rlecuyer", "mvtnorm", "mice", "mitools", "optimx")
            )

parLapply(cl    = cl,
          X     = c(startRep : stopRep),
          fun   = doRep,
          conds = conds,
          parms = parms)

stopCluster(cl)

## Calculate and save the overall run time:
runTime <- proc.time() - t0
saveRDS(runTime, "runTime.rds")
