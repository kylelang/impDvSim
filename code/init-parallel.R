### Title:    Initialize Environment and Parameters for Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2019-11-08
### Modified: 2019-11-11

library(parallel)
source("simulationSubroutines.R")

## Define a vector of necessary packages:
packages <- c("rlecuyer", "mvtnorm", "mice", "mitools")

## Which study are we running?
studyNo <- 2
                                        #studyNo <- as.numeric(args[1])

## Setup parallelization environment:
startRep    <- 1
stopRep     <- 3
clusterSize <- 3
outDir      <- "../output/test1/"
                                        #startRep    <- as.numeric(args[2])
                                        #stopRep     <- as.numeric(args[3])
                                        #clusterSize <- as.numeric(args[4])
                                        #outDir      <- args[5]

## Define levels of variable simulation parameters:
n  <- c(500, 250, 100)             # Sample size
pm <- c(0.1, 0.2, 0.4)             # Proportion missing
r2 <- c(0.15, 0.3, 0.6)            # R-Squared
cx <- c(0.0, 0.1, 0.3, 0.5)        # Predictor covariance
ap <- c(1.0, 0.75, 0.5, 0.25, 0.0) # Proportion of true auxiliaries modeled

conds <- expand.grid(pm = pm, ap = ap, n = n, r2 = r2, cx = cx)

## Define the fixed simulation parameters:
parms <- list()
parms$verbose    <- FALSE
parms$nImps      <- 10
parms$miceIters  <- 10
parms$outDir     <- outDir
parms$incompVars <- c("y", "x1")
parms$auxVars    <- switch(studyNo,
                           list(c("z1", "z2"), c("z1", "z2")),
                           list("y", c("z1", "z2"))
                           )
parms$missType   <- c("high", "low")
parms$coefs      <- matrix(c(1.0, 0.33, 0.33, 0.33))
parms$varNames   <- c("y", "x1", "z1", "z2")
parms$model      <- as.formula("y ~ x1 + z1")
parms$seed       <- 235711
parms$nStreams   <- 500
parms$nObs       <- max(conds$n)

## Initialize a cluster:
cl <- makeCluster(clusterSize)

## Setup the environment on worker nodes:
clusterCall(cl = cl, fun = source, file = "simMissingness.R")
clusterCall(cl = cl, fun = source, file = "simulationSubroutines.R")
clusterCall(cl = cl, fun = applyLib, pkgList = packages)
