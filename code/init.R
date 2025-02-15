### Title:    Initialize Environment and Parameters for Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2019-11-08
### Modified: 2020-04-29

library(parallel)
source("simMissingness.R")
source("simulationSubroutines.R")

## Define a vector of necessary packages:
packages <- c("rlecuyer", "mvtnorm", "mice", "mitools")

## Which study are we running?
                                        #studyNo <- as.numeric(args[1])
studyNo <- 1

## Setup parallelization environment:
                                        #startRep    <- as.numeric(args[2])
                                        #stopRep     <- as.numeric(args[3])
                                        #parallel    <- as.logical(args[4])
                                        #clusterSize <- as.numeric(args[5])
                                        #outDir      <- args[6]
startRep    <- 1
stopRep     <- 3
parallel    <- TRUE
clusterSize <- 3
outDir      <- "../output/test2/"

## Define levels of variable simulation parameters:
n   <- c(500, 250, 100)                 # Sample size
pm  <- c(0.1, 0.2, 0.4)                 # Proportion missing
r2  <- c(0.15, 0.3, 0.6)                # R-Squared
cx  <- c(0.0, 0.1, 0.3, 0.5)            # Predictor covariance
ap  <- c(1.0, 0.75, 0.5, 0.25, 0.0)     # Proportion of true auxiliaries modeled
imp <- c(100, 50, 25, 20, 15, 10, 5, 2) # Number of imputations

conds <- expand.grid(imp = imp, pm = pm, ap = ap, n = n, r2 = r2, cx = cx)

## Define the fixed simulation parameters:
parms <- list()
parms$verbose    <- FALSE
parms$miceIters  <- 20
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
parms$mySeed     <- 235711
parms$nStreams   <- 500
parms$nObs       <- max(conds$n)

## Create the output directory, if necessary:
if(!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)

## Initialize a cluster, if necessary:
if(parallel) {
    cl <- makeCluster(clusterSize, type = "PSOCK") # Use type = "MPI" when running on cluster
    
    clusterCall(cl = cl, fun = source, file = "simMissingness.R")
    clusterCall(cl = cl, fun = source, file = "simulationSubroutines.R")
    clusterCall(cl = cl, fun = applyLib, pkgList = packages)
} else {
    applyLib(packages)
}
