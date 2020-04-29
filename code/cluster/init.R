### Title:    Initialize Environment and Parameters for Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2019-11-08
### Modified: 2020-04-28
### Note:     This version is meant to be run on the Lisa cluster using the
###           Stopos environment to implement parallelization.

## Initialize subroutines:
source("simMissingness.R")
source("simulationSubroutines.R")

## Load the necessary packages:
applyLib(c("rlecuyer", "mvtnorm", "mice", "mitools"))

## Parse command line arguments:
rp      <- as.numeric(args[1]) # Which replication is this?
studyNo <- as.numeric(args[2]) # Which study are we running?
outDir  <- args[3]             # Where should we write the output?

## Create the output directory, if necessary:
if(!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)

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
