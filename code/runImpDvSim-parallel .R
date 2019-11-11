### Title:    Run Imputed DV Simulation in Parallel using the 'parallel' Package
### Author:   Kyle M. Lang
### Created:  2015-09-21
### Modified: 2019-11-11
### Note:     -This script will implement parallelization via the R package
###            'parallel'. 
###           -This approach works well for parallelizing over CPU cores on a
###            local machine, but parallelization via the 'parallel' package
###            is not the preferred way to implement parallel processing on the
###            Lisa cluster.
###           -Parallel processing on Lisa will be implemented outside of R, via
###            shell scripts.

rm(list = ls(all = TRUE))

                                        #args <- commandArgs(trailingOnly = TRUE) # extract commandline arguments

## Initialize the environment:
source("init-parallel.R")

## Run the simulation:
runTime <- system.time(
    parLapply(cl    = cl,
              X     = startRep : stopRep,
              fun   = doRep,
              conds = conds,
              parms = parms)
)
stopCluster(cl)

## Save the run time:
saveRDS(runTime,
        paste0(outDir,
               "runTime-",
               format(Sys.time(), "%Y%m%d_%H:%M:%S"),
               ".rds")
        )
