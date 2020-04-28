### Title:    Run Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-09-21
### Modified: 2020-04-28

rm(list = ls(all = TRUE))

                                        #args <- commandArgs(trailingOnly = TRUE) # extract commandline arguments

## Initialize the environment:
source("init.R")

## Run the simulation:
if(parallel) {
    runTime <- system.time(
        parLapply(cl    = cl,
                  X     = startRep : stopRep,
                  fun   = doRep,
                  conds = conds,
                  parms = parms)
    )
    stopCluster(cl)
} else
    runTime <- system.time(doRep(rp = 1, conds = conds, parms = parms))

## Save the run time:
saveRDS(runTime,
        paste0(outDir,
               "runTime-",
               format(Sys.time(), "%Y%m%d_%H:%M:%S"),
               ".rds")
        )
