### Title:    Run Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-09-21
### Modified: 2019-11-08

rm(list = ls(all = TRUE))

                                        #args <- commandArgs(trailingOnly = TRUE) # extract commandline arguments

## Initialize the environment:
source("init.R")

## Run the simulation:
if(parallel) {
    runTime <- system.time(
        parLapply(cl    = cl,
                  X     = c(startRep : stopRep),
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

out1 <- readRDS(paste0(outDir, "compOut_n500_rs60_cx50_ap25_pm40_rep1.rds"))
out2 <- readRDS(paste0(outDir, "compOut_n500_rs60_cx50_ap25_pm20_rep1.rds"))

out1
out2

out1 - out2
