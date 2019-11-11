### Title:    Run Imputed DV Simulation in Serial
### Author:   Kyle M. Lang
### Created:  2015-09-21
### Modified: 2019-11-11
### Note:     - Parallelization can be implemented via shell scripting
###           - This option is preferred for working on the Lisa cluster

rm(list = ls(all = TRUE))

                                        #args <- commandArgs(trailingOnly = TRUE) # extract commandline arguments

## Initialize the environment:
source("init-serial.R")

## Run the simulation:
runTime <- system.time(doRep(rp = rep, conds = conds, parms = parms))

## Save the run time:
saveRDS(runTime,
        paste0(outDir,
               "runTime-",
               format(Sys.time(), "%Y%m%d_%H:%M:%S"),
               ".rds")
        )
