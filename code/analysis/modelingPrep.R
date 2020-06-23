### Title:    Prepare Simulation Results for Modeling
### Author:   Kyle M. Lang
### Created:  2020-06-23
### Modified: 2020-06-23

rm(list = ls(all = TRUE))

resDir <- "../../results/exp1/"

source("analysisSubroutines.R")

## Read in the processed output:
out <- readRDS(paste0(resDir, "pooledOutput-20200429.rds"))

## Compile and save data frames of modelable data:
for(i in c("dev", "ciw", "cic", "sig")) {
    tmp <- prepModelData(out = out, type = i)
    saveRDS(tmp, paste0(resDir, i, "_model_data.rds"))
}
    
