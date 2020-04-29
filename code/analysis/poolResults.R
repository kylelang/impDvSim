### Title:    Pool/Process Results of Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-11-16
### Modified: 2020-04-29

rm(list = ls(all = TRUE))

outDir   <- "../../output/test1/"
saveDir  <- "../../results/test1/"
saveDate <- format(Sys.time(), "%Y%m%d")
nReps    <- 6

source("analysisSubroutines.R")

## Define levels of variable simulation parameters:
n   <- c(500, 250, 100)                 # Sample size
pm  <- c(0.1, 0.2, 0.4)                 # Proportion missing
r2  <- c(0.15, 0.3, 0.6)                # R-Squared
cx  <- c(0.0, 0.1, 0.3, 0.5)            # Predictor covariance
ap  <- c(1.0, 0.75, 0.5, 0.25, 0.0)     # Proportion of true auxiliaries modeled
imp <- c(100, 50, 25, 20, 15, 10, 5, 2) # Number of imputations

conds <- expand.grid(imp = imp, pm = pm, ap = ap, n = n, r2 = r2, cx = cx)

out  <- list()
reps <- rep(0, nrow(conds))
for(i in 1 : nrow(conds)) {
    
    ## Create a condition tag to label output objects:
    tag1 <- with(conds[i, ],
                 paste0("_n", n,
                        "_rs", 100 * r2,
                        "_cx", 100 * cx)
                 )
    tag2 <- with(conds[i, ],
                 paste0(tag1,
                        "_ap", 100 * ap,
                        "_pm", 100 * pm)
                 )
    tag3 <- with(conds[i, ],
                 paste0(tag2, "_imp", imp)
                 )
    
    out0 <- list()
    for(rp in 1 : nReps) {
        compName <- paste0(outDir,
                           "compOut", tag1,
                           "_rep", rp,
                           ".rds")
        ldName   <- paste0(outDir,
                           "ldOut", tag2,
                           "_rep", rp,
                           ".rds")
        miName   <- paste0(outDir,
                           "miOut", tag3,
                           "_rep", rp,
                           ".rds")
        midName  <- paste0(outDir,
                           "midOut", tag3,
                           "_rep", rp,
                           ".rds")

        test1 <-
            file.exists(compName) &
                file.exists(ldName) &
                    file.exists(miName) &
                        file.exists(midName)

        if(test1) {
            reps[i]         <- reps[i] + 1
            out0$conds      <- conds[i, ]
            out0$comp[[rp]] <- readRDS(compName)
            out0$ld[[rp]]   <- readRDS(ldName)
            out0$mi[[rp]]   <- readRDS(miName)
            out0$mid[[rp]]  <- readRDS(midName)
        }
    }# END for(rp in 1 : nReps)

    out[[i]] <- out0
}# END for(i in 1 : nrow(conds)

## Calculate the outcome measures:
res <- list()
for(i in 1 : nrow(conds)) {
    tmp <- list()
    for(j in c("ld", "mi", "mid"))
        tmp <- calcOutcomes(outList = out[[i]], what = j)
    res[[i]] <- unlist(tmp)
}

## Aggregate outcome measures into a data frame:
res <- data.frame(conds, do.call(rbind, res))

## Save processed results:
saveRDS(reps, file = paste0(saveDir, "repCounts-", saveDate, ".rds"))
saveRDS(out, file = paste0(saveDir, "pooledOutput-", saveDate, ".rds"))
saveRDS(res, file = paste0(saveDir, "outcomeMeasures-", saveDate, ".rds"))

reps

### NOTE:
### reps = vector giving the number of successful replications for each set of
###        crossed conditions
### out  = list of aggregated, raw output from the simulation
### res  = data.frame containing processed outcome measures. Each row
###        corresponds to one set of crossed conditions. 
