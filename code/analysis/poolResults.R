### Title:    Pool Results of Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-11-16
### Modified: 2019-11-05

rm(list = ls(all = TRUE))

source("../supportFunctions.R")

plotDir  <- "../../plots/study2/"
outDir   <- "../../output/study2/"
saveDir  <- "../../results/study2/"
saveDate <- format(Sys.time(), "%Y%m%d")
nReps    <- 500

## Define levels of variable simulation parameters:
nVec  <- c(500, 250, 100)
pmVec <- c(0.1, 0.2, 0.4)
r2Vec <- c(0.15, 0.3, 0.6)
clVec <- c(0.0, 0.1, 0.3, 0.5)
apVec <- c(1.0, 0.75, 0.5, 0.25, 0.0)

condMat <- expand.grid(ap = apVec,
                       cl = clVec,
                       pm = pmVec,
                       n  = nVec,
                       r2 = r2Vec)

outList2 <- list()
reps     <- rep(0, nrow(condMat))
for(i in 1 : nrow(condMat)) {
    fnCore <- paste0("_n",  condMat[i, "n"],
                     "_rs", condMat[i, "r2"],
                     "_cl", condMat[i, "cl"],
                     "_ap", condMat[i, "ap"],
                     "_pm", condMat[i, "pm"])

    outList <- list()
    for(rp in 1 : nReps) {
        compFileName <- paste0(outDir,
                               "compOut", fnCore,
                               "_rep", rp,
                               ".rds")
        ldFileName   <- paste0(outDir,
                               "ldOut", fnCore,
                               "_rep", rp,
                               ".rds")
        miFileName   <- paste0(outDir,
                               "miOut", fnCore,
                               "_rep", rp,
                               ".rds")
        midFileName  <- paste0(outDir,
                               "midOut", fnCore,
                               "_rep", rp,
                               ".rds")

        test1 <-
            file.exists(compFileName) &
                file.exists(ldFileName) &
                    file.exists(miFileName) &
                        file.exists(midFileName)

        if(test1) {
            reps[i] <- reps[i] + 1
            outList$comp[[rp]] <- readRDS(compFileName)
            outList$ld[[rp]]   <- readRDS(ldFileName)
            outList$mi[[rp]]   <- readRDS(miFileName)
            outList$mid[[rp]]  <- readRDS(midFileName)
        }
    }# END for(rp in 1 : nReps)

    outList2[[i]] <- outList
}# END for(i in 1 : nrow(condMat)

saveRDS(reps, file = paste0(saveDir, "impDvSimRepCounts-", saveDate, ".rds"))
saveRDS(outList2, file = paste0(saveDir, "impDvSimOutList-", saveDate, ".rds"))

any(reps < 500)
