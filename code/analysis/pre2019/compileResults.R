### Title:    Compile Results of Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-11-16
### Modified: 2019-11-05

rm(list = ls(all = TRUE))

source("../supportFunctions.R")

oneFile   <- FALSE
plotDir   <- "../../plots/study2/individual/"
dataDir   <- "../../results/study2/"
outDir    <- "../../results/study2/"
fileName1 <- "impDvSimRepCounts-20170821.rds"
fileName2 <- "impDvSimOutList-20170821.rds"
saveDate  <- format(Sys.time(), "%Y%m%d")
nReps     <- 500
beta      <- c(1.0, 0.33, 0.33)

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

## Read in the results:
reps <- readRDS(paste0(dataDir, fileName1))
any(reps < nReps)

resList <- readRDS(paste0(dataDir, fileName2))

prbFrame <- sbFrame <- mcsdFrame <-
    sigFrame <- cicFrame <- ciwFrame <-
        data.frame(condMat, matrix(NA, nrow(condMat), 9))

colnames(prbFrame) <- colnames(sbFrame) <- colnames(mcsdFrame) <-
    colnames(sigFrame) <- colnames(cicFrame) <- colnames(ciwFrame) <-
        c(colnames(condMat), "ldInt", "miInt", "midInt",
          "ldX1", "miX1", "midX1", "ldZ1", "miZ1", "midZ1")

## Create a hash-map to store stacked results:
map <- new.env()

## Compute outcome measures:
for(i in 1 : nrow(condMat)) {
    stackResults(outList = resList[[i]], conds = condMat[i, ])
    outList <- calcOutcomeMeasures(resList[[i]])

    prbFrame[i, 6 : 14]  <- as.vector(outList$prb)
    sbFrame[i, 6 : 14]   <- as.vector(outList$sb)
    mcsdFrame[i, 6 : 14] <- as.vector(outList$mcsd)
    sigFrame[i, 6 : 14]  <- as.vector(outList$sig)
    cicFrame[i, 6 : 14]  <- as.vector(outList$cic)
    ciwFrame[i, 6 : 14]  <- as.vector(outList$ciw)
}# END for(i in 1 : nrow(condMat)

colnames(map$iDevFrame) <- colnames(map$xDevFrame) <-
    colnames(map$zDevFrame) <- colnames(map$iSigFrame) <-
    colnames(map$xSigFrame) <- colnames(map$zSigFrame) <-
    colnames(map$iCiwFrame) <- colnames(map$xCiwFrame) <-
    colnames(map$zCiwFrame) <-  c(colnames(condMat), "mi", "mid", "lwd")

stackResList <- as.list.environment(map)

saveRDS(stackResList,
        paste0(outDir, "stackResList-study2-", saveDate, ".rds")
        )

par(mfrow = c(3, 3))
flag <- "g"
while(flag != "s") {
    for(j in c("i", "x", "z")) {
        tmp <- stackResList[[paste0(j, "CiwFrame")]]

        cNo <- sample(c(1 : 540), 1)
        n0  <- 500 * (cNo - 1)

        tmp <- tmp[(n0 + 1) : (n0 + 500), ]

        for(i in 6 : 8) {
            y <- tmp[ , i]
            x <- seq(min(y), max(y), 0.01 * diff(range(y)))
            plot(density(y))
            lines(x = x, y = dnorm(x, mean(y), sd(y)), col = "red")
        }
    }
    flag <- readline("Type 's' to stop. ")
}


##### PLOTTING #####

if(oneFile)
    pdf(paste0(plotDir, "prbPlots-", saveDate, ".pdf"),
        paper = "special",
        onefile = TRUE)

for(i in nVec) {
    for(j in r2Vec) {

        if(!oneFile)
            pdf(paste0(plotDir, "prbPlot_n", i, "_rSq", j*100, "-", saveDate,
                       ".pdf"),
                paper = "special")

        targetData <- prbFrame[with(prbFrame, n == i & r2 == j), ]

        if(oneFile) outerTop <- 5
        else        outerTop <- 0

        par(mfrow = c(3, 4),
            cex = 0.5,
            family = "serif",
            oma = c(4.5, 3.5, outerTop, 2),
            mar = c(0, 1, 2, 0))

        for(k in pmVec) {
            for(v in clVec) {
                plotData <- t(
                    as.matrix(
                        targetData[with(targetData, pm == k & cl == v),
                                   c("miX1", "midX1", "ldX1")]
                    )
                )
                barplot(plotData,
                        beside = TRUE,
                        ylim = c(-70, 15),
                        yaxt = "n",
                        xaxt = "n")
                lines(x = c(0, 100), y = c(-10, -10), lty = 2)
                lines(x = c(0, 100), y = c(10, 10), lty = 2)
                axis(2, at = seq(-35, 15, 5), tick = TRUE, label = FALSE)
                if(k == min(pmVec))
                    title(main = paste0("rXZ = ", v),
                          outer = FALSE,
                          line = 1)

                if(k == max(pmVec))
                    axis(side = 1,
                         at = c(2.5, 6.5, 10.5, 14.5, 18.5),
                         labels = apVec,
                         tick = FALSE)

                if(v == min(clVec)) {
                    axis(2)
                    axis(4,
                         at = -10,
                         tick = FALSE,
                         labels = paste0("PM = ", k),
                         outer = TRUE,
                         line = 0,
                         font = 2)
                }
            }
        }
        if(oneFile)
            title(main = paste0("N = ", i, " R2 = ", j),
                  outer = TRUE,
                  line = 3,
                  cex.main = 2,
                  font.main = 2)

        title(xlab = "Proportion MAR",
              font.lab = 2,
              outer = TRUE,
              line = 3,
              cex.lab = 1.5
              )

        title(ylab = "Percentage Relative Bias",
              font.lab = 2,
              outer = TRUE,
              line = 2,
              cex.lab = 1.5
              )
        if(!oneFile) dev.off()
    }
}
if(oneFile) dev.off()


if(oneFile)
    pdf(paste0(plotDir, "powerPlots-", saveDate, ".pdf"),
        paper = "special",
        onefile = TRUE)

for(i in nVec) {
    for(j in r2Vec) {

        if(!oneFile)
            pdf(paste0(plotDir, "powerPlot_n", i, "_rSq", j*100, "-", saveDate,
                       ".pdf"),
                paper = "special")

        targetData <- sigFrame[with(prbFrame, n == i & r2 == j), ]

        if(oneFile) outerTop <- 5
        else        outerTop <- 0

        par(mfrow = c(3, 4),
            cex = 0.5,
            family = "serif",
            oma = c(3.5, 3.5, outerTop, 2),
            mar = c(1, 1, 2, 0))

        for(k in pmVec) {
            for(v in clVec) {
                plotData <- t(
                    as.matrix(
                        targetData[with(targetData, pm == k & cl == v),
                                   c("miX1", "midX1", "ldX1")]
                    )
                )
                barplot(plotData,
                        beside = TRUE,
                        ylim = c(0, 1.0),
                        yaxt = "n",
                        xaxt = "n")
                lines(x = c(0, 100), y = c(0.8, 0.8), lty = 2)
                axis(2, at = seq(0, 1.0, 0.2), tick = TRUE, label = FALSE)
                if(k == min(pmVec))
                    title(main = paste0("rXZ = ", v),
                          outer = FALSE,
                          line = 1)

                if(k == max(pmVec))
                    axis(side = 1,
                         at = c(2.5, 6.5, 10.5, 14.5, 18.5),
                         labels = apVec,
                         tick = FALSE)

                if(v == min(clVec)) {
                    axis(2)
                    axis(4,
                         at = 0.5,
                         tick = FALSE,
                         labels = paste0("PM = ", k),
                         outer = TRUE,
                         line = 0,
                         font = 2)
                }
            }
        }
        if(oneFile)
            title(main = paste0("N = ", i, " R2 = ", j),
                  outer = TRUE,
                  line = 3,
                  cex.main = 2,
                  font.main = 2)

        title(xlab = "Proportion MAR",
              font.lab = 2,
              outer = TRUE,
              line = 2,
              cex.lab = 1.5
              )

        title(ylab = "Empirical Power",
              font.lab = 2,
              outer = TRUE,
              line = 2,
              cex.lab = 1.5
              )
        if(!oneFile) dev.off()
    }
}
if(oneFile) dev.off()

