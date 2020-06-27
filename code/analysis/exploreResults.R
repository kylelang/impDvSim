### Title:    Explore Simulation Results
### Author:   Kyle M. Lang
### Created:  2020-06-24
### Modified: 2020-06-24


rm(list = ls(all = TRUE))

resDir <- "../../results/exp1/"
fn1    <- "outcomeMeasures-20200501.rds"

source("analysisSubroutines.R")

library(ggplot2)
library(ggpubr)

dat0 <- readRDS(paste0(resDir, fn1))

## Extract the design matrix:
conds <- dat0[1 : 6]

## Extract the results for each missing data method:
mi  <- data.frame(method = "mi", conds, dat0[grep("^mi\\.", colnames(dat0))])
mid <- data.frame(method = "mid", conds, dat0[grep("^mid\\.", colnames(dat0))])
ld  <- data.frame(method = "ld", conds, dat0[grep("^ld\\.", colnames(dat0))])

## Remove redundant rows from the LD results:
ld <- ld[ld$imp == 100, ]

## Normalize column names:
names0 <- gsub("mi.", "", colnames(mi), fixed = TRUE)
colnames(mi) <- colnames(mid) <- colnames(ld) <- names0

## Stack results:
dat1 <- rbind(mi, mid, ld)

## Compute relative CIWs:
rciw <- mi[grep("ciw", colnames(mi))] / mid[grep("ciw", colnames(mid))]
colnames(rciw) <- paste0("r", colnames(rciw))
rciw <- data.frame(conds, rciw)

## Compute relative absolute biases:
rab <- abs(mi[grep("prb", colnames(mi))]) / abs(mid[grep("prb", colnames(mid))])
colnames(rab) <- paste("rab", c("int", "x", "z1"), sep = ".")
rab <- data.frame(conds, rab)

condList <- lapply(dat1[1 : 7], unique)
condList

## Subset:
pm1 <- 0.4
n1  <- 500
par <- "int"
out <- "ciw"

filter <- with(dat1, pm == pm1 & n == n1)
dat2   <- dat1[filter, ]

p1 <- plot0(data = dat2, outcome = out, param = par) #+ geom_abline(intercept = 0, slope = 0)
                                        #p1 + facet_grid(r2 ~ cx, scales = "free")
p1 + facet_wrap(~ r2 + cx, scales = "free")

?facet_wrap

head(dat1)

group <- apply(dat2, 1, function(x) paste0(x["method"], x["r2"]))

p2 <- ggplot(data = dat2, mapping = aes(x = imp, y = ciw.x, group = group, linetype = method, color = r2))
p2 <- p2 + geom_line() + theme_classic() + scale_color_gradient(low = "blue", high = "red") + scale_linetype_manual(values = c("solid", "longdash", "dotted"))
p2 + facet_wrap(~cx + ap)

p2 + geom_line() + theme_classic() + scale_color_gradient(low = "blue", high = "red") + scale_linetype_manual(values = c("solid", "longdash", "dotted"))

condList

filter <- with(rciw, pm == 0.1 & n == 100)
dat2   <- rciw[filter, ]

dat2$ap  <- as.factor(dat2$ap)
dat2$imp <- as.factor(dat2$imp)

p3 <- ggplot(data = dat2, mapping = aes(x = imp, y = rciw.x, group = ap, color = ap))
p3 + geom_line() +
    geom_hline(yintercept = 1.0) +
    theme_classic() +
                                        #scale_color_gradient(low = "blue", high = "red") +
    facet_wrap(~r2 + cx, scales = "free")

filter <- with(rab, pm == 0.4 & n == 500)
dat2   <- rab[filter, ]

dat2$ap  <- as.factor(dat2$ap)
dat2$imp <- as.factor(dat2$imp)

p4 <- ggplot(data = dat2, mapping = aes(x = imp, y = rab.x, group = ap, color = ap))
p4 + geom_line() +
    geom_hline(yintercept = 1.0) +
    theme_classic() +
                                        #scale_color_gradient(low = "blue", high = "red") +
    facet_wrap(~r2 + cx, scales = "free")

filter <- with(rab, pm == 0.1 & n == 500 & imp == 100)
dat2   <- rab[filter, ]

                                        #dat2$ap  <- as.factor(dat2$ap)
                                        #dat2$imp <- as.factor(dat2$imp)

dat2$r2 <- as.factor(dat2$r2)

p4 <- ggplot(data = dat2, mapping = aes(x = ap, y = rab.x, group = r2, color = r2))
p4 + geom_line() +
    geom_hline(yintercept = 1.0) +
    theme_classic() +
                                        #scale_color_gradient(low = "blue", high = "red") +
    facet_wrap(~cx, scales = "free")


filter <- with(dat1, pm == 0.1 & n == 100 & imp == 5)
dat2   <- dat1[filter, ]

                                        #dat2$ap  <- as.factor(dat2$ap)
                                        #dat2$imp <- as.factor(dat2$imp)
                                        #dat2$r2 <- as.factor(dat2$r2)

p4 <- ggplot(data = dat2, mapping = aes(x = ap, y = prb.x, group = method, color = method))
p4 + geom_line() +
    geom_hline(yintercept = 0.0) +
    theme_classic() +
                                        #scale_color_gradient(low = "blue", high = "red") +
    facet_wrap(~r2 + cx, scales = "free")


+ scale_color_gradient(low = "blue", high = "red")

p4 <- ggplot(data = dat1, mapping = aes(x = ap, y = mid.ciw.int, group = imp, color = imp))
p4 + geom_line() + theme_classic() + scale_color_gradient(low = "blue", high = "red")
