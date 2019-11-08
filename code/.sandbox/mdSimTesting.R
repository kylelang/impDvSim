### Title:    Experiments with Missing Data Simulation Methods
### Author:   Kyle M. Lang
### Created:  2019-11-06
### Modified: 2019-11-08

rm(list = ls(all = TRUE))

library(optimx)
library(mvtnorm)

source("../simMissingness.R")

n <- 10000
b <- c(1.0, 1.0, 1.0)

dat0 <- data.frame(x = runif(n),
                   y = runif(n),
                   z = runif(n)
                   )

#sigma       <- matrix(0.5, 3, 3)
#diag(sigma) <- 1.0

#dat0           <- data.frame(rmvnorm(n, sigma = sigma))
#colnames(dat0) <- c("x", "y", "z")

pm    <- 0.75
preds <- colnames(dat0)
type  <- "low"

r <- simMissingness(pm = pm, data = dat0, preds = preds, type = type)

mean(r)

out1 <- glm(r ~ as.matrix(dat0), family = binomial())
out0 <- glm(r ~ 1, family = binomial())

r2 <- 1 - (logLik(out1) / logLik(out0))
r2

par(mfrow = c(1, 3))
tmp <- lapply(dat0, function(x) print(boxplot(x ~ r)))

z2    <- dat0[ , "z"]
z2[r] <- NA

par(mfrow = c(1, 1))
plot(density(dat0$z))
lines(density(dat0$z[r]), col = "red")
lines(density(dat0$z[!r]), col = "blue")

mean(dat0$z)
mean(z2, na.rm = TRUE)

cor(dat0$x, dat0$z, use = "pairwise")
cor(dat0$x, z2, use = "pairwise")

mean(abs(dat0$z[r]))
mean(abs(dat0$z[!r]))
