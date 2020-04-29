### Title:    Use stan to conduct a Bayesian analysis of simulation results
### Author:   Kyle M. Lang
### Created:  2017-08-22
### Modified: 2019-11-05


### Set Up the Environment ###

rm(list = ls(all = TRUE))

source("../supportFunctions.R")

library(rstan)
library(coda)

## Variable parameters:
modName  <- "deviance_model.stan" # Name of stan syntax file
runType  <- "target"              # Token to identify saved output
nIters   <- 1000                  # Total Gibbs sampling iterations
nWarm    <- 500                   # Number of warm-up iterations
priorSd  <- 100.0                 # Informative priors' SDs

## Stable parameters:
dataDir  <- "../../results/study1/"
plotDir  <- "../../plots/study1/"
fileName <- "stackResList-study1-20170822.rds" # Name of data file
rngSeed  <- 235711                             # Seed number for the RNG
recompile <- TRUE                              # Recompile the model source?
nChains   <- 2                                 # Number of Monte Carlo chains

## NOTE: nChains should be > 1 & <= the number of available processors

dat1 <- readRDS(paste0(dataDir, fileName))
tag  <- paste0(runType, "_warm", nWarm, "_sam", nIters - nWarm)

## Read in (or compile) the stanmodel object:
if(!recompile & file.exists("stanMod.rds")) {
    stanMod <- readRDS("stanMod.rds")
} else {
    stanMod <- stan_model(modName)
    saveRDS(stanMod, "stanMod.rds")
}

## Data object to pass to rstan

dat2 <- dat1$xDevFrame
dat2 <- dat2[sample(c(1 : nrow(dat2)), 1000), ]

colnames(dat2)

yName <- "mi"
xNames <- colnames(dat2)[1 : 5]

stanData <- list(N = nrow(dat2),
                 P = length(xNames),
                 y = as.vector(dat2[ , yName]),
                 X = dat2[ , xNames]
                 )

if(runType != "default") stanData$pSd <- priorSd


### Estimate the Model ###

## Sample from the BSEM model using the compiled C++ code:
stanOut <- sampling(object  = stanMod,
                    data    = stanData,
                    pars    = NA,
                    chains  = nChains,
                    iter    = nIters,
                    warmup  = nWarm,
                    thin    = 1,
                    cores   = nChains,
                    seed    = rngSeed,
                    control = list(max_treedepth = 20)
                    )

tmp <- summary(stanOut)$summary

range(tmp[ , "Rhat"])

names(tmp)

betaSams <- extract(stanOut, "beta")[[1]]
apply(betaSams, 2, getMode)

## Save the fitted model object:
saveRDS(stanOut, file = paste0(outDir, "stanOut-", tag, ".rds"))

## Convert chains to MCMC object:
postSams <- As.mcmc.list(stanOut, pars = c("chlM"), include = FALSE)

### Check Convergence ###

## Create traceplots:
pdf(paste0(plotDir, "tracePlots-", tag, ".pdf"), onefile = TRUE)
par(mfrow = c(4, 4), cex = 0.5)
coda::traceplot(postSams)
dev.off()

## Compute (split-chain) R-Hats:
rHats <- summary(stanOut)$summary[ , "Rhat"]
max(rHats, na.rm = TRUE) # should be less than 1.1
