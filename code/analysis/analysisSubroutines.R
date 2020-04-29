### Title:    Analysis Subroutines for Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-11-16
### Modified: 2020-04-29

###--------------------------------------------------------------------------###

## Compute simulation outcome measures:
calcOutcomes <- function(outList, what, betaTrue = NULL) {
    ## Extract complete data estimates:
    compOut <- do.call("rbind", outList$comp)
    
    ## Define true beta values:
    if(is.null(betaTrue)) betaTrue <- colMeans(compOut)
    
    ## Aggregate missing data estimates:
    missOut <- do.call("rbind", outList[[what]])
    betaHat <- missOut[ , -grep("lb|ub|sig", colnames(missOut), ignore = TRUE)]
    means   <- colMeans(missOut)

    ## Compute CI-related outcome measures:
    upperNames <-
        colnames(missOut)[grep("ub", colnames(missOut), ignore = TRUE)]
    lowerNames <-
        colnames(missOut)[grep("lb", colnames(missOut), ignore = TRUE)]
    
    ciw <-colMeans(missOut[ , upperNames] - missOut[ , lowerNames])
    
    tmp <- apply(X      = missOut,
                 MARGIN = 1,
                 FUN    = function(x, y, u, l) x[u] > y & x[l] < y,
                 y      = betaTrue,
                 u      = upperNames,
                 l      = lowerNames)
    
    cic <- rowMeans(tmp)

    ## Compute bias- and variance-related outcome measures:
    betaBar <- means[-grep("lb|ub|sig", names(means), ignore = TRUE)]
    bias    <- betaBar - betaTrue
    prb     <- 100 * bias / betaTrue
    mcsd    <- apply(betaHat, 2, sd)
    sb      <- bias / mcsd
    
    ## Compute NHST rejection rates:
    sig <- means[grep("sig", names(means), ignore = TRUE)]

    ## Clean up some names:
    names(cic) <- names(ciw) <- names(sig) <- names(prb)
    
    list(prb  = prb,  # Percentage relative bias
         sb   = sb,   # Standardized bias
         mcsd = mcsd, # Monte Carlo standard deviation
         sig  = sig,  # NHST rejection rates
         cic  = cic,  # Confidence interval covarage
         ciw  = ciw)  # Confidence interval width
}# END calcOutcome()

###--------------------------------------------------------------------------###

## Estimate the mode of a continuous variable:
getMode <- function(x) {
    d <- density(x)
    d$x[which.max(d$y)]
}

###--------------------------------------------------------------------------###
