### Title:    Analysis Subroutines for Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-11-16
### Modified: 2020-06-24

###--------------------------------------------------------------------------###

## Compute the true values of the regression coefficients:
calcBetaTrue <- function(out) {
    betaTrue        <- colMeans(do.call(rbind, out$comp))
    names(betaTrue) <- c("int", "x", "z1")
    betaTrue
}

###--------------------------------------------------------------------------###

## Calculate deviance from true values:
calcDev <- function(out, betaTrue) {
    betaHat <- out[ , -grep("lb|ub|sig", colnames(out), ignore = TRUE)]
    dev     <- t(apply(betaHat, 1, function(x, y) x - y, y = betaTrue))
    
    colnames(dev) <- c("int", "x", "z1")
    dev
}

###--------------------------------------------------------------------------###

## Calculate CI widths:
calcCiw <- function(out) {
    upperNames <- colnames(out)[grep("ub", colnames(out), ignore = TRUE)]
    lowerNames <- colnames(out)[grep("lb", colnames(out), ignore = TRUE)]

    ciw <- out[ , upperNames] - out[ , lowerNames]

    colnames(ciw) <- c("int", "x", "z1")
    ciw
}

###--------------------------------------------------------------------------###

## Calculate CI coverage flags:
calcCic <- function(out, betaTrue) {
    upperNames <- colnames(out)[grep("ub", colnames(out), ignore = TRUE)]
    lowerNames <- colnames(out)[grep("lb", colnames(out), ignore = TRUE)]
    
    cic <- t(
        apply(X      = out,
              MARGIN = 1,
              FUN    = function(x, y, u, l) as.numeric(x[u] > y & x[l] < y),
              y      = betaTrue,
              u      = upperNames,
              l      = lowerNames)
    )
    
    colnames(cic) <- c("int", "x", "z1")
    cic
}

###--------------------------------------------------------------------------###

## Calculate NHST significance flags:
getSig <- function(out) {
    sig           <- out[ , grep("sig", colnames(out), ignore = TRUE)]
    colnames(sig) <- c("int", "x", "z1")
    sig
}

###--------------------------------------------------------------------------###

## Process the simulation output into modelible variables:
prepModelData <- function(out, type) {
    dv <- list()
    for(i in 1 : length(out)) {
        ## Extract results from one set of conditions:
        out2 <- out[[i]]
        
        ## Compute the true values of the regression coefficients:
        betaTrue <- calcBetaTrue(out2)
        
        ## Compute the outcome data for each method:
        tmp <- list()
        for(j in c("mi", "mid", "ld")) {
            out3     <- do.call(rbind, out2[[j]])
            tmp[[j]] <- data.frame(method = j,
                                   out2$conds,
                                   switch(type,
                                          dev = calcDev(out3, betaTrue),
                                          ciw = calcCiw(out3),
                                          cic = calcCic(out3, betaTrue),
                                          sig = getSig(out3)
                                          )
                                   )
        }
        ## Stack the outcome data for different methods:
        dv[[i]] <- do.call(rbind, tmp)
    }
    ## Stack the outcome data across conditions:
    do.call(rbind, dv)
}


                                        #prepDvs <- function(outList, what, betaTrue = NULL) {
                                        #    ## Extract complete data estimates:
                                        #    compOut <- do.call("rbind", outList$comp)
                                        #    
                                        #    ## Define true beta values:
                                        #    if(is.null(betaTrue)) betaTrue <- colMeans(compOut)
                                        #    
                                        #    ## Aggregate missing data estimates:
                                        #    missOut <- do.call("rbind", outList[[what]])
                                        #    
                                        #    ## Compute DV matrices and return them in a list:
                                        #    list(
                                        #        dev = calcDev(out = missOut, betaTrue = betaTrue),
                                        #        ciw = calcCiw(out = missOut),
                                        #        cic = calcCic(out = missOut, betaTrue = betaTrue),
                                        #        sig = getSig(out = missOut)
                                        #    )
                                        #}# END prepDvs()

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
}# END calcOutcomes()

###--------------------------------------------------------------------------###

## Estimate the mode of a continuous variable:
getMode <- function(x) {
    d <- density(x)
    d$x[which.max(d$y)]
}

###--------------------------------------------------------------------------###

plot0 <- function(data, outcome, param) {
    data$group <- apply(data, 1, function(x) paste0(x["method"], x["imp"]))
    
    p1 <- ggplot(data,
                 aes_string(x        = "ap",
                            y        = paste(outcome, param, sep = "."),
                            group    = "group",
                            color    = "imp",
                            linetype = "method")
                 )
    
    p1 + geom_line() +
        theme_classic() +
        scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
        scale_color_gradient(low = "blue", high = "red")
}

###--------------------------------------------------------------------------###
