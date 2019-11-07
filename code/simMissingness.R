### Title:    Experiments with Missing Data Simulation Methods
### Author:   Kyle M. Lang
### Created:  2019-11-06
### Modified: 2019-11-07

library(optimx)

###--------------------------------------------------------------------------###

## Objective function for intercept to generate 'high' or 'low' missingness:
f1 <- function(alpha, eta, pm) (mean(plogis(alpha + eta)) - pm)^2

###--------------------------------------------------------------------------###

## Objective function for weights to generate 'center' or 'tail' missingness:
f2 <- function(par, eta, pm, type) {
    f <- switch(type,
                center = par[1] + par[2] - abs(eta),
                tails  = abs(eta) - par[1] + par[2]
                )
    
    (mean(plogis(f)) - pm)^2
}

###--------------------------------------------------------------------------###

## Optimize the logistic regression intercept (alpha) for a given value of the
## linear predictor (eta) to get a desired percent missing (pm):
optAlpha1 <- function(pm, eta, tol = c(0.1, 0.001), maxIter = 10) {
    for(k in 1 : maxIter) {
        ## Define the search range:
        int <- k * range(eta)
        
        ## Optimize the objective over 'int':
        out <- optimize(f        = fAlpha,
                        interval = int,
                        eta      = eta,
                        pm       = pm)
        
        ## Are we far enough from the boundary?
        dist   <- out$minimum - int
        check1 <- all(abs(dist) > tol[1] * diff(int))
        
        ## Are we within tolerance?
        check2 <- out$objective < tol[2]
        
        if(check1 & check2) break
    }
    ## Did we fail?
    if(!check1 | ! check2) stop("I could not optimize this function.")
    
    out
}

###--------------------------------------------------------------------------###

## Optimize the intercept (par1) and offset (par2) for a given value of the
## linear predictor (eta) to get a desired percent missing (pm):
optAlpha2 <- function(pm, eta, type) {
    ## Optimize the objective:
    out <- optimx(par     = c(0.5, 0.5),
                  fn     = f2,
                  method = "BFGS",
                  eta    = eta,
                  pm     = pm,
                  type   = type)

    ## Did the algorithm converge?
    check <- out$convcode == 0
    if(!check) stop("The optimization did not converge.")
    
    c(offset = out$p1, int = out$p2)
}

###--------------------------------------------------------------------------###

## Simulate a nonresponse vector:
simMissingness <- function(pm,
                           data,
                           preds = colnames(data),
                           type  = "high",
                           beta  = NULL)
{
    ## Define a trivial slope vector, if necessary:
    if(is.null(beta)) beta <- rep(1.0, length(preds))

    ## Compute (and standardize) the linear predictor:
    eta <- scale(
        as.matrix(data[ , preds]) %*% matrix(beta)
    )
    
    ## Optimize the intercept/offset and compute the probabilities of
    ## nonresponse:
    if(type %in% c("high", "low")) {
        alpha <- optAlpha1(pm = pm, eta = eta)$minimum        
        probs <- plogis(alpha + switch(type, high = eta, low = -eta))
    }
    else if(type %in% c("center", "tails")) {
        weights <- optAlpha2(pm = pm, eta = eta, type = type)
        probs   <- plogis(
            switch(type,
                   center = sum(weights) - abs(eta),
                   tails  = abs(eta) - weights[1] + weights[2]
                   )
        )
    }
    else
        stop(paste(type, "is not a valid 'type'."))
    
    ## Return a logical nonresponse vector:
    as.logical(rbinom(n = length(eta), size = 1, prob = probs))
}

###--------------------------------------------------------------------------###
