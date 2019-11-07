### Title:    Experiments with Missing Data Simulation Methods
### Author:   Kyle M. Lang
### Created:  2019-11-06
### Modified: 2019-11-07


### ToDo:
###   Figure out why the 'center' type is producing too much missingness

fAlpha <- function(alpha, eta, pm, filter = rep(TRUE, length(eta))) {
    p <- plogis(alpha + eta)
    r <- as.logical(rbinom(n = length(p), size = 1, prob = p))
    
    (mean(r & filter) - pm)^2
}


                                        #(mean(plogis(alpha + eta)) - pm)^2

optAlpha <- function(pm, eta, tol = c(0.1, 0.001), maxIter = 10, filter = rep(TRUE, length(eta))) {
    for(k in 1 : maxIter) {
        ## Define the search range:
        int <- k * range(eta)

        ## Optimize the objective over 'int':
        out <- optimize(f        = fAlpha,
                        interval = int,
                        eta      = eta,
                        pm       = pm,
                        filter   = filter)
        
        ## Are we far enough from the boundary?
        dist   <- out$minimum - int
        check1 <- all(abs(dist) > tol[1] * diff(int))
        
        ## Are we within tolerance?
        check2 <- out$objective < tol[2]
        
        if(check1 & check2) {
                                        #out$converged <- TRUE
            break
        }
    }
    ## Did we fail?
    if(!check1 | ! check2) {
                                        #out$converged <- FALSE
        stop("I could not optimize this function.")
    }
    
    out
}

rVec <- function(pm, eta, alpha = NULL, filter = rep(TRUE, length(eta))) {
    if(is.null(alpha))
        alpha <- optAlpha(pm = pm, eta = eta, filter = filter)$minimum
    
    probs <- plogis(alpha + eta)

    list(rVec  = as.logical(rbinom(n = length(eta), size = 1, prob = probs)),
         alpha = alpha)
}

   
### NOTE:
###   1. Positive beta => right tail missingness
###   2. Negative beta => left tail missingness
###   3. Mix (1) & (2) to get symmetric tail missingness
###   4. Negate (3) to get center missing
###   5. We can also multiply (1) by -1 to get (2)

data  <- dat0
preds <- colnames(dat0)
beta  <- rep(1.0, length(preds))
pm    <- 0.5
type  <- "tails"

formula <- switch(type[i],
                  LEFT = function(x, b) logit(mean(x) - x + b),
                  MID = function(x, b) logit(-abs(x - mean(x)) + 0.75 + b),
                  TAIL = function(x, b) logit(abs(x - mean(x)) - 0.75 + b),
                  function(x, b) logit(-mean(x) + x + b))

x <- rnorm(n, 10, 5)

b <- 0.5

y <- x - mean(x) + b
y <- mean(x) - x + b

y <- -abs(x - mean(x)) + b
y <- abs(x)

plot(x = x, y = plogis(y))

?rbeta

n <- 10000
a <- 0.9
b <- 0.9

x <- seq(0, 1, length.out = n)
y <- dbeta(x, a, b)

plot(x = x, y = y, type = "l")

simMissingness <- function(pm,
                           data,
                           preds = colnames(data),
                           type  = "high",
                           beta  = NULL)
{
    if(is.null(beta)) beta <- rep(1.0, length(preds))
    
    eta <- as.matrix(data[ , preds]) %*% matrix(beta)

    eta <- rnorm(n)
    plot(x = eta, y = plogis(eta))

    eta <- abs(eta - mean(eta))
    plot(x = eta, y = plogis(eta))

    eta <- eta[eta > 0]
    plot(x = eta, y = plogis(eta))

    plot(sort(eta))
    mean(eta)

    mean(r)
    
    tmp <- rVec(pm = pm, eta = abs(eta - mean(eta)))
    
    if(type == "high" | type == "low") {
        if(type == "low") eta <- -eta
        r <- rVec(pm = pm, eta = eta)$rVec
    }
    else if(type == "center" | type == "tails") {
        pm <- ifelse(type == "center", (1 - pm) / 2, pm / 2)
        pm
        tmp <- rVec(pm = pm, eta = eta)
        mean(tmp$rVec)
        r1  <- tmp$rVec
        r2  <- rVec(pm = pm, eta = -eta, filter = !r1)$rVec
        mean(r1)
        mean(r2)
        
        1 - mean(r1 | r2) 

        mean(r)
        mean(r1 & r2)
       
        r <- r1 | r2
        if(type == "center") r <- !r
    }
    else stop("Please specify a valid 'type'")
    
    r
}

n <- 100000
b <- c(1.0, 1.0, 1.0)

dat0 <- data.frame(x = rnorm(n),
                   y = rnorm(n),
                   z = rnorm(n)
                   )

data  <- dat0
preds <- colnames(dat0)
beta  <- rep(1.0, length(preds))
pm    <- 0.25

type  <- "center"

r <- simMissingness(pm = pm, data = dat0, preds = preds, type = type, beta = beta)

mean(r)

par(mfrow = c(1, 3))
tmp <- lapply(dat0, function(x) print(boxplot(x ~ r)))

boxplot(eta ~ r)

pm / 2

2 * (1 - pm) / 2
2 * (1 - (pm / 2))
