
dat0 <- data.frame(rmvnorm(500, rep(0, 10), diag(10)))

fun1.1 <- function(x) {
    x <- x[1 : 500, ]
    cov(x)
}

fun1.2 <- function(x) {
    y <- x[1 : 500, ]
    cov(y)
}

fun2.1 <- function(x) {
    x <- x
    cov(x)
}

fun2.2 <- function(x) {
    y <- x
    cov(y)
}

fun3.1 <- function(x) cov(x[1 : 500, ])

fun3.2 <- function(x) cov(x)

nRep <- 5000
t1.1 <- system.time(replicate(n = nRep, fun1.1(dat0)))
t1.2 <- system.time(replicate(n = nRep, fun1.2(dat0)))
t2.1 <- system.time(replicate(n = nRep, fun2.1(dat0)))
t2.2 <- system.time(replicate(n = nRep, fun2.2(dat0)))
t3.1 <- system.time(replicate(n = nRep, fun3.1(dat0)))
t3.2 <- system.time(replicate(n = nRep, fun3.2(dat0)))

t1.1
t1.2
t2.1
t2.2
t3.1
t3.2
