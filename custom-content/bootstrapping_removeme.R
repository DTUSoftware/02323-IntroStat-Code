# ---------------------
#   From Hansen
# ---------------------

# 4) Bootstraping ----

###Paramatic 1-sample (kender fordelingen) ----

##exponentiel fordeling
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
       38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
rate <- 1/26.08
simsamples <- replicate(k, rexp(n, rate))
#indsæt værdi der ønskes:
sim <- apply(simsamples, 2, mean)
quantile(sim, c(0.025, 0.975))

##Normal fordeling
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
       38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
mean <- mean(x)
SD <- sd(x)
simsamples <- replicate(k, rnorm(n, mean, SD))
#indsæt værdi der ønskes:
sim <- apply(simsamples, 2, q1)
quantile(sim, c(0.025, 0.975))

##Log-normal fordeling
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
       38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
mean <- mean(log(x))
SD <- sd(log(x))
simsamples <- replicate(k, rlnorm(n, mean, SD))
#indsæt værdi der ønskes:
sim <- apply(simsamples, 2, mean)
quantile(sim, c(0.025, 0.975))

###Paramatic 2-sample ----

##Exponential fordeling
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(1, 2, 1, 3, 2,1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2,3, 2, 4, 3, 2)
n1 <- length(x)
n2 < length(y)
k <- 10000
rate1 <-
  rate2 <-
    simsamplesx <- replicate(k, rexp(n, rate1))
simsamplesy <- replicate(k, rexp(n, rate2))
#indsæt værdi der ønskes:
sim <- apply(simsamplesx, 2, mean) - apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))

##Normal fordeling
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(1, 2, 1, 3, 2,1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2,3, 2, 4, 3, 2)
n1 <- length(x)
n2 < length(y)
k <- 10000
simsamplesx <- replicate(k, rnorm(n, mean(x), sd(x)))
simsamplesy <- replicate(k, rnorm(n, mean(y), sd(y)))
#indsæt værdi der ønskes:
sim <- apply(simsamplesx, 2, mean) - apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))

##Log-normal fordeling
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(1, 2, 1, 3, 2,1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2,3, 2, 4, 3, 2)
n1 <- length(x)
n2 < length(y)
k <- 10000
simsamplesx <- replicate(k, rnorm(n, mean(log(x)), sd(log(x))))
simsamplesy <- replicate(k, rnorm(n, mean(log(y)), sd(log(y))))
#indsæt værdi der ønskes:
sim <- apply(simsamplesx, 2, mean) - apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))

###Non-Paramatic 1-sample (kender ikke fordelingen) -----
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
       38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
simsamples <- replicate(k, sample(x, replace=TRUE))
#indsæt værdi der ønskes(mean, median, etc):
sim <- apply(simsamples, 2, q1)
quantile(sim, c(0.025, 0.975))

###Non-Paramatic 2-sample (kender ikke fordelingen) -----
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(1, 2, 1, 3, 2,1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2,3, 2, 4, 3, 2)
k <- 10000
simsamplesx <- replicate(k, sample(x, replace=TRUE))
simsamplesy <- replicate(k, sample(y, replace=TRUE))
#indsæt værdi der ønskes(mean, median, etc):
sim <- apply(simsamplesx, 2, mean)- apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))