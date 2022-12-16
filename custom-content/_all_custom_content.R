# -----------------------------------------------------------------------------
#   ANOVA
# -----------------------------------------------------------------------------

y <- c(1.89, 2.35, 1.68, 2.11, 3.15, 2.16, 2.40, 2.59, 1.54, 2.02, 2.01, 2.11)
grp <- c(rep("1", 4), rep("2", 4), rep("3", 4))
fit <- lm(y ~ grp)
anova(fit)
# if you want SST just sum the sq

## Fra Hansen ---------------

### 8) Anova ----
# Pairwise CI####
yi <- 4.0483
yj <- 5.5517
alpha <- 0.05
sse <- 4.1060
n <- 18
k <- 3
ni <- 6
nj <- 6
df <- n - k
yi - yj + c(-1, 1) *
  qt(1 - alpha / 2, df) *
  sqrt(SSE / (n - k) * (1 / ni + 1 / nj))

# F-størrelsen (test størrelsen)#####
F = (SSTr / (k - 1)) / (SSE / (n - k)); F
#OR
MSTr <- SSTr / (k - 1)
F = MSTr / MSE; F

# SST - Totalafvigelsessum ####
SST <- SSE + SSTr; SST

# SSE ####
SSE <- MSE * (n - k)
# MSTr  #####
MSTr <- SSTr / (k - 1)
# Kritisk værdi####
#1-way ANOVA critical value
alpha <- 0.05
k <- 7
n <- 7 * 5
qf(1 - alpha, k - 1, n - k)
# F - Teststørrelse#####
F <- (SSTr / (df1) / (SSE / (df2))); F
# P-værdi#####
pv <- 1 - pf(F, df1 <- df1, df2 <- df2); pv
### 2-way ANOVA -----
df1 <- 4
df2 <- 7
df3 <- 28
mstr <- 1.82
msbl <- 11.07
mse <- 3.18
ftr <- mstr / mse
fbl <- msbl / mse
pvtr <- 1 - pf(ftr, df1, df3); pvtr
pvbl <- 1 - pf(fbl, df2, df3); pvbl
# -----------------------------------------------------------------------------
#   BOOTSTRAPPING (WTF)
# -----------------------------------------------------------------------------

# From Hansen ---------------------

# 4) Bootstraping ----

###Paramatic 1-sample (kender fordelingen) ----

##exponentiel fordeling
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
       38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
rate <- 1 / 26.08
simsamples <- replicate(k, rexp(n, rate))
#indsæt værdi der ønskes:
sim <- apply(simsamples, 2, mean)
quantile(sim, c(0.025, 0.975))

##Normal fordeling
q1 <- function(x) { quantile(x, 0.25) }
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
q1 <- function(x) { quantile(x, 0.25) }
set.seed(6287)
x <- c(1, 2, 1, 3, 2, 1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2, 3, 2, 4, 3, 2)
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
q1 <- function(x) { quantile(x, 0.25) }
set.seed(6287)
x <- c(1, 2, 1, 3, 2, 1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2, 3, 2, 4, 3, 2)
n1 <- length(x)
n2 < length(y)
k <- 10000
simsamplesx <- replicate(k, rnorm(n, mean(x), sd(x)))
simsamplesy <- replicate(k, rnorm(n, mean(y), sd(y)))
#indsæt værdi der ønskes:
sim <- apply(simsamplesx, 2, mean) - apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))

##Log-normal fordeling
q1 <- function(x) { quantile(x, 0.25) }
set.seed(6287)
x <- c(1, 2, 1, 3, 2, 1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2, 3, 2, 4, 3, 2)
n1 <- length(x)
n2 < length(y)
k <- 10000
simsamplesx <- replicate(k, rnorm(n, mean(log(x)), sd(log(x))))
simsamplesy <- replicate(k, rnorm(n, mean(log(y)), sd(log(y))))
#indsæt værdi der ønskes:
sim <- apply(simsamplesx, 2, mean) - apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))

###Non-Paramatic 1-sample (kender ikke fordelingen) -----
q1 <- function(x) { quantile(x, 0.25) }
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
       38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
simsamples <- replicate(k, sample(x, replace = TRUE))
#indsæt værdi der ønskes(mean, median, etc):
sim <- apply(simsamples, 2, q1)
quantile(sim, c(0.025, 0.975))

###Non-Paramatic 2-sample (kender ikke fordelingen) -----
q1 <- function(x) { quantile(x, 0.25) }
set.seed(6287)
x <- c(1, 2, 1, 3, 2, 1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2, 3, 2, 4, 3, 2)
k <- 10000
simsamplesx <- replicate(k, sample(x, replace = TRUE))
simsamplesy <- replicate(k, sample(y, replace = TRUE))
#indsæt værdi der ønskes(mean, median, etc):
sim <- apply(simsamplesx, 2, mean) - apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))
# -----------------------------------------------------------------------------
#   CONFIDENCE INTERVALS
# -----------------------------------------------------------------------------

data <- c(22.3, 11.8, 14.8, 13.4, 14.8, 15.9, 6.6, 9.6, 16.5)
conf_interval <- 99.9
alpha <- conf_interval / 100
t.test(data, conf.level = alpha)

# To do it from the standard deviation, sample mean and sample size
# n is sample size
n <- 56
# x is sample mean
x <- 21.5
# sample standard deviation is
s <- 9.8
# the confidence interval thingy
conf_interval <- 95
alpha <- (100 - conf_interval) / 100

# Calculate
x + c(-1, 1) * qt(1 - alpha / 2, n - 1) * s / sqrt(n)


# 1. Insert Variables

n <- 0                         # Number of Observations
x <- 0                         # Sample Mean
s <- 0                         # Standard Deviation

# 2. Insert Variables

alpha <- 0.05                   # Significane Value
t <- qt(1 - alpha / 2, n - 1)     # T-value

# 3. Calculate Interval

x - t * s / sqrt(n)                # Lower value
x + t * s / sqrt(n)                # Higher value

# ---- From Hansen ---------------------

# 3) Konfidens Interval ----
### 1-sample mean ----
t.test(x, mu = 180, conf.level = 0.99)

x_bar <- 39
alpha <- 0.05
sd <- 2.6458
n <- 7
x_bar + c(-1, 1) * qt(1 - alpha / 2, n - 1) * sd / sqrt(n)
### 1-sample variance & sd ----
## Variance
x_bar <- 39
alpha <- 0.05
s <- 2.6458
n <- 7
((n - 1) * s^2) / qchisq(1 - alpha / 2, n - 1)
((n - 1) * s^2) / qchisq(alpha / 2, n - 1)
##Standard Deviation
sqrt(((n - 1) * s^2) / qchisq(1 - alpha / 2, n - 1))
sqrt(((n - 1) * s^2) / qchisq(alpha / 2, n - 1))

### 2-sample mean ----
x_bar1 <- 122.4
x_bar2 <- 145.9
Sp1 <- 30.5
Sp2 <- 22.3
n_1 <- 15
n_2 <- 10
alpha <- 0.05

#Finder antal frihedsgrader
v <- ((((Sp1^2) / n_1) + ((Sp2^2) / n_2))^2) / (((((Sp1^2) / n_1)^2) / (n_1 - 1)) + (((Sp2^2) / n_2)^2) / (n_2 - 1))
v
#Finder KI:
(x_bar1 - x_bar2) + c(-1, 1) *
  qt(1 - (alpha / 2), v) *
  sqrt(((Sp1^2) / n_1) + ((Sp2^2) / n_2))


### 2-sample variance & sd ----
# -----------------------------------------------------------------------------
#   Binomial Distribution
# -----------------------------------------------------------------------------

# How many "hits" you want / the value you want to hit
x <- 4
# size is how many times you try / draw
size <- 10
# the probability of getting a hit (percent - 3% -> 0.03)
prob <- 3 / 100

# remember that binomal doesn't "remove" options, and if you want the result to be above something use the 1-
# The dbinom function is used to compute the probability mass function (PMF) of the binomial distribution,
# which gives the probability that a random variable will take on a certain value.
# In general, you should use the dbinom function if you want to compute the probability of a specific
# number of successes in a certain number of trials
dbinom(x, size = size, prob = prob)
1 - dbinom(x, size = size, prob = prob)

# The pbinom function is used to compute the cumulative distribution function (CDF) of the binomial
# distribution, which gives the probability that a random variable will take on a value less than or equal
# to a certain value.
# the pbinom function if you want to compute the probability of having a certain number of successes or
# fewer in a certain number of trials,
pbinom(x, size = size, prob = prob)
1 - pbinom(x, size = size, prob = prob)

# The qbinom function is used to compute the quantile function of the binomial distribution,
# which gives the value that a random variable will take on with a certain probability.
# the qbinom function if you want to find the number of successes that will occur with a certain probability
# in a certain number of trials.
qbinom(x, size = size, prob = prob)
1 - qbinom(x, size = size, prob = prob)


# --- PBINOM GUIDE -----

# Pbinom return probability of a given number of successes with an amount of attempts.
# at something. Lets say the probability of rolling five 6's in 10 tries on a die

# successes = 1, Attempts = 100, succes_prob = 1/6 = 0.9999% of happening

successes <- 1
attempts <- 2
succes_prob <- 1 / 2

# Probability of AT MOST having this amount
pbinom(successes, attempts, succes_prob)

# Probability of having LESS successes
1 - pbinom(successes, attempts, succes_prob)

# Chance of EXACTLY the amount of successes
dbinom(successes, attempts, succes_prob)


#   ROBOT OVERLORD

# Generate binomial random variables
binomial_data <- rbinom(n = 100, size = 10, prob = 0.5)

# Summarize the binomial data
summary(binomial_data)
# -----------------------------------------------------------------------------
#   CHISQUARE DISTRIBUTION
# -----------------------------------------------------------------------------

## 1D

#  Insert values
n <- 0
s <- 0
alpha <- 0.01

# Calculate values
df <- n - 1
alpha_1 <- alpha / 2
alpha_2 <- 1 - alpha_1

# Calculate Result
c(sqrt(df * s^2 / qchisq(alpha_1, df)), sqrt(df * s^2 / qchisq(alpha_2, df)))


## 2D

#  Insert values
n1 <- 3
n2 <- 2
alpha <- 0.01

# Calculate values
df2d <- (n1 - 1) * (n2 - 1)

# Calculate Result
qchisq(1 - alpha, df2d)
# -----------------------------------------------------------------------------
#   EXPONENTIAL DISTRIBUION
# -----------------------------------------------------------------------------

# The exponential distribution is a continuous probability distribution used to model the time it takes for an event to
# occur. It is often used to model the time between events in a Poisson process, which is a statistical process that
# describes the occurrence of events at a constant average rate. The exponential distribution has a number of useful
# properties. For example, it is memoryless, which means that the probability of an event occurring at time t, given
# that it has not occurred up to time t, is the same as the probability of it occurring at time 0. This property makes
# the exponential distribution useful for modeling the lifetime of a system or the time it takes for a customer to make
# a purchase, for example.

# remember to use 1 - ..... if you want the chance of above something.

# whatever you want
x <- 2
# The rate (percent - 3% -> 0.03) - if you have a time of 3 years (mean), you need the inverse of the mean = 1/3
rate <- 1 / 3

# dexp is the density function of the exponential distribution. It calculates the probability density of a given value,
# given the parameters of the distribution.
dexp(x, rate = rate)
1 - dexp(x, rate = rate)

# pexp is the cumulative distribution function (CDF) of the exponential distribution. It calculates the probability
# that a random variable from the exponential distribution is less than or equal to a given value.
pexp(x, rate = rate)
1 - pexp(x, rate = rate)

# qexp is the inverse cumulative distribution function (ICDF) of the exponential distribution. It calculates the value
# at which a given probability occurs in the exponential distribution.
qexp(x, rate = rate)
1 - qexp(x, rate = rate)

# rexp is a function that generates random numbers from the exponential distribution. It is often used in statistical
# software to simulate data from an exponential distribution or to perform Monte Carlo simulations.
rexp(x, rate = rate)
# -----------------------------------------------------------------------------
#   HYPERGEOMETRIC DISTRIBUTION
# -----------------------------------------------------------------------------

# The hypergeometric distribution is a discrete probability distribution used to model the probability of a certain
# number of successes in a sample drawn from a finite population without replacement. It is used in situations where
# the probability of success is dependent on the number of successes and failures that have already occurred.
# The hypergeometric distribution is often used in statistical hypothesis testing, for example, to test the
# significance of the difference between the observed number of successes in a sample and the expected number of
# successes based on the population proportions. It is also used in fields such as biology and engineering to analyze
# data from experiments and observations.

# remember to use 1 - ..... if you want the chance of above something.

# Hits that you want
x <- 3
# how many in the sample size that are hits
m <- 4
# size
s <- 10
# ammount of tries
k <- 5

# dhyper function can be used to calculate the probability of a certain number of successes in a sample from a
# hypergeometric distribution.
# Calculate the probability of 3 successes in a sample of 5 from a population of 10 with 4 successes
dhyper(x, m, s - m, k)
1 - dhyper(x, m, s - m, k)

# phyper function can be used to calculate the cumulative probability of a certain number of successes or fewer.
phyper(x, m, s - m, k)
1 - phyper(x, m, s - m, k)

# qhyper function can be used to calculate the number of successes at which a given cumulative probability occurs.
qhyper(x, m, s - m, k)
1 - qhyper(x, m, s - m, k)
# -----------------------------------------------------------------------------
#   NORMAL DISTRIBUTION
# -----------------------------------------------------------------------------

# Pnorm finds out what percentage of the values in the normal distribution that
# are higher than the quantile value you have given it.

# Fx. What percent of users on chess.com has a rating higher than 1000? then
# 1000 would be the quantile value you give and it would return and say 43% are
# rated higher than 1000.

x <- 0       # Mean
s <- 0       # Standard Deviation
q <- 0       # Quantile to check for

# What percent of the population has a higher value? (q < population)
pnorm(q, x, s)

# What percent of the population has a lower value? (q > population)
1 - pnorm(q, x, s)

# ROBOT OVERLORD

# Generate normal random variables
normal_data <- rnorm(n = 100, mean = 0, sd = 1)

# Summarize the normal data
summary(normal_data)
# -----------------------------------------------------------------------------
#   POISSON DISTRIBUTION
# -----------------------------------------------------------------------------

# In R, the dpois, ppois, qpois, and rpois functions are all related to the Poisson distribution.
#
# The Poisson distribution is a discrete probability distribution that expresses the probability of a given number of
# events occurring in a fixed interval of time or space, if these events occur with a known average rate and
# independently of the time since the last event. It is often used to model the number of times an event occurs within
# a time period, such as the number of calls received by a call center per minute or the number of customers arriving
# at a store per hour.

# In a Poisson distribution, the parameter lambda (λ) represents the average number of events that occur in a given
# interval of time or space. It is also known as the rate parameter or the expected value of the distribution.
lambda <- 7 / 2


x <- 0

# remember to use 1 - pois if you want something above a certain value, for example when you want the chance that two
# or more people is gonna visit a website then you set x to 1 because you want more than 1 person and then do the
# 1 - ppois

# dpois: This function calculates the probability density function (PDF) of the Poisson
# distribution at a given point. The PDF gives the probability of observing a value within
# a given range.
dpois(x, lambda = lambda)
1 - dpois(x, lambda = lambda)

# ppois: This function calculates the cumulative distribution function (CDF) of the Poisson
# distribution at a given point. The CDF gives the probability of observing a value less than
# or equal to a given point.
ppois(x, lambda = lambda)
1 - ppois(x, lambda = lambda)

# qpois: This function calculates the inverse CDF (quantile function) of the Poisson distribution
# at a given probability. It gives the value at which the CDF equals the given probability.
qpois(x, lambda = lambda)
1 - qpois(x, lambda = lambda)

# rpois: This function generates random samples from the Poisson distribution.
rpois(x, lambda = lambda)
# -----------------------------------------------------------------------------
#   ERROR PROPAGATION
# -----------------------------------------------------------------------------

## Fra Hansen

# 4) Error defining/Error propagation  ----
### Theoretical derivation ----
rm(list = ls())
# indsæt formel i{}:
f <- function(x, y) { indsætformel }
# indsæt formel:
fd <- expression(indsæt formel)
dx <- D(fd, 'x')
dy <- D(fd, 'y')
#Finder værdien af et datapunkt:
x <- 240.48
y <- 9.987
f(x, y)
#Finder usikkerheden:
usikker_x <- 0.03
usikker_y <- 0.002
#Indsæt dx og dy definitioner med tal i:
dx
dy
sqrt((dx indsæt)^2 * (usikker_x)^2 + (dyindsæt)^2*(usikker_y)^2)

### Simulation ----
set.seed(28973)
k <- 10000
#Ændre variable navne og tal
Vs <- rnorm(k, 9.987, sd <- 0.002)
Ts <- rnorm(k, 289.12, sd <- 0.02)
#Indsæt formel
Ps <- 8.31*Ts/Vs
sd(Ps)
# -----------------------------------------------------------------------------
#   HYPOTHESIS TESTING
# -----------------------------------------------------------------------------

# fra Nielsen

n1 <- 2000
x1 <- 1920
n2 <- 2024
x2 <- 1801
x <- c(x1, x2)
n <- c(n1, n2)
alpha <- 0.01

# proportion test
prop.test(x, n, conf.level = 1 - alpha, correct = FALSE)

t.test(x, n, conf.level = 1 - alpha)


#--------------- manual as fuck---------------------------

# 1. Insert Values

x = 21.5
mu = 23
s = 9.8
n = 56

# 2. Calculate T_obs value & df

t_obs = (x - mu) / (s / sqrt(n))
t_obs
df = n - 1

# 3. Calculate p

P <- 2 * (1 - pt(t_obs, df))
P

# if P > 0.05 the hypothes is rejected
# if P < 0.05 the hypothes is NOT rejected


# To calculate the p - value from a t-obs and sample size
t_obs <- 0.857
# n is the sample size
n <- 50
2 * (1 - pt(t_obs, df = n - 1))
# -----------------------------------------------------------------------------
#   LINEAR REGRESSION
# -----------------------------------------------------------------------------

# x value
x <- c(0, 0, 2, 2, 4, 4, 6, 6, 8, 8, 10, 10)
# y values
y <- c(114, 14, 870, 1141, 2087, 2212, 3353, 2633, 3970, 4299, 4950, 5207)

# Wished x value
x_wish <- 5
# Confidence (percent)
conf <- 95

fit <- lm(y ~ x)
fit

# gives the conf interval for the wished x value in the data frame
predict(fit, newdata = data.frame(x = x_wish), interval = "prediction", level = conf / 100)

# summary, can give you the p value for example, to look at the null hypothesis
summary(fit)


# FROM HANSEN -----------------------------

# 5) Linear regression ----
### Parameter confidence intervals ----
x <- c(0, 25, 50, 75, 100)
y <- c(14, 38, 54, 76, 95)
#Lave framwork
D <- data.frame(x = x, y = y)
fit <- lm(y ~ x, data = D)
summary(fit)
#læs summary og indsæt:
alpha <- 0.05
b1_mean <- 0.80000
b1_sd <- 0.02444
df <- 7
b1_mean + c(-1, 1) * qt(1 - alpha / 2, df) * b1_sd

### Confidence & Prediction interval ----
x <- c(0, 25, 50, 75, 100)
y <- c(14, 38, 54, 76, 95)
#Lave framwork
D <- data.frame(x = x, y = y)
fit <- lm(y ~ x, data = D)
summary(fit)
## finde det automatisk
## Indsæt data punkt:
predict(fit, newdata = data.frame(x = 80), interval = "confidence",
        level = 0.95)

#læs summary og indsæt:
alpha <- 0.05
b1_mean <- 0.80000
b0_mean <- 15.40000
sd <- 1.932
df <- 3
n <- length(x)
x_punkt <- 80
x_mean <- 50
sx <- 39.52847
sxx <- (n - 1) * sx^2
#Confidence interval for line
(b0_mean + b1_mean * x_punkt) + c(-1, 1) *
  qt(1 - alpha / 2, df) *
  sd *
  sqrt((1 / n) + (x_punkt - x_mean)^2 / sxx)
#Interval for new point prediction:
(b0_mean + b1_mean * x_punkt) + c(-1, 1) *
  qt(1 - alpha / 2, df) *
  sd *
  sqrt(1 + (1 / n) + (x_punkt - x_mean)^2 / sxx)
# -----------------------------------------------------------------------------
#   MODEL CONTROL
# -----------------------------------------------------------------------------

# FROM HANSEN

# 3) model control ----
### QQ-plot ----
qqnorm(x)
qqline(x)
par(mfrow <- c(3, 3))
for (i in 1:9) {
  xr <- rnorm(9)
  qqnorm(xr, main = "")
  qqline(xr)
}
### Histogram ----
x <- c(3003, 3005, 2997, 3006, 2999, 2998, 3007, 3005, 3001)
hist(x, freq = F, col <- 4)
xp <- seq(2996, 3008, 0.1)
lines(xp, dnorm(xp, mean(x), sd(x)), lwd <- 2)
### Fordelingsfunktion ----
plot(ecdf(x), verticals <- TRUE)
xp <- seq(0.9 * min(x), 1.1 * max(x), length.out <- 100)
lines(xp, pnorm(xp, mean(x), sd(x)))
# -----------------------------------------------------------------------------
#   MULTI LINEAR REGRESSION
# -----------------------------------------------------------------------------

# LOOK IN LINEAR REGRESSION DOCUMENT / TITLE, ADD ANOTHER ONE, OR LOOK IN BOOK, IDK

# FROM HANSEN ---------------------

# 6) Multi linear regression -----
### p value ----
tobs <- 13.42
df <- 25
2 * (1 - (pt(abs(tobs), df)))
### Summary på data + konfidence interval ----
D <- data.frame(
  x1 = c(0.58, 0.86, 0.29, 0.20, 0.56, 0.28, 0.08, 0.41, 0.22,
         0.35, 0.59, 0.22, 0.26, 0.12, 0.65, 0.70, 0.30, 0.70,
         0.39, 0.72, 0.45, 0.81, 0.04, 0.20, 0.95),
  x2 = c(0.71, 0.13, 0.79, 0.20, 0.56, 0.92, 0.01, 0.60, 0.70,
         0.73, 0.13, 0.96, 0.27, 0.21, 0.88, 0.30, 0.15, 0.09,
         0.17, 0.25, 0.30, 0.32, 0.82, 0.98, 0.00),
  y = c(1.45, 1.93, 0.81, 0.61, 1.55, 0.95, 0.45, 1.14, 0.74,
        0.98, 1.41, 0.81, 0.89, 0.68, 1.39, 1.53, 0.91, 1.49,
        1.38, 1.73, 1.11, 1.68, 0.66, 0.69, 1.98))
fit <- lm(y ~ x1 + x2, data = D)
summary(fit)
#Confidence interval
confint(fit)

### Residual analysis ----
#definer data som fit.
par(mfrow = c(1, 2))
qqnorm(fit$residuals, pch = 19)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch = 19,
     xlab = "Fitted.values", ylab = "Residuals")
plot(D$x1, fit$residuals, xlab = "x1", ylab = "Residuals")
plot(D$x2, fit$residuals, xlab = "x2", ylab = "Residuals")

### Plot confidence band and prediction band ----
x1new <- seq(0, 1, by = 0.01)
pred <- predict(fit, newdata = data.frame(x1 = x1new),
                interval = "prediction")
conf <- predict(fit, newdata = data.frame(x1 = x1new),
                interval = "confidence")
plot(x1new, pred[, "fit"], type = "l", ylim = c(0.1, 2.4),
     xlab = "x1", ylab = "Prediction")
lines(x1new, conf[, "lwr"], col = "green", lty = 2)
lines(x1new, conf[, "upr"], col = "green", lty = 2)
lines(x1new, pred[, "lwr"], col = "red", lty = 2)
lines(x1new, pred[, "upr"], col = "red", lty = 2)
legend("topleft", c("Prediction", "Confidence band", "Prediction band"),
       lty = c(1, 2, 2), col = c(1, 3, 2), cex = 0.7)

### Plot data ----
par(mfrow = c(1, 2))
plot(D$x1, D$y, xlab = "x1", ylab = "y")
plot(D$x2, D$y, xlab = "x1", ylab = "y")
# -----------------------------------------------------------------------------
#   POWER T-TEST
# -----------------------------------------------------------------------------

# FROM HANSEN ---------------------

# 3) Power / sample size ----
### 1-sample power/ sample size ----
# number of observasions
n <- 30
# true difference in means
delta <- 5
# variance
v <- 16
# stabdard devitation
sd <- sqrt(v)
# sig level
alpha <- 0.01
# power
power <- 0.98
# types
type <- c("two.sample", "one.sample", "paired")
# remove whatever you need to find for example if you need to find n remove n from below
power.t.test(n = n, delta = delta, sd = sd, sig.level = alpha, power = power,
             type = type[1])


#1. metode
alpha <- 0.05
mean <- 178
sd <- 12.21
#me er afvigelsen i en retning, altså halvdelen af bredden
me <- 3
n <- ((qnorm(1 - alpha / 2) * sd) / me)^2
n

#2. metode power
sd <- 12.21
#tast ind forskellen der ønskes at findes eller angiv means.
meandiff <- 4
#beta er power. hvis det er 80% indtastes 0.2
beta <- 0.2
alpha <- 0.05
n <- (sd * (qnorm(1 - beta) + qnorm(1 - alpha / 2)) / (meandiff))^2
n

### 2-sample power/ sample size ----
# n er antal i hver gruppe.
#find power
power.t.test(n = 10, delta = 2, sd = 1, sig.level = 0.05)
#find sample size
power.t.test(power = 0.8, delta = 4, sd = 12.21, sig.level = 0.05)
#finde effekt / afvigelse (kaldet delta (forskel))
power.t.test(n = 50, power = 0.80, sd = 12.21, sig.level = 0.05)
# -----------------------------------------------------------------------------
#   PROPORTION TESTING
# -----------------------------------------------------------------------------

# FROM HANSEN ---------------------


# 7) proportion ----
#1. find ud af om det er 1-sample, eller to grupper/fordelinger der sammenlignes.
#Eller flere elementer der skal sammenlignes.
#Hvis det er 1-sample er man interesseret i en nulhypotese omkring en procentvis fordeling.
# er det 2-sample eller mange sample så tester man om fordelingerne er ens, ud fra en antagelse om at de er.
#Ved multi samle anvendes chi^2 test, ved 1-sample eller 2-sample kan prop.test anvendes.

### automatisk test ----
prop.test(x = c(6, 12), n = c(50, 50), correct <- FALSE)
prop.test(36, 200, correct = FALSE, conf.level = 0.99)
### 1- sample CI, Z, p-værdi for fordeling ----
x <- 44
n <- 100
#fordelingen der testes efter
p <- x / n
alpha <- 0.05
#Z - teststørrelse
z_obs <- (x - n * p) / sqrt(n * p * (1 - p))
#kritisk værdi:
qnorm(1 - alpha / 2)
#p-værdi
2 * pnorm(z_obs)
#CI
p + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)

#Alternativt
n <- 2333 + 2536
p <- 2333 / n
sigma_p <- sqrt(p * (1 - p) / n)
l_limit <- p - 1.96 * sigma_p
u_limit <- p + 1.96 * sigma_p

### 2x2 forskel CI, Z og p-værdi ----
x1 <- 44
n1 <- 100
x2 <- 56
n2 <- 100
p1 <- x1 / n1
p2 <- x2 / n2
p <- (x1 + x2) / (n1 + n2)
alpha <- 0.05
#Z - teststørrelse
Z <- (p1 - p2) / sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
Z
#kritisk værdi:
qnorm(1 - alpha / 2)
#p-værdi
2 * pnorm(Z)
#CI
(p1 - p2) + c(-1, 1) *
  qnorm(1 - alpha / 2) *
  sqrt((((x1 / n1) * (1 - (x1 / n1))) / n1) + ((x2 / n2) * (1 - (x2 / n2))) / n2)

### sample size ----
p <- 0.3
me <- 0.01
alpha <- 0.01

p * (1 - p) * (qnorm(1 - alpha / 2) / (me / 2))^2
#me er i en retning, så hvis der er angivet en brede i opgaven, så skal den skrives ind.
### multiple fordelinger /proportions ----
#teststørrelse bidrag
row_total <- 9 + 8
col_total <- 229
total <- 229
e <- (row_total * col_total) / total
o <- 9
(o - e)^2 / e
# -----------------------------------------------------------------------------
#   SIMULATIONS
# -----------------------------------------------------------------------------

# Number of values
k <- 1000000
x1 <- rnorm(k, 0, 1)
x2 <- rnorm(k, 0, 1)
sd(exp(x1) + x2^4 + x1 * x2)
# YOU CAN USE THE SAME THING WITH OTHER THINGS
# -----------------------------------------------------------------------------
#   SUMMARY STATISTICS
# -----------------------------------------------------------------------------

# Input the array
x <- c(22.3, 11.8, 14.8, 13.4, 14.8, 15.9, 6.6, 9.6, 16.5)

# Summary Table
Tbl <- apply(data.frame(x), 2, function(x) {
  c(
    n = sum(!is.na(x)),                                   ## Total number of observations (doesn't include missing values if there are any)
    mean = mean(x, na.rm = TRUE),                         ## Sample mean of daily heat consumption
    var = var(x, na.rm = TRUE),                           ## Sample variance of daily heat consumption
    sd = sd(x, na.rm = TRUE),                             ## Sample standard deviance
    lq = unname(quantile(x, probs = 0.25, na.rm = TRUE)), ## Lower quartile, Q1
    median = median(x, na.rm = TRUE),                     ## Median, Q2 (could also have used "quantile(x, probs=0.5, na.rm=TRUE)")
    hq = unname(quantile(x, probs = 0.75, na.rm = TRUE))  ## Upper quartile, Q3
  )
})
Tbl
# Summary function
summary(x)

# Manual functions
mean(x)
median(x)
var(x)
sd(x)


# estimate of standard error of the estimated proportion
# the ammount thatare hits
x <- 1801
# the sample size
n <- 2024
# the proportion
p <- x / n
# estimated variance
s2 <- p * (1 - p) / n
# estimated standard error
s <- sqrt(s2)
s

## From array to columns
y <- c(1.89, 2.35, 1.68, 2.11, 3.15, 2.16, 2.40, 2.59, 1.54, 2.02, 2.01, 2.11)
grp <- c(rep("a", 4), rep("b", 4), rep("c", 4))


# ## Read data into R
#
# # Read the dataset 'soenderborg2_data.csv' into R
# D <- read.table("soenderborg2_data.csv", sep = ";", header = TRUE)


## Processing of data

# Make 't' a date variable in R
D$t <- as.Date(D$t, format = "%d/%m/%Y")

# Choose data from 15 Oct 2009 to 15 Apr 2010 for the four houses
D_model <- subset(D, ("2009-10-15" <= t & t < "2010-04-16") &
  (houseId %in% c(3, 5, 10, 17)))

# Remove observations with missing values
D_model <- na.omit(D_model)


# STANDARD ERROR

s <- 0
n <- 0

SE <- s / sqrt(n)# -----------------------------------------------------------------------------
#   T-TEST
# -----------------------------------------------------------------------------

# FROM HANSEN

### 2-sample variance & sd ----
# 3) t-test typer ----
### Standard 1-sample test ----
t.test(x, mu = 3000, conf.level = 0.95)

#Test størrelse
mean <- 180.05
h0 <- 180
sd <- 0.0959
n <- 16
T <- (mean - h0) / (sd / sqrt(n))
T
df <- n - 1
#p-værdi:
2 * (1 - pt(abs(T), df))

### Kritisk værdi####
n <- length(x)
mu0 <- 180
alpha <- 0.01
df <- n - 1

c(-1, 1) * qt(1 - alpha / 2, df = df)

### Paired 2-sample test ----
### Welch 2-sample test ----
x1 <- c(1, 2, 1, 3, 2, 1, 2, 3, 1, 1)
x2 <- c(3, 4, 2, 4, 2, 3, 2, 4, 3, 2)
t.test(x1, x2)

mean1 <- 5.97
mean2 <- 8.25
h0 <- 0
sd1 <- sqrt(23)
sd2 <- sqrt(21)
n1 <- 51
n2 <- 68
T <- ((mean1 - mean2) - h0) / sqrt(sd1^2 / n1 + sd2^2 / n2)
T
df <- (sd1^2 / n1 + sd2^2 / n2)^2 / ((sd1^2 / n1)^2 / (n1 - 1) + (sd2^2 / n2)^2 / (n2 - 1))
#p-værdi:
2 * (1 - pt(abs(T), df))
# -----------------------------------------------------------------------------
#   MANUEL WELCH T-TEST
# -----------------------------------------------------------------------------

# sample mean
x1 <- 3449
# standard deviation
s1 <- 409.0
# sample size
n1 <- 50
# sample mean
x2 <- 3505.7
# standard deviation
s2 <- 467.9
# sample size
n2 <- 50

v <- (((s1^2) / n1 + (s2^2) / n2)^2) / ((((s1^2) / n1)^2) / (n1 - 1) + (((s2^2) / n2)^2) / (n2 - 1))
v

# variable (x er n)
s1 <- 467.9^2
s2 <- 409^2
x <- 50

# t står for tæller og n1 og n2 står for de to dele nævneren
t <- ((s1 / x) + (s2 / x))^2
n1 <- ((s1 / x)^2) / (x - 1)
n2 <- ((s2 / x)^2) / (x - 1)

t / (n1 + n2)
