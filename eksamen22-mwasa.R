# -----------------------------------------------------------------------------
#   Question 1
# -----------------------------------------------------------------------------

X_mean <- 2
Y_mean <- -1

# theorem 2.56
2*X_mean+Y_mean
# 3

# -----------------------------------------------------------------------------
#   Question 2
# -----------------------------------------------------------------------------

y <- c(59, 52, 42, 59, 24, 24, 40, 32, 63, 55, 34, 24)
x <- c(298, 303, 270, 287, 236, 245, 265, 233, 286, 290, 264, 239)

fit <- lm(y ~ x)

summary(fit)

# 3 - 0.8334


# -----------------------------------------------------------------------------
#   Question 3
# -----------------------------------------------------------------------------

y <- c(59, 52, 42, 59, 24, 24, 40, 32, 63, 55, 34, 24)
x <- c(298, 303, 270, 287, 236, 245, 265, 233, 286, 290, 264, 239)

fit <- lm(y ~ x)

qqnorm(fit$residuals)
qqline(fit$residuals)

# 2

# -----------------------------------------------------------------------------
#   Question 4
# -----------------------------------------------------------------------------

df <- 10
n <- df+2
s <- 0.0763
p <- 1
conf_interval <- 95
alpha <- (100 - conf_interval) / 100
1-alpha/2
t <- qt(1 - alpha / 2, n - (p+1))     # T-value
t

# 3

# -----------------------------------------------------------------------------
#   Question 5
# -----------------------------------------------------------------------------

# 4

# -----------------------------------------------------------------------------
#   Question 6
# -----------------------------------------------------------------------------

df <- 34
# To do it from the standard deviation, sample mean and sample size
# n is sample size
n <- df+1
# x is sample mean
x <- 6.461533
# sample standard deviation is
s <- 0.845 # wtf where
# the confidence interval thingy
conf_interval <- 99
alpha <- (100 - conf_interval) / 100

# Calculate
x + c(-1, 1) * qt(1 - alpha / 2, n - 1) * s / sqrt(n)

# 3 - 6.07 6.85

# -----------------------------------------------------------------------------
#   Question 7
# -----------------------------------------------------------------------------

# To do it from the standard deviation, sample mean and sample size
# n is sample size
n <- 34+1
# x is sample mean
x <- 1.857769
# variance
v <- 0.01643549
# sample standard deviation is
s <- sqrt(v)
t <- 45.117
# the confidence interval thingy
conf_interval <- 95
alpha <- (100 - conf_interval) / 100

# Calculate
x + c(-1, 1) * qt(1 - alpha / 2, n - 1) * s / sqrt(n)

x + t

# 4

# -----------------------------------------------------------------------------
#   Question 8
# -----------------------------------------------------------------------------

t.test(log(range1), log(range2), paired = TRUE)
#4?

# -----------------------------------------------------------------------------
#   Question 9
# -----------------------------------------------------------------------------

# method 3.63
ME <- 0.1
s <- 0.8
n <- ((1.96*s)/ME)^2
n
# 4

# -----------------------------------------------------------------------------
#   Question 10
# -----------------------------------------------------------------------------

SST <- 14.163 + 20.305

# 4

# -----------------------------------------------------------------------------
#   Question 11
# -----------------------------------------------------------------------------

# 4 - 0.02 < 0.05 (p)

# -----------------------------------------------------------------------------
#   Question 12
# -----------------------------------------------------------------------------

# binomial

# How many "hits" you want / the value you want to hit
x <- 2
# size is how many times you try / draw
size <- 6
# the probability of getting a hit (percent - 3% -> 0.03)
prob <- 35 / 100

# probability of two or more (at least), so probability of 1 or fewer, other way around
1 - pbinom(x-1, size = size, prob = prob)

# 5 - 0.681


# -----------------------------------------------------------------------------
#   Question 13
# -----------------------------------------------------------------------------

# simulation

# size is how many times you try / draw
size <- 6
# the probability of getting a hit (percent - 3% -> 0.03)
prob <- 35 / 100
# Number of values
k <- 1000000
x <- rbinom(n = k, size = size, prob = prob)
var(x)

# 4 - 1.37

# -----------------------------------------------------------------------------
#   Question 14
# -----------------------------------------------------------------------------

# poisson
bus_per_minute <- 1/15
minutes <- 20
# In a Poisson distribution, the parameter lambda (λ) represents the average number of events that occur in a given
# interval of time or space. It is also known as the rate parameter or the expected value of the distribution.
lambda <- bus_per_minute*minutes

x <- 1

# dpois: This function calculates the probability density function (PDF) of the Poisson
# distribution at a given point. The PDF gives the probability of observing a value within
# a given range.
dpois(x, lambda = lambda)

# 3 - 35.1%


# -----------------------------------------------------------------------------
#   Question 15
# -----------------------------------------------------------------------------

# # poisson
# bus_per_minute <- 1/15
# minutes <- 20
# # In a Poisson distribution, the parameter lambda (λ) represents the average number of events that occur in a given
# # interval of time or space. It is also known as the rate parameter or the expected value of the distribution.
# lambda <- 1/15*60
#
# x <- 90
#
# # ppois: This function calculates the cumulative distribution function (CDF) of the Poisson
# # distribution at a given point. The CDF gives the probability of observing a value less than
# # or equal to a given point.
# qpois(x, lambda = lambda)
# 1 - qpois(x, lambda = lambda)
#
# n <- 10
# # Simulate
# x <- rexp(n, rate = 1/15)
# # Plot the empirical pdf
# plot(table(x)/n)
# # Add the pdf to the plot
# lines(0:20, dpois(0:20,lambda), type="h", col="red")
#
#
# gen <- dnorm(1, 15)
# hist(gen)

# ------------------------

rate <- 1/15
x <- 0.9
# qexp is the inverse cumulative distribution function (ICDF) of the exponential distribution. It calculates the value
# at which a given probability occurs in the exponential distribution.
qexp(x, rate = rate)

# 3 - 34.53

# -----------------------------------------------------------------------------
#   Question 16
# -----------------------------------------------------------------------------

runif(50, 0, 100)
# 1

# -----------------------------------------------------------------------------
#   Question 17
# -----------------------------------------------------------------------------

df <- 10
q <- 0.371
2*(1 - pt(q=q, df=df))
# 4 - 0.7184

# -----------------------------------------------------------------------------
#   Question 18
# -----------------------------------------------------------------------------

df <- 10
p <- 2
n <- df + (p+1)
n
# 5 - 13

# -----------------------------------------------------------------------------
#   Question 19
# -----------------------------------------------------------------------------

# 1 - p < 0.5 (sign diff)

# -----------------------------------------------------------------------------
#   Question 20
# -----------------------------------------------------------------------------

simsamples <- replicate(10000, sample(glutenA, replace = FALSE)) # FALSE?
simmeans <- apply(simsamples, 2, mean)
quantile(simmeans, c(0.025, 0.975))
# 4

# -----------------------------------------------------------------------------
#   Question 21
# -----------------------------------------------------------------------------

simsamples <- replicate(10000, rnorm(10,mean(glutenA),sd(glutenA)))
simsds <- apply(simsamples, 2, sd)
quantile(simsds, c(0.025, 0.975))

# 2 (or 1 if mean)

# -----------------------------------------------------------------------------
#   Question 22
# -----------------------------------------------------------------------------

# 1

# -----------------------------------------------------------------------------
#   Question 23
# -----------------------------------------------------------------------------

# 5

# -----------------------------------------------------------------------------
#   Question 24
# -----------------------------------------------------------------------------

n <- 1268
p <- (852)/n
conf_interval <- 95
alpha <- (100 - conf_interval) / 100
p + c(-1, 1) * sqrt(p * (1 - p) / n) * qnorm(1 - alpha / 2)
# 4 -  0.6460817 0.6977669

# -----------------------------------------------------------------------------
#   Question 25
# -----------------------------------------------------------------------------

# p1 <- 746/1085
# p2 <- 339/1085
n1 <- 852
n2 <- 416
p1 <- 746/n1
p2 <- 339/n2
conf_interval <- 95
alpha <- (100 - conf_interval) / 100
p1-p2 + c(-1,1) * sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)*qnorm(1 - alpha / 2)

# 3 - 0.01727775 0.10408826

# -----------------------------------------------------------------------------
#   Question 26
# -----------------------------------------------------------------------------

total_allgender <- 1268
total_agree <- 1085
total_male <- 416

male_agree <- total_agree/total_allgender*total_male
male_agree

# 5

# -----------------------------------------------------------------------------
#   Question 27
# -----------------------------------------------------------------------------


#  Insert values
n1 <- 3
n2 <- 2
alpha <- 0.05

# Calculate values
df2d <- (n1 - 1) * (n2 - 1)

# Calculate Result
qchisq(1 - alpha, df2d)

# 2 - 5.991

# -----------------------------------------------------------------------------
#   Question 28
# -----------------------------------------------------------------------------

x <- c(-4.8, -0.1, -1.4, -3.3, 1.1, -2.2, -2.6, -2.8)
sd(x)
# 4 - 1.86

# -----------------------------------------------------------------------------
#   Question 29
# -----------------------------------------------------------------------------

x <- c(-4.8, -0.1, -1.4, -3.3, 1.1, -2.2, -2.6, -2.8)
hist(x)
# 3 - C

# -----------------------------------------------------------------------------
#   Question 30
# -----------------------------------------------------------------------------

# x <- c(-4.8, -0.1, -1.4, -3.3, 1.1, -2.2, -2.6, -2.8)

x <- 10       # Mean
s <- 5       # Standard Deviation
q <- 20       # Quantile to check for

# P( X <= x )
pnorm(10-1, x, s) # P(Y < 10)
1-pnorm(10-1, x, s) # P(Y > 10)
pnorm(10-1, x, s)-pnorm(0-1, x, s) # P(0 < Y < 10)
pnorm(20-1, x, s)-pnorm(0-1, x, s) # P(0 < Y < 20)
1-pnorm(20-1, x, s) # P(Y > 20)


pnorm(20, x, s)-pnorm(0, x, s) # P(0 < Y < 20)
# 4 - 0.954
