#################################################################
### Testing hypotheses about one proportion using prop.test() ###
#################################################################

prop.test(10, 100, p = 0.5, correct = FALSE)

##################################################################
### Testing hypotheses about two proportions using prop.test() ###
##################################################################

# Read data table into R
pill.study <- matrix(c(23, 34, 35, 132), 
                     ncol = 2, byrow = TRUE)
colnames(pill.study) <- c("Blood Clot", "No Clot")
rownames(pill.study) <- c("Pill", "No pill")

# Show data table
pill.study

# Test whether probabilities are equal for the two groups
prop.test(pill.study, correct = FALSE)

###################################################################
### Testing hypotheses about two proportions using chisq.test() ###
###################################################################

# Test whether probabilities are equal for the two groups
chisq.test(pill.study, correct = FALSE)

# Expected values
chisq.test(pill.study, correct = FALSE)$expected

###################################################################
### Testing hypotheses in contingency tables using chisq.test() ###
###################################################################

# Read data table into R
poll <-matrix(c(79, 91, 93, 84, 66, 60, 37, 43, 47), 
              ncol = 3, byrow = TRUE)
colnames(poll) <- c("4 weeks", "2 weeks", "1 week")
rownames(poll) <- c("Cand1", "Cand2", "Undecided")

# Show data table
poll

# Show column percentages
prop.table(poll, 2)

# Plot probabilities
barplot(t(prop.table(poll, 2)), beside = TRUE, col = 2:4, las = 1, ylim = c(0, 0.5),
        ylab = "Percent", xlab = "Candidate", 
        main = "Distribution of votes")
legend(legend = colnames(poll), fill = 2:4, "topright")

# Testing for same distribution in the three populations
chisq.test(poll, correct = FALSE)

# Expected values
chisq.test(poll, correct = FALSE)$expected


########################################################
### Example: One-way and two-way ANOVA with B&O data ###
########################################################

# Get the B&O data from the lmerTest-package
library(lmerTest)
data(TVbo)
head(TVbo) # First rows of the data

# Define factor identifying the 12 TV set and picture combinations 
TVbo$TVPic <- factor(TVbo$TVset:TVbo$Picture)
 
# Each of 8 assessors scored each of the 12 combinations twice.
# Average the two replicates for each assessor and combination of 
# TV set and picture
library(doBy)
TVbonoise <- summaryBy(Noise ~ Assessor + TVPic, data = TVbo, 
                       keep.names = T)

# One-way ANOVA of the noise (not the correct analysis!)
anova(lm(Noise ~ TVPic, data = TVbonoise))

# Two-way ANOVA of the noise (better analysis, week 12)
anova(lm(Noise ~ Assessor + TVPic, data = TVbonoise))

##############################################
### Simple example: Plots of data by group ###
##############################################

# Input data
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)

## Define treatment groups
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))

## Plot data by treatment groups
par(mfrow = c(1,2))
plot(y ~ as.numeric(treatm), xlab = "Treatment", ylab = "y")
boxplot(y ~ treatm, xlab = "Treatment", ylab = "y")

###############################################
###  Plot F-distribution and critical value ###
###############################################

# Remember, this is "under H0" (i.e. we compute as if H0 is true)

# Number of groups
k <- 3

# Total number of observations
n <- 12

# Sequence for plot
xseq <- seq(0, 10, by = 0.1)

# Plot density of the F-distribution
plot(xseq, df(xseq, df1 = k-1, df2 = n-k), type = "l")

# Plot critical value for significance level 5%
cr <- qf(0.95, df1 = k-1, df2 = n-k)
abline(v = cr, col = "red") 

############################################
### One-way ANOVA using anova() and lm() ###
############################################

anova(lm(y ~ treatm))

###############################
### One-way ANOVA 'by hand' ###
###############################

k <- 3; n <- 12  # Number of groups k, total number of observations n

# Total variation, SST
(SST <- sum( (y - mean(y))^2 ))

# Residual variance after model fit, SSE
y1 <- y[1:4]; y2 <- y[5:8]; y3 <- y[9:12]

(SSE <- sum( (y1 - mean(y1))^2 ) + 
        sum( (y2 - mean(y2))^2 ) + 
        sum( (y3 - mean(y3))^2 ))

# Variance explained by the model, SS(Tr)
(SSTr <- SST - SSE)

# Test statistic
(Fobs <- (SSTr/(k-1)) / (SSE/(n-k)))

# P-value
(1 - pf(Fobs, df1 = k-1, df2 = n-k))

########################
### Model validation ###
########################

# Check assumption of homogeneous variance using, e.g., 
# a box plot.
plot(treatm, y)

# Check normality of residuals using a normal QQ-plot
fit1 <- lm(y ~ treatm)
qqnorm(fit1$residuals)
qqline(fit1$residuals)


#####################
### A random draw ###
#####################

# Set random seed ensuring that the random outcome is reproducible
set.seed(1)

# One random draw from (1,2,3,4,5,6) 
# with equal probability for each outcome
sample(1:6, size = 1)

##################################
### Empirical density function ###
### 'Fair dice' example        ###
##################################

# Set random seed ensuring that the random outcome is reproducible
set.seed(2)

# Number of simulated realizations (sample size)
n <- 30

# n independent random draws from the set (1,2,3,4,5,6) 
# with equal probability of each outcome
xFair <- sample(1:6, size = n, replace = TRUE)
xFair

# Count number of each outcome using the 'table' function
table(xFair)
    
# Plot the empirical pdf
plot(table(xFair)/n, lwd = 10, ylim = c(0,1), xlab = "x", 
     ylab = "Density f(x)")
# Add the true pdf to the plot
lines(rep(1/6,6), lwd = 4, type = "h", col = 2)
# Add a legend to the plot
legend("topright", c("Empirical pdf","True pdf"), lty = 1, col = c(1,2), 
       lwd = c(5, 2), cex = 0.8)

##################################
### Empirical density function ###
### 'Unfair dice' example      ###
##################################

# Set random seed ensuring that the random outcome is reproducible
set.seed(3)

# Number of simulated realizations (sample size)
n <- 30

# n independent random draws from the set (1,2,3,4,5,6) 
# with higher probability of getting a six
xUnfair <- sample(1:6, size = n, replace = TRUE, prob = c(rep(1/7,5),2/7))
xUnfair
    
# Plot the empirical pdf
plot(table(xUnfair)/n, lwd = 10, ylim = c(0,1), xlab = "x", 
     ylab = "Density f(x)")
# Add the true pdf to the plot
lines(c(rep(1/7,5),2/7), lwd = 4, type = "h", col = 2)
# Add a legend to the plot
legend("topright", c("Empirical pdf","True pdf"), lty = 1, col = c(1,2), 
       lwd = c(5, 2), cex = 0.8)

###############################################
### Simulating from a binomial distribution ###
###############################################

# Set random seed ensuring that the random outcome is reproducible
set.seed(4)

## Probability of success
p <- 0.1

## Number of repetitions
nRepeat <- 30

## Simulate Bernoulli experiment 'nRepeat' times
tmp <- sample(c(0,1), size = nRepeat, prob = c(1-p,p), replace = TRUE)

# Compute 'x'
sum(tmp)

## Or: Use the binomial distribution simulation function 
rbinom(1, size = 30, prob = p)

########################################################
### Example: Simulating number of six'es (fair dice) ###
########################################################

# Set random seed ensuring that the random outcome is reproducible
set.seed(5)

# Number of simulated realizations (sample size)
n <- 30

# n independent random draws from the set (1,2,3,4,5,6) 
# with equal probability for each outcome
xFair <- sample(1:6, size = n, replace = TRUE)

# Count the number of six'es
sum(xFair == 6)

## Do the same using 'rbinom()' instead
rbinom(n = 1, size = 30, prob = 1/6)


#########################################################
### What is the probability that all six errors are   ###
### corrected within the same day that they occurred? ###
#########################################################

# A binomial distribution with $n = 6$ and $p= 0.7$

# dbinom gives the density, pbinom gives the distribution function, 
# qbinom gives the quantile function and rbinom generates random deviates.

##One way to obtain the result for P(X=6)
dbinom(6,6,0.7)

##Another way is to compute 1- P(X<=5)
1-pbinom(5,6,0.7)




#################################
### Example: The binomial cdf ###
#################################

pbinom(q = 5, size = 10, prob = 0.6)

# Get help with:
?pbinom
###########################
### Sample mean         ###
### 'Fair dice' example ###
###########################

# Set random seed ensuring that random outcome is reproducible
set.seed(6)

# Number of simulated realizations (sample size)
n <- 30

# Sample independently from the set (1,2,3,4,5,6)
# with equal probability of outcomes
xFair <- sample(1:6, size = n, replace = TRUE)

# Compute the sample mean
mean(xFair)

###########################
### Sample variance     ###
### 'Fair dice' example ###
###########################

# Set random seed ensuring that random outcome is reproducible
set.seed(7)

# Number of simulated realizations (sample size)
n <- 30

# Sample independently from the set (1,2,3,4,5,6)
# with equal probability of outcomes
xFair <- sample(1:6, size = n, replace = TRUE)

# Compute the sample variance
var(xFair)
#################################################################
### Simulation study: (Empirical) distribution of sample mean ###
#################################################################

# 'True' mean and standard deviation
mu <- 178
sigma <- 12

# Sample size
n <- 10

# Simulate normal distributed X_i for n = 10
x <- rnorm(n = n, mean = mu, sd = sigma)
x

# Empirical density
hist(x, prob = TRUE, col = 'blue')

# Compute sample mean
mean(x)

# Repeat the simulated sampling many times (100 samples)
mat <- replicate(100, rnorm(n = n, mean = mu, sd = sigma))

# Compute the sample mean for each sample; 
# mat is a matrix, where 1 indicates rows and 2 indications columns
# mean is the function to be applied
# 2 is used because each column in the matrix is the output from one run of the function, i.e. mean

xbar <- apply(mat, 2, mean)
xbar

# See the distribution of the sample means
hist(xbar, prob = TRUE, col = 'blue')

# Empirical mean and variance of sample means
mean(xbar)
var(xbar)

#####################################################################
### Density function for t-distribution with 9 degrees of freedom ###
### plotted together with that of a standard normal distribution  ###
#####################################################################

x <- seq(-4, 4, by = 0.01)
plot(x, dt(x, df = 9), type = "l", col = "red", ylab = "Density(x)")
# "p" for points, "l" for lines, "b" for both points and lines, "c" 
# for empty points joined by lines, "o" for overplotted points and lines, 
# "s" and "S" for stair steps and "h" for histogram-like vertical lines. 
# Finally, "n" does not produce any points or lines#
lines(x, dnorm(x), type = "l")
text(2.5, 0.3,"Black: N(0,1)")
text(3, 0.1,"Red: t(9)", col = "red")

#############################################################
### Example: Heights                                      ###
### Computing a 99% CI for mu using the 't.test' function ###
#############################################################

# Data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)

# 99% CI for mu
t.test(x, conf.level = 0.99)

################################################################################
### CLT Example: Mean of independent and U(0,1)-distributed random variables ###
################################################################################

n <- 1 # Sample size
k <- 1000 # No. of samples (i.e. no. of means to be computed)

# Simulations from U(0,1)-distribution (k = 1000 samples, each of size n = 1)
u <- matrix(runif(k*n), ncol = n)

# Empirical density of means
hist(apply(u, 1, mean), col = "blue", main = "n = 1", xlab = "Means", prob = TRUE)

n <- 2 # Sample size
k <- 1000 # No. of samples (i.e. no. of means to be computed)

# Simulations from U(0,1)-distribution (k = 1000 samples, each of size n = 2)
u <- matrix(runif(k*n), ncol = n)

# Empirical density of means
hist(apply(u, 1, mean), col = "blue", main = "n = 2", xlab = "Means", xlim = c(0,1), prob = TRUE)

n <- 6 # Sample size
k <- 1000 # No. of samples (i.e. no. of means to be computed)

# Simulations from U(0,1)-distribution (k = 1000 samples, each of size n = 6)
u <- matrix(runif(k*n), ncol = n)

# Empirical density of means
hist(apply(u, 1, mean), col = "blue", main = "n = 6", xlab = "Means", xlim = c(0,1), prob = TRUE)

n <- 30 # Sample size
k <- 1000 # No. of samples (i.e. no. of means to be computed)

# Simulations from U(0,1)-distribution (k = 1000 samples, each of size n = 30)
u <- matrix(runif(k*n), ncol = n)

# Empirical density of means
hist(apply(u, 1, mean), col = "blue", main = "n = 30", xlab = "Means", xlim = c(0,1), prob = TRUE)
#######################################
### 'Manual' one-sample t-test in R ###
#######################################

# Enter data
x <- c(1.2, 2.4, 1.3, 1.3, 0.9, 1.0, 1.8, 0.8, 4.6, 1.4) 
n <- length(x) # sample size

# Compute 'tobs' - the observed test statistic
tobs <- (mean(x) - 0) / (sd(x) / sqrt(n))

# Compute the p-value as a tail-probability 
# in the relevant t-distribution:
2 * (1 - pt(abs(tobs), df = n-1))

####################################################
### One-sample t-test in R using t.test function ###
####################################################

t.test(x)

############################################
### Density histogram of student heights ###
############################################

# Student heights data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)

# Density histogram of student height data together with normal pdf
hist(x, xlab = "Height", main = "", freq = FALSE)
lines(seq(160, 200, 1), dnorm(seq(160, 200, 1), mean(x), sd(x)))

# Density histogram of simulated data from normal distribution
# (n = 100) together with normal pdf
xr <- rnorm(100, mean(x), sd(x))
hist(xr, xlab = "Height", main = "", freq = FALSE, ylim = c(0, 0.032))
lines(seq(130, 230, 1), dnorm(seq(130, 230, 1), mean(x), sd(x)))

# Empirical cdf for student height data together 
# with normal cdf
plot(ecdf(x), verticals = TRUE)
xp <- seq(0.9*min(x), 1.1*max(x), length.out = 100) 
lines(xp, pnorm(xp, mean(x), sd(x))) 

# Empirical cdf of simulated data from normal distribution
# (n = 100) together with normal cdf
xr <- rnorm(100,  mean(x), sd(x))
plot(ecdf(xr), verticals = TRUE)
xp <- seq(0.9*min(xr), 1.1*max(xr), length.out = 100) 
lines(xp, pnorm(xp, mean(xr), sd(xr))) 

# Normal q-q plot of student heights
qqnorm(x)
qqline(x)

###########################
### Example: Radon data ###
###########################

## Reading in the data
radon <- c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3,
        1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)

## Histogram and q-q plot of data
par(mfrow = c(1,2))
hist(radon)
qqnorm(radon)
qqline(radon)

# Transform data using the natural logarithm
logRadon<-log(radon)

## Histogram and q-q plot of transformed data
par(mfrow = c(1,2))
hist(logRadon)
qqnorm(logRadon)
qqline(logRadon)

#######################################################
### Nutrition example: Welch two-sample t-test in R ###
#######################################################

# Read the two samples into R
xA = c(7.53, 7.48, 8.08, 8.09, 10.15, 8.4, 10.88, 6.13, 7.9)
xB = c(9.21, 11.51, 12.79, 11.85, 9.97, 8.79, 9.69, 9.68, 9.19)

# Perform Welch two-sample t-test
t.test(xB, xA)

##################################################
### Sleep medicine example: Paired t-test in R ###
##################################################

# Read the paired samples into R
x1 = c(.7,-1.6,-.2,-1.2,-1,3.4,3.7,.8,0,2)
x2 = c(1.9,.8,1.1,.1,-.1,4.4,5.5,1.6,4.6,3.4)

# Compute differences for the paired t-test
dif = x2 - x1

# Perform paired t-test
t.test(dif)

# Another way to perform the paired t-test
t.test(x2, x1, paired = TRUE)

# Normal Q-Q plots separately for each sample
qqnorm(xA, main = "Hospital A")
qqline(xA)
qqnorm(xB, main = "Hospital B")
qqline(xB)

# Multiple (simulated) Q-Q plots and sample A
require(MESS)
fitA <- lm(xA ~ 1)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...); qqline(y, ...)}
wallyplot(fitA, FUN = qqnorm.wally, main = "")

# Multiple (simulated) Q-Q plots and sample B
fitB <- lm(xB ~ 1)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...); qqline(y, ...)}
wallyplot(fitB, FUN = qqnorm.wally, main = "")

#######################################################
### Example: Power-related calculations for t-tests ###
#######################################################

# Power calculation (one-sample)
power.t.test(n = 40, delta = 4, sd = 12.21, type = "one.sample")

# Sample size calculation (one-sample)
power.t.test(power = .80, delta = 4, sd = 12.21, type = "one.sample")

# Power calculation (two-sample)
power.t.test(n = 10, delta = 2, sd = 1, sig.level = 0.05)

# Sample size calculation (two-sample)
power.t.test(power = 0.90, delta = 2, sd = 1, sig.level = 0.05)

# Detectable effect size (two-sample)
power.t.test(power = 0.90, n = 10, sd = 1, sig.level = 0.05)
######################################################################
### Simulating from the exponential distribution with lambda = 0.5 ###  
### using the U(0,1)-distribution                                  ###
######################################################################

xseq <- seq(-5, 10, len = 200)
plot(xseq, pexp(xseq, 1/2), type = "l", xlab = "Exponential outcomes", ylab = "Uniform outcomes", cex.lab = 2)
set.seed(123)
us <- runif(5)
arrows(rep(-5,5), us, qexp(us, 1/2), us, lty = 3, length = 0.2)
arrows(qexp(us, 1/2), us, qexp(us, 1/2), 0, lty = 3, length = 0.2)

##################################
### Simulation: Area of plates ###
##################################

set.seed(345)

k = 10000 # Number of simulations 
X = rnorm(k, 2, 0.01) 
Y = rnorm(k, 3, 0.02) 
A = X*Y 

mean(A) 
var(A) 
mean(abs(A - 6) > 0.1)

########################################################################
### Simulations: Mean of 10 exponential distributed random variables ###
########################################################################

set.seed(9876)

# Number of simulations
k <- 100000

# Simulate 10 exponentials with the 'right' mean k times
sim_samples <- replicate(k, rexp(10, 1/26.08))

# Compute the mean of the 10 simulated observations k times
sim_means <- apply(sim_samples, 2, mean)

# Find relevant quantiles of the k simulated means
quantile(sim_means, c(0.025, 0.975)) 

# Make histogram of simulated means
hist(sim_means, col = "blue", nclass = 30, main = "", prob = TRUE, xlab = "Simulated means")

##########################################################################
### Simulations: Median of 10 exponential distributed random variables ###
##########################################################################

set.seed(9876)

# Number of simulations
k <- 100000

# Simulate 10 exponentials with the 'right' mean k times
sim_samples <- replicate(k, rexp(10, 1/26.08))

# Compute the median of the 10 simulated observations k times
sim_medians <- apply(sim_samples, 2, median)

# Find relevant quantiles of the k simulated medians
quantile(sim_medians, c(0.025, 0.975)) 

# Make histogram of simulated medians
hist(sim_medians, col = "blue", nclass = 30, main = "", prob = TRUE, xlab = "Simulated medians")

######################################################
### Simulation: CI for Q3 of a normal distribution ###
######################################################

set.seed(9876)

# Heights data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
n <- length(x)

# Define a Q3-function
Q3 <- function(x){ quantile(x, 0.75)}

# Set number of simulations
k <- 100000

# Simulate k samples of n = 10 normals with the 'right' mean and variance
sim_samples <- replicate(k, rnorm(n, mean(x), sd(x)))

# Compute the Q3 of the n = 10 simulated observations k times
simQ3s <- apply(sim_samples, 2, Q3)

# Find the two relevant quantiles of the k simulated Q3s
quantile(simQ3s, c(0.005, 0.995)) 

#################################################################################
### Simulations: Confidence interval for difference between exponential means ###
#################################################################################

set.seed(9876)

# Day 1 data
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3,  2.3 , 4.7, 13.6, 2.0)
n1 <- length(x)

# Day 2 data
y <- c(9.6, 22.2, 52.5, 12.6, 33.0, 15.2, 76.6, 36.3, 110.2, 
       18.0, 62.4, 10.3)
n2 <- length(y)

# Set number of simulations:
k <- 100000

# Simulate k samples of each n1 = 10 and n2 = 12 exponentials 
# with the 'right' means

simX_samples <- replicate(k, rexp(n1, 1/mean(x)))
simY_samples <- replicate(k, rexp(n2, 1/mean(y)))

# Compute the difference between the simulated means k times
sim_dif_means <- apply(simX_samples, 2, mean) - 
  apply(simY_samples, 2, mean) 

# Find the relevant quantiles of the k simulated differences of means:
quantile(sim_dif_means, c(0.025, 0.975)) 

##############################################
### Example: Womens' cigarette consumption ###
##############################################

# Data 
x1 <-  c(8, 24, 7, 20, 6, 20, 13, 15, 11, 22, 15) 
x2 <-  c(5, 11, 0, 15, 0, 20, 15, 19, 12, 0, 6) 

# Compute differences
dif <- x1-x2 
dif

# Compute average difference
mean(dif)

# 95% CI for mean by non-parametric bootstrap
k = 100000 
sim_samples = replicate(k, sample(dif, replace = TRUE)) 
sim_means = apply(sim_samples, 2, mean) 
quantile(sim_means, c(0.025,0.975)) 

# 95% CI for median by non-parametric bootstrap
k = 100000 
sim_samples = replicate(k, sample(dif, replace = TRUE)) 
sim_medians = apply(sim_samples, 2, median) 
quantile(sim_medians, c(0.025,0.975)) 

#############################
### Example: Tooth health ###
#############################

# Reading in data
x <- c(9, 10, 12, 6, 10, 8, 6, 20, 12)
y <- c(14,15,19,12,13,13,16,14,9,12) 

# 95% CI for mean difference by non-parametric bootstrap
k <- 100000 
simx_samples <- replicate(k, sample(x, replace = TRUE))
simy_samples <- replicate(k, sample(y, replace = TRUE)) 
sim_mean_difs <- apply(simx_samples, 2, mean)-
                           apply(simy_samples, 2, mean)  
quantile(sim_mean_difs, c(0.025,0.975)) 

# 99% CI for median difference by non-parametric bootstrap
k <- 100000 
simx_samples <- replicate(k, sample(x, replace = TRUE))
simy_samples <- replicate(k, sample(y, replace = TRUE)) 
sim_median_difs <- apply(simx_samples, 2, median)-
                        apply(simy_samples, 2, median)  
quantile(sim_median_difs, c(0.005,0.995)) 
#################################
### Linear regression example ###
### with simulated data 1     ###
#################################

# Set seed (so that simulations may be redone)  
set.seed(100)
  
# Number of data points
n <- 20
  
# Intercept, slope, and std. deviation for simulations 
beta0 <- 50
beta1 <- 200
sigma <- 90
  
# Simulated data points
x <- runif(n, -2, 4)
y <- beta0 + beta1 * x + rnorm(n, mean = 0, sd = sigma)
  
# Scatter plot of x and y
plot(x, y)
  
# Add 'true' line to the plot
lines(x, beta0 + beta1*x, col = 2)

#################################
### Linear regression example ###
### with simulated data 2     ###
#################################

# Set seed (so that simulations may be redone)  
set.seed(100)

# Generate x
x <- runif(n = 20, min = -2, max = 4)

# Simulate y
beta0 <- 50; beta1 <- 200; sigma <- 90
y <- beta0 + beta1 * x + rnorm(n = length(x), mean = 0, sd = sigma)

# From here: like for the analysis of 'real data', we have data in x and y:

# Scatter plot of y against x
plot(x, y)
  
# Find the least squares estimates, use Theorem 5.4
(beta1hat <- sum( (y - mean(y))*(x-mean(x)) ) / sum( (x-mean(x))^2 ))
(bet0hat <- mean(y) - beta1hat*mean(x))
  
# Use lm() to find the estimates
lm(y ~ x)
  
# Plot the fitted  line
abline(lm(y ~ x), col="red")

################################################
### Distribution of estimators of regression ###
### coefficients by simulation               ###
################################################

# Number of repetitions
nRepeat <- 1000

# Two vectors to save the estimates in
Beta0Hat <- numeric(nRepeat)
Beta1Hat <- numeric(nRepeat)

# Repeat the  simulation and estimation nRepeat times
for(i in 1:nRepeat){
  # Generate x
  x <- runif(n = 20, min = -2, max = 4)
  # Simulate from the linear regression model
  beta0 = 50; beta1 = 200; sigma = 90
  y <- beta0 + beta1 * x + rnorm(n = length(x), mean = 0, sd = sigma)
  # Use lm() to find the estimates
  fit <- lm(y ~ x)
  # Save the estimates
  Beta0Hat[i] <- fit$coefficients[1]
  Beta1Hat[i] <- fit$coefficients[2]
}

# See empirical distributions of the estimates
hist(Beta0Hat, probability = TRUE)
hist(Beta1Hat, probability = TRUE)

###########################################
### Linear regression: Hypothesis tests ###
### Example: Height-Weight data         ###
###########################################

# Read data into R

x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Fit model to data
fit <- lm(y ~ x)

# Look at model summary to find Tobs-values and p-values
summary(fit)

##################################################
### Example: Illustration of CIs by simulation ###
##################################################

# Number of repetitions (here: CIs)
nRepeat <- 1000

# Empty logical vector of length nRepeat
TrueValInCI <- logical(nRepeat)

# Repeat the simulation and estimation nRepeat times:
for(i in 1:nRepeat){
  # Generate x
  x <- runif(n = 20, min = -2, max = 4)
  # Simulate y
  beta0 = 50; beta1 = 200; sigma = 90
  y <- beta0 + beta1 * x + rnorm(n = length(x), mean = 0, sd = sigma)
  # Use lm() to fit model
  fit <- lm(y ~ x)
  # Use confint() to compute 95% CI for intercept
  ci <- confint(fit, "(Intercept)", level=0.95)
  # Was the 'true' intercept included in the interval? (covered)
  (TrueValInCI[i] <-  ci[1] < beta0  &  beta0 < ci[2])
}

# How often was the true intercept included in the CI?
sum(TrueValInCI) / nRepeat

##################################################
### Example: Confidence intervals for the line ###
##################################################

# Generate x
x <- runif(n = 20, min = -2, max = 4)

# Simulate y
beta0 = 50; beta1 = 200; sigma = 90
y <- beta0 + beta1 * x + rnorm(n = length(x), sd = sigma)

# Use lm() to fit model
fit <- lm(y ~ x)

# Make a sequence of 100 x-values
xval <- seq(from = -2, to = 6, length.out = 100)

# Use the  predict function
CI <- predict(fit, newdata = data.frame(x = xval),
              interval = "confidence",
              level = 0.95)

# Check what we got
head(CI)

# Plot the data, model fit and intervals
plot(x, y, pch = 20)
abline(fit)
lines(xval, CI[, "lwr"], lty=2, col = "red", lwd = 2)
lines(xval, CI[, "upr"], lty=2, col = "red", lwd = 2)

##################################################
### Example: Prediction intervals for the line ###
##################################################

# Generate x
x <- runif(n = 20, min = -2, max = 4)

# Simulate y
beta0 = 50; beta1 = 200; sigma = 90
y <- beta0 + beta1 * x + rnorm(n = length(x), sd = sigma)

# Use lm() to fit model
fit <- lm(y ~ x)

# Make a sequence of 100 x-values
xval <- seq(from = -2, to = 6, length.out = 100)

# Use the  predict function
PI <- predict(fit, newdata = data.frame(x = xval),
              interval = "prediction",
              level = 0.95)

# Check what we got
head(CI)

# Plot the data, model fit and intervals
plot(x, y, pch = 20)
abline(fit)
lines(xval, PI[, "lwr"], lty = 2, col = "blue", lwd = 2)
lines(xval, PI[, "upr"], lty = 2, col = "blue", lwd = 2)

##############################################
### Linear regression: Correlation and R^2 ###
### Example: Height-Weight data            ###
##############################################

# Read data into R

x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Fit model to data
fit <- lm(y ~ x)

# Scatter plot of data with fitted line
plot(x,y, xlab = "Height", ylab = "Weight")
abline(fit, col="red")  
  
# See summary
summary(fit)
  
# Correlation between  x and y
cor(x,y)
  
# Squared correlation is the "Multiple R-squared" from summary(fit)
cor(x,y)^2

###########################################
### Linear regression: Model validation ###
### Example: Height-Weight data         ###
###########################################

# Read data into R
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Fit model to data
fit <- lm(y ~ x)

# QQ-plot of residuals
qqnorm(fit$residuals, main = "") # or "Wally plot" of residuals

# Plots of residuals against fitted values
plot(fit$fitted, fit$residuals)
################################

## Se info about  data
?airquality
## Copy the data
Air <- airquality
## Remove rows with at least one NA value
Air <- na.omit(Air)

## Remove one outlier
Air <- Air[-which(Air$Ozone == 1), ]

## Check  the empirical density
hist(Air$Ozone, probability=TRUE, xlab="Ozon", main="")

## Concentrations are positive and very skewed, let's  
## log-transform right away: 
## (although really one could wait and check residuals from models)
Air$logOzone <- log(Air$Ozone)
## Bedre epdf?
hist(Air$logOzone, probability=TRUE, xlab="log Ozone", main="")

## Make a time variable (R timeclass, se ?POSIXct)
Air$t <- ISOdate(1973, Air$Month, Air$Day)
## Keep only some of the columns
Air <- Air[ ,c(7,4,3,2,8)]
## New names of the columns
names(Air) <- c("logOzone","temperature","wind","radiation","t")

## What's in  Air?
str(Air)
Air
head(Air)
tail(Air)

## Typically one would  begin with a pairs plot
pairs(Air, panel = panel.smooth, main = "airquality data")
################################

## See the relation between ozone and temperature
plot(Air$temperature, Air$logOzone, xlab="Temperature", ylab="Ozon")

## Correlation
cor(Air$logOzone, Air$temperature)

## Fit a simple linear regression model
summary(lm(logOzone ~ temperature, data=Air))

## Add a vector with random values, is there a significant linear relation?
## ONLY for ILLUSTRATION purposes
Air$noise <- rnorm(nrow(Air))
plot(Air$logOzone, Air$noise, xlab="Noise", ylab="Ozon")
cor(Air$logOzone, Air$noise)
summary(lm(logOzone ~ noise, data=Air))
################################
## With each of the other two independent variables

## Simple linear regression model with the wind speed
plot(Air$logOzone, Air$wind, xlab="logOzone", ylab="Wind speed")
cor(Air$logOzone, Air$wind)
summary(lm(logOzone ~ wind, data=Air))

## Simple linear regression model with the radiation
plot(Air$logOzone, Air$radiation, xlab="logOzone", ylab="Radiation")
cor(Air$logOzone, Air$radiation)
summary(lm(logOzone ~ radiation, data=Air))
################################
## Extend the model

## Forward selection:
## Add wind to the model
summary(lm(logOzone ~ temperature + wind, data=Air))
## Add radiation to the model
summary(lm(logOzone ~ temperature + wind + radiation, data=Air))
################################
## Backward selection

## Fit the full model
summary(lm(logOzone ~ temperature + wind + radiation + noise, data=Air))
## Remove the most non-significant input, are all now significant?
summary(lm(logOzone ~ temperature + wind + radiation, data=Air))
################################
## Assumption of normal distributed residuals

## Save the selected fit
fitSel <- lm(logOzone ~ temperature + wind + radiation, data=Air)

## qq-normalplot
qqnorm(fitSel$residuals)
qqline(fitSel$residuals)
################################
## Plot the residuals vs. predicted values

plot(fitSel$fitted.values, fitSel$residuals, xlab="Predicted values", 
     ylab="Residuals")
################################
## Plot the residuals vs. the independent variables

par(mfrow=c(1,3))
plot(Air$temperature, fitSel$residuals, xlab="Temperature")
plot(Air$wind, fitSel$residuals, xlab="Wind speed")
plot(Air$radiation, fitSel$residuals, xlab="Radiation")
################################
## Extend the ozone model with appropriate curvilinear regression

## Make the squared wind speed
Air$windSq <- Air$wind^2
## Add it to the model
fitWindSq <- lm(logOzone ~ temperature + wind + windSq + radiation, data=Air)
summary(fitWindSq)

## Equivalently for the temperature
Air$temperature2 <- Air$temperature^2
## Add it
fitTemperatureSq <- lm(logOzone ~ temperature + temperature2 + wind + radiation, data=Air)
summary(fitTemperatureSq)

## Equivalently for the radiation
Air$radiation2 <- Air$radiation^2
## Add it
fitRadiationSq <- lm(logOzone ~ temperature + wind + radiation + radiation2, data=Air)
summary(fitRadiationSq)

## Which one was best?
## One could try to extend the model further
fitWindSqTemperaturSq <- lm(logOzone ~ temperature + temperature2 + wind + windSq + radiation, data=Air)
summary(fitWindSqTemperaturSq)

## Model validation
qqnorm(fitWindSq$residuals)
qqline(fitWindSq$residuals)
plot(fitWindSq$residuals, fitWindSq$fitted.values, pch=19)
################################
## Confidence and prediction intervals for the curvilinear model

## Generate a new data.frame with constant temperature and radiation, but with varying wind speed
wind<-seq(1,20.3,by=0.1)
AirForPred <- data.frame(temperature=mean(Air$temperature), wind=wind, 
                         windSq=wind^2, radiation=mean(Air$radiation))

## Calculate confidence and prediction intervals (actually bands)
CI <- predict(fitWindSq, newdata=AirForPred, interval="confidence", level=0.95)
PI <- predict(fitWindSq, newdata=AirForPred, interval="prediction", level=0.95)

## Plot them
plot(wind, CI[,"fit"], ylim=range(CI,PI), type="l", 
     main=paste("At temperature =",format(mean(Air$temperature),digits=3), 
                "and radiation =", format(mean(Air$radiation),digits=3)))
lines(wind, CI[,"lwr"], lty=2, col=2)
lines(wind, CI[,"upr"], lty=2, col=2)
lines(wind, PI[,"lwr"], lty=2, col=3)
lines(wind, PI[,"upr"], lty=2, col=3)
## legend
legend("topright", c("Prediction","95% confidence band","95% prediction band"), lty=c(1,2,2), col=1:3)
################################
## See problems with highly correlated inputs

## Generate values for MLR
n <- 100
## First variable
x1 <- sin(0:(n-1)/(n-1)*2*2*pi) + rnorm(n, 0, 0.1)
plot(x1, type="b")
## The second variable is the first plus a little noise
x2 <- x1 + rnorm(n, 0, 0.1)
## x1 and x2 are highly correlated
plot(x1,x2)
cor(x1,x2)
## Simulate an MLR
beta0=20; beta1=1; beta2=1; sigma=1
y <- beta0 + beta1 * x1 + beta2 * x2 + rnorm(n,0,sigma)
## See scatter plots for y vs. x1, and y vs. x2
par(mfrow=c(1,2))
plot(x1,y)
plot(x2,y)
## Fit an MLR
summary(lm(y ~ x1 + x2))

## If it was an experiment and the effects could be separated in the design
x1[1:(n/2)] <- 0
x2[(n/2):n] <- 0
## Plot them
plot(x1, type="b")
lines(x2, type="b", col="red")
## Now very low correlation
cor(x1,x2)
## Simulate MLR again
y <- beta0 + beta1 * x1 + beta2 * x2 + rnorm(n,0,sigma)
## and fit MLR
summary(lm(y ~ x1 + x2))

