################################################################
# R basic operations
#
# Add two numbers in the console
2+3



# Assign the value 3 to y
y <- 3



# Concatenate numbers to a vector
x <- c(1, 4, 6, 2)
x



# A sequence from 1 to 10
x <- 1:10
x



# Sequence with specified steps
x <- seq(0, 1, by=0.1)
x




################################################################
# Summary statistics in
#
# Sample Mean and Median
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
mean(x)
median(x)



# Sample variance and standard deviation
var(x)
sqrt(var(x))
sd(x)



# Sample quartiles
quantile(x, type=2)



# Sample quantiles 0%, 10%,..,90%, 100%:
quantile(x, probs=seq(0, 1, by=0.10), type=2)




################################################################
# Histogram in R
#
# A histogram of the heights
hist(x)






################################################################
# Empirical density in R
#
# A density histogram or empirical density of the heights
hist(x, prob=TRUE, col="red", nclass=8)





################################################################
# Cumulative distribution plot in R
#
# Empirical cumulative distribution plot
plot(ecdf(x), verticals=TRUE)





################################################################
# Box plot in R
#
# A basic box plot of the heights (range=0 makes it "basic")
boxplot(x, range=0, col="red", main="Basic box plot")
# Add the blue text
text(1.3, quantile(x), c("Minimum","Q1","Median","Q3","Maximum"),
     col="blue")




# Add an extreme value and box plot
boxplot(c(x, 235), col="red", main="Modified box plot")
boxplot(c(x, 235), col="red", main="Basic box plot", range=0)




# Box plot with two groups
Males <-  c(152, 171, 173, 173, 178, 179, 180, 180, 182, 182, 182, 185,
            185 ,185, 185, 185 ,186 ,187 ,190 ,190, 192, 192, 197)
Females <-c(159, 166, 168 ,168 ,171 ,171 ,172, 172, 173, 174 ,175 ,175,
            175, 175, 175, 177, 178)
boxplot(list(Males, Females), col=2:3, names=c("Males", "Females"))




################################################################
# Read and explore data in R
#
# Read the data (note that per default sep="," but here semicolon)
studentheights <- read.table("studentheights.csv", sep=";", dec=".",
                             header=TRUE, stringsAsFactors=TRUE)


options(digits=7)

# Have a look at the first 6 rows of the data
head(studentheights)
# Get an overview
str(studentheights)
# Get a summary of each column/variable in the data
summary(studentheights, quantile.type=2)

options(digits=digits)



# Box plot for each gender
boxplot(Height ~ Gender, data=studentheights, col=2:3)




################################################################
# Explore data included in R
#
# See information about the mtcars data
?mtcars



# To make 2 plots
par(mfrow=c(1,2))
# First the default version
plot(mtcars$wt, mtcars$mpg, xlab="wt", ylab="mpg")
# Then a nicer version
plot(mpg ~ wt, xlab="Car Weight (1000lbs)", data=mtcars,
     ylab="Miles pr. Gallon", col=factor(am),
     main="Inverse fuel usage vs. size")
# Add a legend to the plot
legend("topright", c("Automatic transmission","Manual transmission"), 
       col=c("black","red"), pch=1, cex=0.7)




################################################################
# Bar plots and Pie charts in R
#
# Barplot
barplot(table(studentheights$Gender), col=2:3)



# Pie chart
pie(table(studentheights$Gender), cex=1, radius=1)







options(width=60)
## Reading the data into R
before <- c(9.1, 8.0, 7.7, 10.0, 9.6, 7.9, 9.0, 7.1, 8.3, 9.6,
            8.2, 9.2, 7.3, 8.5, 9.5)
after  <- c(8.2, 6.4, 6.6, 8.5, 8.0, 5.8, 7.8, 7.2, 6.7, 9.8,
            7.1, 7.7, 6.0, 6.6, 8.4)
## Making ordered vectors using the 'sort' function
sortedBefore <- sort(before)
sortedAfter <- sort(after)
## Printing the ordered vectors
sortedBefore
sortedAfter
## Printing the 8th observation in these vectors
sortedBefore[8]
sortedAfter[8]
set.seed(80938)
################################################################
# Example: Simulation of rolling a dice
#
# Make a random draw from (1,2,3,4,5,6) with equal probability
# for each outcome
sample(1:6, size=1)


set.seed(9783)

# Simulate a fair dice

# Number of simulated realizations
n <- 30
# Draw independently from the set (1,2,3,4,5,6) with equal probability
xFair <- sample(1:6, size=n, replace=TRUE)
# Count the number of each outcome using the table function
table(xFair)
# Plot the pdf
par(mfrow=c(1,2))
plot(rep(1/6,6), type="h", col="red", ylim=c(0,1), lwd=10)
# Plot the empirical pdf
lines(table(xFair)/n, lwd=4)
# Plot the cdf
plot(cumsum(rep(1/6,6)), ylim=c(0,1), lwd=10, type="h", col="red")
# Add the empirical cdf
lines(cumsum(table(xFair)/n), lwd=4, type="h")




# Simulate an unfair dice

# Number of simulated realizations
n <- 30
# Draw independently from the set (1,2,3,4,5,6) with higher
# probability for a six
xUnfair <- sample(1:6, size=n, replace=TRUE, prob=c(rep(1/7,5),2/7))
# Plot the pdf
plot(c(rep(1/7,5),2/7), type="h", col="red", ylim=c(0,1), lwd=10)
# Plot the empirical density function
lines(table(xUnfair)/n, lwd=4)
# Plot the cdf
plot(cumsum(c(rep(1/7,5),2/7)), ylim=c(0,1), lwd=10, type="h", col="red")
# Add the empirical cdf
lines(cumsum(table(xUnfair)/n), lwd=4, type="h")



set.seed(87)


################################################################
# Random numbers and seed in R
#
# The random numbers generated depends on the seed

# Set the seed
set.seed(127)
# Generate a (pseudo) random sequence
sample(1:10)
# Generate again and see that new numbers are generated
sample(1:10)
# Set the seed and the same numbers as before just after the 
# seed was set are generated
set.seed(127)
sample(1:10)


set.seed(782)


################################################################
# Example: Simulate and estimate the mean
##
# Simulate a fair dice

# Number of realizations
n <- 30
# Simulate rolls with a fair dice
xFair <- sample(1:6, size=n, replace=TRUE)
# Calculate the sample mean
sum(xFair)/n
# or
mean(xFair)


set.seed(672)    

# Simulate an unfair dice

# n realizations
xUnfair <- sample(1:6, size=n, replace=TRUE, prob=c(rep(1/7,5),2/7))
# Calculate the sample mean
mean(xUnfair)


set.seed(76241)    


################################################################
# Example: Simulate and estimate the sample variance
##
# Simulate a fair dice and calculate the sample variance

# Number of realizations
n <- 30
# Simulate
xFair <- sample(1:6, size=n, replace=TRUE)
# Calculate the distance for each sample to the sample mean
distances <- xFair - mean(xFair)
# Calculate the average of the squared distances
sum(distances^2)/(n-1)
# Or use the built in function
var(xFair)



# Plot the pdf of the six-sided dice and the four-sided dice
plot(rep(1/6,6), type="h", col="red")
plot(rep(1/4,4), type="h", col="blue")



# Calculate the means and variances of the dices

# The means
muXSixsided <- sum((1:6)*1/6)  # Six-sided
muXFoursided <- sum((1:4)*1/4)  # Four-sided
# The variances
sum((1:6-muXSixsided)^2*1/6)
sum((1:4-muXFoursided)^2*1/4)




################################################################
# Example: simulation with a binomial distribution
##
# Simulate a binomial distributed experiment

# Number of flips
nFlips <- 10
# The possible outcomes are (0,1,...,nFlips)
xSeq <- 0:nFlips
# Use the dbinom() function which returns the pdf, see ?dbinom
pdfSeq <- dbinom(xSeq, size=nFlips, prob=1/2)
# Plot the density
plot(xSeq, pdfSeq, type="h")



set.seed(572)


################################################################
# Example: Simulate 30 successive dice rolls
#  
# Simulate 30 successive dice rolls
Xfair <- sample(1:6, size=30, replace=TRUE)
# Count the number sixes obtained
sum(Xfair==6)
# This is equivalent to
rbinom(1, size=30, prob=1/6)




################################################################
# Example: Lottery probabilities using the hypergeometric distribution
##
# The probability of getting x numbers of the sheet in 25 drawings

# Number of successes in the population
a <- 8
# Size of the population
N <- 90
# Number of draws
n <- 25
# Plot the pdf, note: parameters names are different in the R function
plot(0:8, dhyper(x=0:8,m=a,n=N-a,k=n), type="h")





################################################################
# Example: Poisson rate scaling
##
# Probability of no goals in 10 minutes

# The Poisson pdf
dpois(x=0, lambda=3.4/9)




################################################################
# Example: Poisson distributed random variable
##
# Simulate a Poisson random variable

# The mean rate of events per interval
lambda <- 4
# Number of realizations
n <- 1000
# Simulate
x <- rpois(n, lambda)
# Plot the empirical pdf
plot(table(x)/n)
# Add the pdf to the plot
lines(0:20, dpois(0:20,lambda), type="h", col="red")











################################################################
# Example: The normal pdf
##
# Play with the normal distribution
  
# The mean and standard deviation
muX <- 0
sigmaX <- 1
# A sequence of x values
xSeq <- seq(-6, 6, by=0.1)
##
pdfX <- 1/(sigmaX*sqrt(2*pi)) * exp(-(xSeq-muX)^2/(2*sigmaX^2))
# Plot the pdf
plot(xSeq, pdfX, type="l", xlab="$x$", ylab="f(x)")








set.seed(5262)


################################################################
# Example: R functions for the normal distribution
#  
# Do it for a sequence of x values
xSeq <- c(-3,-2,1,0,1,2,3)
# The pdf
dnorm(xSeq, mean=0, sd=1)
# The cdf
pnorm(xSeq, mean=0, sd=1)
# The quantiles
qnorm(c(0.01,0.025,0.05,0.5,0.95,0.975,0.99), mean=0, sd=1)
# Generate random normal distributed realizations
rnorm(n=10, mean=0, sd=1)
# Calculate the probability that the outcome of X is between a and b
a <- 0.2
b <- 0.8
pnorm(b) - pnorm(a)
# See more details by running "?dnorm"




################################################################
# Example: Exponential distributed time intervals
#
# Simulate exponential waiting times

# The rate parameter: events per time
lambda <- 4
# Number of realizations
n <- 1000
# Simulate
x <- rexp(n, lambda)
# The empirical pdf
hist(x, probability=TRUE)
# Add the pdf to the plot
curve(dexp(xseq,lambda), xname="xseq", add=TRUE, col="red")




# Check the relation to the Poisson distribution
# by counting the events in each interval

# Sum up to get the running time
xCum <- cumsum(x)
# Use the hist function to count in intervals between the breaks,
# here 0,1,2,...
tmp <- hist(xCum, breaks=0:ceiling(max(xCum)))
# Plot the discrete empirical pdf
plot(table(tmp$counts)/length(tmp$counts))
# Add the Poisson pdf to the plot
lines(0:20, dpois(0:20,lambda), type="h", col="red")





set.seed(783773)


################################################################
# Example: Random numbers in R
#
# Generate 100 normal distributed values
rnorm(100, mean=2, sd=3)
# Similarly, generate 100 uniform distributed values from 0 to 1 and 
# put them through the inverse normal cdf
qnorm(runif(100), mean=2, sd=3)


set.seed(333)  


################################################################
# Example: Simulating the exponential distribution
#
# Three equivalent ways of simulating the exponential distribution
# with lambda=1/2
re1 <- -2*log(1-runif(10000))

re2 <- qexp(runif(10000), 1/2)

re3 <- rexp(10000, 1/2)

# Check the means and variances of each
c(mean(re1), mean(re2), mean(re3)) 

c(var(re1), var(re2), var(re3)) 






################################################################
# Simulation of chi^2-distribution
#
# Simulate 10 realizations from a standard normal distributed variable
n <- 10
rnorm(n)
# Now repeat this 200 times and calculate the sum of squares each time
# Note: the use of the function replicate: it repeats the
#       expression in the 2nd argument k times, see ?replicate
k <- 200
x <- replicate(k, sum(rnorm(n)^2))
# Plot the epdf of the sums and compare to the theoretical chisquare pdf
par(mfrow=c(1,2))
hist(x, freq=FALSE)
curve(dchisq(xseq,df=n), xname="xseq", add=TRUE, col="red")
# and the ecdf compared to the cdf
plot(ecdf(x))
curve(pchisq(xseq,df=n), xname="xseq", add=TRUE, col="red")





################################################################
# Example: Milk dose machines
#
# Chi-square milk dosing precision

# The sample size
n <- 20
# The claimed deviation
sigma <- 0.02
# The observed sample standard deviation
s <- 0.03
# Calculate the chi-square statistic
chiSq <- (n-1)*s^2 / sigma^2
# Use the cdf to calculate the probability of getting the observed 
# sample standard deviation or higher
1 - pchisq(chiSq, df=n-1)




################################################################
# Example: Relation between normal and chi^2
#
# Set simulate parameters
nu <- 8; k <- 200
# Generate the simulated realizations
z <- rnorm(k)
y <- rchisq(k, df=nu)
x <- z/sqrt(y/nu)
# Plot
par(mfrow=c(1,2))
hist(x, freq = FALSE)
curve(dt(xseq, df = nu), xname="xseq", add=TRUE, col="red")
plot(ecdf(x))
curve(pt(xseq, df = nu), xname="xseq", add=TRUE, col="red")





################################################################
# Example: Simulation of t-distribution
#
# Simulate
n <- 8; k <- 200; mu <- 1; sigma <- 2
# Repeat k times the simulation of a normal dist. sample:
# return the values in a (n x k) matrix
x <- replicate(k, rnorm(n, mean=mu, sd=sigma))
xbar <- apply(x, 2, mean)
s <- apply(x, 2, sd)
tobs <- (xbar - mu)/(s/sqrt(n))
# Plot
par(mfrow=c(1,2))
hist(tobs, freq = FALSE)
curve(dt(xseq, df=n-1), xname="xseq", add=TRUE, col="red")
plot(ecdf(tobs))
curve(pt(xseq, df=n-1), xname="xseq", add=TRUE, col="red")



    

################################################################
# Example: Electric car driving distance
#
# Calculate the probability of getting the sample mean under the 
# conditions that the claim is actually the real mean

# A test of 10 cars was carried out
n <- 10
# The claim is that the real mean is 400 km
muX <- 400
# From the sample the sample mean was calculated to
xMean <- 393
# And the sample deviation was
xSD <- 14
# Use the cdf to calculate the probability of obtaining this 
# sample mean or a lower value
pt( (xMean-muX) / (xSD/sqrt(n)), df=n-1)




################################################################
# Example: t-distribution
#
# Plot the t-distribution for different sample sizes

# First plot the standard normal distribution
curve(dnorm(x), xlim=c(-5,5), xlab="x", ylab="Density")
# Add the t-distribution for 30 observations
curve(dt(x,df=30-1), add=TRUE, col=2)
# Add the t-distribution for 15, 5 and 2 observations
curve(dt(x,df=15-1), add=TRUE, col=3)
curve(dt(x,df=5-1), add=TRUE, col=4)
curve(dt(x,df=2-1), add=TRUE, col=5)
# Add a legend
legend("topright", c("Norm.",paste("n=",c(30,15,5,2))), lty=1, col=1:6, cex=0.8)




################################################################
# Example: F-distribution
#
# Simulate
nu1 <- 8; nu2 <- 10; k <- 200
u <- rchisq(k, df=nu1)
v <- rchisq(k, df=nu2)
fobs <- (u/nu1) / (v/nu2)
# Plot
par(mfrow=c(1,2))
hist(fobs, freq = FALSE)
curve(df(x, df1=nu1, df2=nu2), add=TRUE, col="red")
plot(ecdf(fobs))
curve(pf(x, df1=nu1, df2=nu2), add=TRUE, col="red")





################################################################
# Example: Relation between normal and F-distribution
#
# Simulate
n1 <- 8; n2 <- 10; k <- 200
mu1 <- 2; mu2 <- -1
sigma1 <- 2; sigma2 <- 4
s1 <- replicate(k, sd(rnorm(n1, mean=mu1, sd=sigma1)))
s2 <- replicate(k, sd(rnorm(n2, mean=mu2, sd=sigma2)))
fobs <- (s1^2 / sigma1^2) / (s2^2 / sigma2^2)
# Plot
par(mfrow=c(1,2))
hist(fobs, freq=FALSE)
curve(df(xseq, df1=n1-1, df2=n2-1), xname="xseq", add=TRUE, col="red")
plot(ecdf(fobs))
curve(pf(xseq, df1=n1-1, df2=n2-1), xname="xseq", add=TRUE, col="red")
################################################################
# Example: Normal and t probabilities and quantiles
#
# The P(T>1.96) probability  for n=10
1 - pt(1.96, df = 9)
# The P(Z>1.96) probability
1 - pnorm(1.96)
# The P(T>1.96) probability  for n-values, 10, 20, ... ,50
1 - pt(1.96, df = seq(9, 49, by = 10))
# The P(T>1.96) probability  for n-values, 100, 200, ... ,500
1 - pt(1.96, df = seq(99, 499, by = 100))


# The standard normal 0.975% quantile
qnorm(0.975)
# The t-quantiles for n-values: 10, 20, ... ,50
# (rounded to 3 decimal points)
qt(0.975, df = seq(9, 49, by = 10))
# The t-quantiles for n-values: 100, 200, ... ,500 
# (rounded to 3 decimal points)
qt(0.975, df = seq(99, 499, by = 100))




################################################################
# Example: Student heights
#
# The t-quantiles for n=10:
qt(0.975, 9)






################################################################
# Example: Student heights
#
# The 99% confidence interval for the mean
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
n <- length(x)
mean(x) - qt(0.995, df = 9) * sd(x) / sqrt(n)
mean(x) + qt(0.995, df = 9) * sd(x) / sqrt(n)



# The 99% confidence interval for the mean
t.test(x, conf.level = 0.99)


set.seed(1234)


################################################################
# Example: Central Limit Theorem in practice
#
par(mfrow=c(2,2))
xlim <- c(0,1)
# Number of simulated samples
k <- 1000

# Number of observations in each sample
n <- 1
# Simulate k samples with n observations
# Note, the use of replicate: it repeats the second argument (here k times)
Xbar <- replicate(k, runif(n))
hist(Xbar, col="blue", main="n=1", xlab="Sample means", xlim=xlim)
# Increase the number of observations in each sample
# Note, the use of apply here: it takes the mean on the 2nd dimension 
# (i.e. column) of the matrix returned by replicate
n <- 2
Xbar <- apply(replicate(k, runif(n)), 2, mean)
hist(Xbar, col="blue", main="n=2", xlab="Sample means", xlim=xlim)
# Increase the number of observations in each sample
n <- 6
Xbar <- apply(replicate(k, runif(n)), 2, mean)
hist(Xbar, col="blue", main="n=6", xlab="Sample means", xlim=xlim)
# Increase the number of observations in each sample
n <- 30
Xbar <- apply(replicate(k, runif(n)), 2, mean)
hist(Xbar, col="blue", main="n=30", xlab="Sample means", xlim=xlim)


# We set a seed to be able get the same results
set.seed(12345.6789)


################################################################
# Example: Simulating many confidence intervals
#
# Simulate 1000 samples of n=50 observations, and
# calculate a CI from each sample
k <- 1000
ThousandCIs <- replicate(k, t.test(rnorm(n=50, mean=1, sd=1))$conf.int)
# Count how often 1 is covered
sum(ThousandCIs[1,] < 1 & 1 < ThousandCIs[2,])




################################################################
# Example: The chi-square-distribution
#
# The chisquare-distribution with df=9 (the density)
x <- seq(0, 35, by = 0.1)
plot(x, dchisq(x, df = 9), type = "l", ylab="Density")




# Reading the data into R:
xA <- c(.7,-1.6,-.2,-1.2,-1,3.4,3.7,.8,0,2)
xB <- c(1.9,.8,1.1,.1,-.1,4.4,5.5,1.6,4.6,3.4)
dif <- xB-xA
dif
t.test(dif)




################################################################
# Example: Sleeping medicine
#
# Enter sleep difference observations
x <- c(1.2, 2.4, 1.3, 1.3, 0.9, 1.0, 1.8, 0.8, 4.6, 1.4) 
n <- length(x)
# Compute the tobs - the observed test statistic
tobs <- (mean(x) - 0) / (sd(x) / sqrt(n))
tobs
# Compute the p-value as a tail-probability in the t-distribution
pvalue <- 2 * (1-pt(abs(tobs), df=n-1))
pvalue



t.test(x)






################################################################
# Example: Student heights
#
# The height sample
x <- c(168,161,167,179,184,166,198,187,191,179)

# Using histograms
par(mfrow=c(1,3), mar=c(4,3,1,1))
hist(x, xlab="Height", main="")
hist(x, xlab="Height", main="", breaks=8)
hist(x, xlab="Height", main="", breaks=2)



# Plot the empirical cdf
plot(ecdf(x), verticals = TRUE)
# Plot the best normal cdf
xseq <- seq(0.9*min(x), 1.1*max(x), length.out = 100) 
lines(xseq, pnorm(xseq, mean(x), sd(x))) 



# The expected quantiles in a 0 to 1 uniform distribution 
n <- length(x)
# They have equal distance
pseq <- (1:n-0.5)/n
# Plot the expected normal distribution quantiles
plot(x=qnorm(p=pseq), y=sort(x), xlab="Normal quantiles", 
     ylab="Sample quantiles")
# Mark the 1st and 3rd quantiles with crosses
points(x=qnorm(p=c(0.25,0.75)), y=quantile(x,probs=c(0.25,0.75)), 
       pch=3, col="red")
# Add a straight line through the 1st and 3rd quantiles
qqline(x)


set.seed(89473)

# Simulate 100 normal distributed observations
xsim <- rnorm(100,  mean(x), sd(x))
# Do the q-q normal plot with inbuilt functions
qqnorm(xsim)
qqline(xsim)


set.seed(98)

# Do the Wally plot

# To install the MESS package (run only once)
if (!require("MESS")) install.packages("MESS")
# Load the MESS package
library(MESS)

# Define the plotting function
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}
# Do the Wally plot
wallyplot(x-mean(x), FUN=qqwrap, ylim=c(-3,3))




################################################################
# Example: Radon in houses
#
# Reading in the sample
radon <- c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3,
           1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)
# A histrogram and q-q plot
par(mfrow = c(1,2))
hist(radon)
qqnorm(radon, ylab = "Sample quantiles", xlab = "Normal quantiles")
qqline(radon)



# Transform using the natural logarithm
logRadon <- log(radon)
par(mfrow = c(1,2))
hist(logRadon)
qqnorm(logRadon, ylab = "Sample quantiles", xlab = "Normal quantiles")
qqline(logRadon)



# A confidence interval and t-test
t.test(logRadon, conf.level=0.95)

# Back transform to original scale, now we get the median!
exp(0.9644)

# And the confidence interval on the original scale
exp(c(0.7054, 1.2234))






################################################################
# Example: Nutrition study
#
# Load the two samples
xA <- c(7.53, 7.48, 8.08, 8.09, 10.15, 8.4, 10.88, 6.13, 7.9)
xB <- c(9.21, 11.51, 12.79, 11.85, 9.97, 8.79, 9.69, 9.68, 9.19)
# Summary statistics
c(mean(xA), mean(xB))
c(var(xA), var(xB))
c(length(xA), length(xB))


t.test(xB,xA)
# Check computations with 3 decimal points:
ms=round(c(mean(xA), mean(xB)),3)
vs=round(c(var(xA), var(xB)),3)
ns=c(length(xA), length(xB))
(ms[2]-ms[1])/sqrt(vs[1]/ns[1]+vs[2]/ns[2])
nu=((vs[1]/ns[1]+vs[2]/ns[2])^2)/((vs[1]/ns[1])^2/(ns[1]-1)+(vs[2]/ns[2])^2/(ns[2]-1))
nu



# Keep the summary statistics
ms <- c(mean(xA), mean(xB))
vs <- c(var(xA), var(xB))
ns <- c(length(xA), length(xB))
# The observed statistic
t_obs <- (ms[2]-ms[1])/sqrt(vs[1]/ns[1]+vs[2]/ns[2])
# The degrees of freedom
nu <- ((vs[1]/ns[1]+vs[2]/ns[2])^2)/
  ((vs[1]/ns[1])^2/(ns[1]-1)+(vs[2]/ns[2])^2/(ns[2]-1))
# Print the result
t_obs
nu



# The probability of observed greater that t_obs
1 - pt(t_obs, df = nu)



# Use the built in function for t-test
t.test(xB, xA)



# The critical values for the test
qt(0.975, df = 15.99)




################################################################
# Example: Overlapping confidence intervals?
#
# The confidence intervals and joining the lower and upper limits
CIA <- t.test(xA)$conf.int
CIB <- t.test(xB)$conf.int
lower <- c(CIA[1], CIB[1])
upper <- c(CIA[2], CIB[2])
# The barplot of means WITH CIs added using gplots-package
# First install the package with: install.packages("gplots")
library(gplots)
barplot2(c(mean(xA),mean(xB)), plot.ci=TRUE, ci.l=lower, ci.u=upper,
         col = 2:3)


# This chunk makes the plot, since gplots is not working on the compute server
CIA <- t.test(xA)$conf.int
CIB <- t.test(xB)$conf.int
lower <- c(CIA[1], CIB[1])
upper <- c(CIA[2], CIB[2])
#
barplot(c(mean(xA),mean(xB)), col = 2:3, xlim=c(0,2.5), ylim=c(0,11.5))
arrows(0.7,lower[1],0.7,lower[2], code=3, angle=90, length=0.3)
arrows(1.4+0.5,upper[1],1.4+0.5,upper[2], code=3, angle=90, length=0.3)     



# The confidence intervals 
CIA 
CIB 




################################################################
# Example: Sleeping medicine
#
# Read the samples
x1 <- c(.7,-1.6,-.2,-1.2,-1,3.4,3.7,.8,0,2)
x2 <- c(1.9,.8,1.1,.1,-.1,4.4,5.5,1.6,4.6,3.4)
# Take the differences
dif <- x2 - x1
# t-test on the differences
t.test(dif)



# Give both samples, but make paired t-test
t.test(x2, x1, paired = TRUE)



# WRONG analysis
t.test(x1, x2)



# The sample variances of each sample and of the differences
var(x1)
var(x2)
var(x1-x2)







# The sample size for power=0.80
power.t.test(power=0.8, delta=4, sd=12.21, sig.level=0.05,
             type="one.sample")



# The power for n=50
power.t.test(n=50, delta=4, sd=12.21, sig.level=0.05,
             type="one.sample")



# The detectable effect size for n=50 and power=0.80
power.t.test(n=50, power=0.80, sd=12.21, sig.level=0.05,
             type="one.sample")




################################################################
# Example: Two-sample power and sample size computations in R
#
# Finding the power of detecting a group difference of 2 
# with sigma=1 for n=10
power.t.test(n=10, delta=2, sd=1, sig.level=0.05)


# Finding the sample size for detecting a group difference of 2 
# with sigma=1 and power=0.9
power.t.test(power=0.90, delta=2, sd=1, sig.level=0.05)



# Finding the detectable effect size (delta) 
# with sigma=1, n=10 and power=0.9
power.t.test(power=0.90, n=10, sd=1, sig.level=0.05)







par(mfrow = c(3, 3))
par(cex=0.8)
for (i in 1:9){
    xr <- rnorm(9)
    qqnorm(xr, main="")
    qqline(xr)
}
set.seed(345)
################################################################
# Example: Rectangular plates
#
# Number of simulations 
k <- 10000 
# Simulate X, Y and then A
X <- rnorm(k, 2, 0.01) 
Y <- rnorm(k, 3, 0.02) 
A <- X*Y 



# The mean and std. deviation of the simulated values
mean(A) 
sd(A) 


mean(abs(A-6)>0.1)




set.seed(9876)
 

################################################################
# One-sample confidence interval for mu
#
# Read the data 
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 2.3 , 4.7, 13.6, 2.0)
n <- length(x) 
# Set the number of simulations
k <- 100000
# 1. Simulate 10 exponentials with the sample mean k times
simsamples <- replicate(k, rexp(10,1/26.08))
# 2. Compute the mean of the 10 simulated observations k times
simmeans <- apply(simsamples, 2, mean)
# 3. Find the two relevant quantiles of the k simulated means
quantile(simmeans, c(0.025, 0.975)) 



# Histogram of the simulated means
hist(simmeans, col="blue", nclass=30, cex.main=0.8)


set.seed(9876)


################################################################
# Confidence interval for the median assuming an exponential distribution
#
# Load the data
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 2.3 , 4.7, 13.6, 2.0)
n <- length(x)
# Set the number of simulations
k <- 100000
# 1. Simulate k samples of n=10 exponentials with the sample mean
simsamples <- replicate(k, rexp(n,1/26.08))
# 2. Compute the median of the n=10 simulated observations k times:
simmedians <- apply(simsamples, 2, median)
# 3. Find the two relevant quantiles of the k simulated medians:
quantile(simmedians, c(0.025, 0.975)) 



# See the simulated medians
hist(simmedians, col="blue", nclass=30, cex.main=0.8)


Q3 <- function(x){ quantile(x, 0.75) }


set.seed(9876)
# load in the data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
n <- length(x)
 # Set the number of simulations:
k <- 100000
# 1. Simulate k samples of n=10 normals with the sample mean and
#    variance:
simsamples <- replicate(k, rnorm(n, mean(x), sd(x)))
# 2. Compute the Q3 of the n=10 simulated observations k times:
simQ3 <- apply(simsamples, 2, Q3)
# 3. Find the two relevant quantiles of the k simulated medians:
quantile(simQ3, c(0.005, 0.995)) 


hist(simQ3, col="blue", cex.main=0.8)


set.seed(9876)


################################################################
# CI for the difference of two means from exponential distributed data
#
# Read the data
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 2.3 , 4.7, 13.6, 2.0)
y <- c(9.6, 22.2, 52.5, 12.6, 33.0, 15.2, 76.6, 36.3, 110.2, 18.0, 
       62.4, 10.3)
n1 <- length(x)
n2 <- length(y)
# Set the number of simulations
k <- 100000

# 1. Simulate k samples of each n1=10 and n2=12 exponentials 
#    with the sample means
simXsamples <- replicate(k, rexp(n1,1/mean(x)))
simYsamples <- replicate(k, rexp(n2,1/mean(y))) 
# 2. Compute the difference between the simulated means k times
simDifmeans <- apply(simXsamples,2,mean) - apply(simYsamples,2,mean) 
# 3. Find the two relevant quantiles of the k simulated differences 
#    in sample means
quantile(simDifmeans, c(0.025, 0.975), cex.main=0.8) 



# The histogram of the simulated sample mean differences
hist(simDifmeans, col="blue", nclass=25, cex.main=0.8)




################################################################
# Nutrition study: comparing medians assuming normal distributions
#
# Read the data
xA <- c(7.53, 7.48, 8.08, 8.09, 10.15, 8.4, 10.88, 6.13, 7.9)
xB <- c(9.21, 11.51, 12.79, 11.85, 9.97, 8.79, 9.69, 9.68, 9.19)
nA <- length(xA)
nB <- length(xB)


set.seed(9843)

# Set the number of simulations
k <- 100000
# 1. Simulate k samples of each nA=9 and nB=9 exponentials with the 
#    sample means and standard deviations
simAsamples <- replicate(k, rnorm(nA, mean(xA), sd(xA)))
simBsamples <- replicate(k, rnorm(nB, mean(xB), sd(xB)))
 
# 2. Compute the difference between the simulated medians k times
simDifmedians <- apply(simAsamples, 2, median) - apply(simBsamples, 2,
                                                       median) 
# 3. Find the two relevant quantiles of the k simulated differences
#    of means
quantile(simDifmedians, c(0.025, 0.975)) 


 

################################################################
# Example: Women's cigarette consumption
#
# Read and calculate the differences for each woman before and after
x1 <-  c(8, 24, 7, 20, 6, 20, 13, 15, 11, 22, 15) 
x2 <-  c(5, 11, 0, 15, 0, 20, 15, 19, 12, 0, 6) 
dif <- x1-x2 
dif 


set.seed(7266)

t(replicate(5, sample(dif, replace=TRUE)))


set.seed(112)

# Number of simulated samples
k <- 100000 
# Simulate
simsamples <- replicate(k, sample(dif, replace=TRUE)) 
# Calculate the mean of each simulated sample
simmeans <- apply(simsamples, 2, mean) 
# Quantiles of the differences gives the CI
quantile(simmeans, c(0.025,0.975)) 


 
# The 95% CI for the median change
k <- 100000 
simsamples <- replicate(k, sample(dif, replace = TRUE)) 
simmedians <- apply(simsamples, 2, median) 
quantile(simmedians, c(0.025,0.975)) 



 
################################################################
# Example: Teeth and bottle
#
# Reading in "no bottle" group
x <- c(9, 10, 12, 6, 10, 8, 6, 20, 12) 
# Reading in "yes bottle" group
y <- c(14,15,19,12,13,13,16,14,9,12) 

# Number of simulations
k <- 100000 
# Simulate each sample k times
simxsamples <- replicate(k, sample(x, replace=TRUE))
simysamples <- replicate(k, sample(y, replace=TRUE)) 
# Calculate the sample mean differences
simmeandifs <- apply(simxsamples,2,mean) - apply(simysamples,2,mean)  
# Quantiles of the differences gives the CI
quantile(simmeandifs, c(0.025,0.975)) 



# CI for the median differences
simmediandifs <- apply(simxsamples,2,median)-apply(simysamples,2,median)  
quantile(simmediandifs, c(0.005,0.995)) 




################################################################
# Bootstrapping - a further perspective
#
# Install the bootstrap package
install.packages("bootstrap")


set.seed(3249)  

# Calculate the 95% CI for the Teeth and bottle example above
library(bootstrap) 
quantile(bootstrap(dif,k,mean)$thetastar, c(0.025,0.975)) 




################################################################
# Example: Bootstrapping the mean mu with the boot package
#
# Read and calculate the differences for each woman before and after
x1 <- c(8,24,7,20,6,20,13,15,11,22,15) 
x2 <- c(5,11,0,15,0,20,15,19,12,0,6) 
dif <- x1-x2 



# Define function for calculating the mean of the d indexes
samplemean <- function(x, d){ mean(x[d]) }



# Call the new function
mean(dif)
samplemean(dif,1:11)
samplemean(dif,c(1,3))
dif
dif[c(1,3)]
mean(dif[c(1,3)])


set.seed(231)

# Load the boot package
library(boot)

# Non-parametric bootstrap of the mean difference:
k <- 10000
meandifboot <- boot(dif, samplemean, k) 
plot(meandifboot)



# Percentile bootstrap CI:
boot.ci(meandifboot, type="perc")


# Bias Corrected Accelerated CI:
boot.ci(meandifboot, type="bca")



# Define a function for taking the median in the needed format
samplemedian <- function(x, d) {
  return(median(x[d]))
}
# Non-parametric bootstrap of the x1 median
b <- boot(x1,samplemedian,k) 
boot.ci(b, type="bca")


# Non-parametric bootstrap of the Dif coef. of var
samplecoefvar <- function(x, d) {
  return(sd(x[d])/mean(x[d]))
}
#
b <- boot(dif,samplecoefvar,k) 
boot.ci(b, type="bca")


# Example with data frame and two variables

# Making our own data for the example - into a data frame:
x <- runif(100)
y <- x + 2*runif(100)
D <- data.frame(x, y)
head(D)
plot(D)
cor(D$x,D$y)



# The correlation function on the right form:
mycor  <- function(D, d) {
  E <- D[d,]
  return(cor(E$x, E$y))
}


# Check:
mycor(D, 1:100)
mycor(D, 1:15)


# Doing the bootstrap on the data frame:
b <-  boot(D, mycor, 10000)
boot.ci(b, type="bca")


# Bootstrapping an R-Squared from an MLR using 
# the data set mtcars from the boot package:
# (And showing how to send EXTRA stuff to your function)

# function to obtain R-Squared from the data 
# AND working for ANY model fit you want!!


rsq <- function(formula, data, d) {
  fit <- lm(formula, data=data[d,])
  return(summary(fit)$r.square)
} 


# Bootstrapping with 1000 replications 
b <- boot(mtcars, rsq, 1000, formula=mpg~wt+disp)
plot(b)


boot.ci(b, type="bca")
# Read data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Calculate averages 
xbar <- mean(x)
ybar <- mean(y)

# Parameters estimates
Sxx <- sum((x - xbar)^2)
beta1hat <- sum((x - xbar)*(y - ybar)) / Sxx
beta0hat <- ybar - beta1hat * xbar


D <- data.frame(x=x, y=y)
fitStudents <- lm(y ~ x, data=D)
summary(fitStudents)


  set.seed(124)
  n <- 10; k <- 500
  beta0 <- 10;  beta1 <- 3;  sigma <- 5
  x <- seq(-2, 5, length=n)
  y <- matrix(0, ncol=k, nrow=n)
  y <- y + beta0 + beta1*x + rnorm(n*k, sd=sigma)


 b0 <- numeric(k);  b1 <- numeric(k)
 for(i in 1:k){
   b <- coef(lm(y[ ,i] ~ x))
   b0[i] <- b[1]
   b1[i] <- b[2]
 }
 c(mean(b0),  mean(b1))


par(mfrow=c(1,2))
hist(b0, prob=TRUE, main="Empirical density of $\\hat{\\beta}_0$")
lines(beta0*c(1, 1),c(0, 300), col=2, lwd=2)
hist(b1, prob=TRUE, main="Empirical density of $\\hat{\\beta}_1$")
lines(beta1*c(1, 1),c(0, 300), col=2, lwd=2,)


# NOGET HELT ANDET DATA  x <- c(-2.00, -1.22, -0.44, 0.33, 1.11, 1.89, 2.67, 3.44, 4.22, 5.00)
#  y <- c(-2.93, 6.52, 4.85, 12.06, 20.46, 19.39, 21.50, 19.19, 23.65, 31.04)
#  mx <- mean(x)
#  my <- mean(y)
#  Sxx <- sum((x - mx)^2)
#  beta1 <- sum((x - mx)*(y - my))/Sxx;
#  beta0 <- my - beta1*mx; 
beta0 <- coef(fitStudents)[1]
beta1 <- coef(fitStudents)[2]
e <- y - (beta0 + beta1 * x)
n <- length(e)
sigma <- sqrt(sum(e^2) / (n - 2))
sigma.beta0 <- sqrt(sigma^2 * (1 / n + xbar^2 / Sxx))
sigma.beta1 <- sqrt(sigma^2 / Sxx) 
c(sigma, sigma.beta0, sigma.beta1)


set.seed(124)
n <- 10; k <- 500
beta0 <- 10;  beta1 <- 3; sigma <- 5
x <- seq(-2, 5, length=n)
Sxx <- (n-1)*var(x)
c(mean(x), Sxx)
y <- matrix(0, ncol=k, nrow=n)
y <- y + beta0 + beta1*x + rnorm(n*k,sd=sigma)


b0 <- numeric(k);  b1 <- numeric(k)
sigma <- numeric(k)
for(i in 1:k){
  fit <- lm(y[ ,i] ~ x)
  b <- coef(fit)
  b0[i] <- b[1]
  b1[i] <- b[2]
  sigma[i] <- summary(fit)$sigma
 }
 c(var(b0), var(b1), mean(sigma))


set.seed(124)
x <- seq(0, 1, length=10)
y <- 1 + x + rnorm(10)
# Fit the model (estimate parameter)
fit <- lm(y ~ x)
# Print summary of model fit
summary(fit)
# Residual standard deviation
sigma <- summary(fit)$sigma 
# Estimated standard deviation of parameters
summary(fit)$coefficients[ ,2]


set.seed(124)
k <- 500
sigma.beta <- matrix(nrow=k,ncol=2)
sigma <- numeric(k);
n <- seq(3, k+2)
for(i in 1:k){
  x <- seq(0,1,length=n[i])
  y <- 1+x+rnorm(n[i])
  fit <- lm(y ~ x)
  sigma[i] <- summary(fit)$sigma
  sigma.beta[i, ] <- summary(fit)$coefficients[ ,2]
}

Sxx<-n*(n+1)/12/(n-1)

par(mfrow=c(1,3), mar=c(3.25, 2.5, 0, 0.75), cex=0.85)
plot(n,sigma^2,pch=19,cex=0.5,xlab="$n$",ylab="$\\hat{\\sigma}^2$")
lines(c(0,max(n)),c(1,1),col=2,lwd=2)
plot(n,sigma.beta[ ,1],ylim=c(0,max(sigma.beta[ ,1])),pch=19,cex=0.5,xlab="$n$",ylab="$\\hat{\\sigma}_{\\beta_0}$")
lines(n,sqrt(1/n+1/4*1/Sxx),col=2,lwd=2)
plot(n,sigma.beta[ ,2],ylim=c(0,max(sigma.beta[ ,2])),pch=19,cex=0.5,xlab="$n$",ylab="$\\hat{\\sigma}_{\\beta_1}$")
lines(n,sqrt(1/Sxx),col=2,lwd=2)


qt(0.975,df=10-2)    


p.v0 <- 2 * (1 - pt(abs(-6.35), df=10-2))
p.v1 <- 2 * (1 - pt(abs(1.07), df=10-2))
c(p.v0,p.v1)


#  x <- c(-2.00, -1.22, -0.44, 0.33, 1.11, 1.89, 2.67, 3.44, 4.22, 5.00)
#  y <- c(-2.93, 6.52, 4.85, 12.06, 20.46, 19.39, 21.50, 19.19, 23.65, 31.04)
#  fit <- lm(y~x)
confint(fitStudents, level=0.95) 


predict(fitStudents, newdata=data.frame(x=200), interval="confidence",
        level=0.95) 

predict(fitStudents, newdata=data.frame(x=200), interval="prediction",
        level=0.95) 


set.seed(1234)

# The number of observations and the parameters
n <- 30
beta0 <- 10;  beta1 <- 3; sigma <- 0.5
# Generate some input values
x <- runif(n, -10, 10)
# Simulate output values
y <- beta0 + beta1*x + rnorm(n, sd=sigma)
# Fit a simple linear regression model to the sample
fit <- lm(y ~ x)

# The number of new observations
k <- 10000
# Generate k new input values
xnew <- runif(k, -10, 10)
# Calculate the prediction intervals for the new input values
PI <- predict(fit, newdata=data.frame(x=xnew), interval="pred")
# Simulate new output observations
ynew <- beta0 + beta1*xnew + rnorm(k, sd=sigma)
# Calculate the fraction of times the prediction interval covered the
# new observation
sum(ynew > PI[ ,"lwr"] & ynew < PI[ ,"upr"]) / k


# The number of simulated samples
k <- 10000
# Repeat the sampling k times
covered <- replicate(k, {
  # The number of observations and the parameters
  n <- 30
  beta0 <- 10;  beta1 <- 3; sigma <- 0.5
  # Generate some input values
  x <- runif(n, -10, 10)
  # Simulate output values
  y <- beta0 + beta1*x + rnorm(n, sd=sigma)
  # Fit a simple linear regression model to the sample
  fit <- lm(y ~ x)
  
  # Generate a new input value
  xnew <- runif(1, -10, 10)
  # The prediction interval for the new value
  PI <- predict(fit, newdata=data.frame(x=xnew), interval = "pred")
  # Simulate a single new observation
  ynew <- beta0 + beta1*xnew + rnorm(1, sd=sigma)
  # Check if the new observation was inside the interval
  ynew > PI[1,"lwr"] & ynew < PI[1,"upr"]
})
# The fraction of covered new observations
sum(covered)/k


# Data
X <- cbind(1, x)
n <- length(y)
# Parameter estimates and variance
beta     <- solve(t(X) %*% X) %*% t(X) %*% y
e        <- y - X %*% beta
s        <- sqrt(sum(e^2) / (n - 2))
Vbeta    <- s^2 * solve(t(X) %*% X)
sbeta    <- sqrt(diag(Vbeta))
T.stat   <- beta / sbeta
p.value  <- 2 * (1 - pt(abs(T.stat), df = n-2))
# Print the results
coef.mat <- cbind(beta, sbeta, T.stat, p.value); 
colnames(coef.mat) <- c("Estimates","Std.Error","t.value","p.value")
rownames(coef.mat) <- c("beta0", "beta1")
coef.mat;  s

# Prediction and confidence interval
xnew  <- matrix(c(1, 200), ncol=2)
ynew  <- xnew %*% beta
Vconf <- xnew %*% Vbeta %*% t(xnew)
Vpred <- Vconf + s^2
sqrt(c(Vconf, Vpred))


cor(x, y)^2


#  fit <- lm(y ~ x)
coef(fitStudents)[2]^2 * var(x) / var(y)


set.seed(124)
par(mfrow=c(1,2))
n <-100
x1 <- seq(1, 10, length=n)
y <- x1 + rnorm(n)
fit <- lm(y ~ x1)
qqnorm(fit$residuals, pch=19, cex=0.5)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19, cex=0.5,
     xlab="Fitted values ($\\hat{y}_i$)", ylab="Residuals ($e_i$)")


x1 <- seq(1, 10, length=n)
x2 <- seq(1, 10, length=n)^2
y <- x1 + 0.5 * x2 + rnorm(n)
fit <- lm(y ~ x1)
par(mfrow=c(1,2))
qqnorm(fit$residuals, pch=19, cex=0.5)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19, cex=0.5,
     xlab="Fitted values ($\\hat{y}_i$)", ylab="Residuals ($e_i$)")


x1 <- seq(4, 10, length=100)
y <- exp( 0.2 * x1 + rnorm(length(x1), sd=0.15))
fit <- lm(y ~ x1)
par(mfrow=c(1,2))
qqnorm(fit$residuals, pch=19, cex=0.5)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19, cex=0.5,
     xlab="Fitted values ($\\hat{y}_i$)", ylab="Residuals ($e_i$)")


y <- log(y)
fit <- lm(y ~ x1)
par(mfrow=c(1,2))
qqnorm(fit$residuals, pch=19, cex=0.5)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19, cex=0.5,
     xlab="Fitted values ($\\hat{y}_i$)", ylab="Residuals ($e_i$)")
# Read data
x1 <- c(0.083, 0.409, 0.515, 0.397, 0.223, 0.292, 0.584, 0.491, 0.923, 
   0.280, 0.772, 0.857, 0.758, 0.850, 0.409, 0.055, 0.578, 0.745, 
   0.886, 0.031)
x2 <- c(0.625, 0.604, 0.077, 0.414, 0.343, 0.202, 0.840, 0.266, 0.831, 
   0.385, 0.821, 0.308, 0.440, 0.865, 0.111, 0.970, 0.192, 0.939, 
   0.149, 0.318)
y <- c(0.156, 1.234, 0.490, 1.649, 0.500, 0.395, 1.452, 0.416, 1.390, 
  0.234, 1.574, 0.349, 1.287, 1.709, 0.323, 1.201, 1.210, 1.787, 
  0.591, 0.110)


# Parameter estimation
fit <- lm(y ~ x1 + x2)

# Summary of fit (parameter estimates, standard error, p-values, etc.)
summary(fit)


-0.118+c(-1,1)*qt(0.975,df=17)*0.212
#     0.827+c(-1,1)*qt(0.975,df=17)*0.304
#     1.239+c(-1,1)*qt(0.975,df=17)*0.293


confint(fit, level = 0.95)


# New data
Xnew <- data.frame(x1 = c(0.5, 1), x2 = c(0.5, 1))

# Prediction
pred <- predict(fit, newdata = Xnew, se = TRUE)
pred

# Average of the independent variables in the original data
# c(mean(x1), mean(x2))


# Confidence interval
predict(fit, newdata = Xnew, interval = "confidence", level = 0.95)
# Prediction interval
predict(fit, newdata = Xnew, interval = "prediction", level = 0.95)


s3d<-scatterplot3d(x=x1,y=x2,z=y,pch=19,box=FALSE,color="white",
                 angle=40,xlab=expression(x[1]),
          ylab=expression(x[2]),zlab="$\\hat{\\sigma}_{\\hat{y}}$",zlim=c(0,0.5),ylim=c(0,1),xlim=c(0,1))
my.lm <- lm(y ~ x1 + x2)
#s3d$plane3d(my.lm, lty.box = "solid",col=gray(0.5))

n<-50
x1.plot<-seq(0,1,length=n)
x2.plot<-seq(0,1,length=n)

x2.plot<-seq(0,1,length=5)
for(i in 1:length(x2.plot)){
  x2.tmp<-rep(x2.plot[i],n)
  se<-predict(my.lm,newdata=data.frame(x1=x1.plot,
                 x2=x2.tmp),se.fit=TRUE)$se.fit
  se2<-sqrt(se^2+summary(my.lm)$sigma^2)
  s3d$points(x=x1.plot,y=x2.tmp,z=se,type="l",col="blue")
  s3d$points(x=x1.plot,y=x2.tmp,z=se2,type="l",col="red")
}

x2.plot<-seq(0,1,length=n)
x1.plot<-seq(0,1,length=5)
for(i in 1:length(x2.plot)){
  x1.tmp<-rep(x1.plot[i],n)
  se<-predict(my.lm,newdata=data.frame(x1=x1.tmp,
                 x2=x2.plot),se.fit=TRUE)$se.fit
  se2<-sqrt(se^2+summary(my.lm)$sigma^2)
  s3d$points(x=x1.tmp,y=x2.plot,z=se,type="l",col="blue")
  s3d$points(x=x1.tmp,y=x2.plot,z=se2,type="l",col="red")
}


# Confidence interval
predict(fit, newdata=Xnew, interval="confidence", level=1-alpha)

# Prediction interval
predict(fit, newdata=Xnew, interval="prediction", level=1-alpha)


set.seed(1234)
n <- 200
x <- runif(n)
y <- sin(pi*x) + rnorm(n,sd=0.1)


fit1 <- lm(y ~ x)
confint(fit1) 


x1 <- x; x2 <- x^2
fit2 <- lm(y ~ x1 + x2)
confint(fit2)


par(mfrow=c(1,2))
plot(fit1$fitted.values,fit1$residuals,pch=19,cex=0.5, xlab="fit1\\$fitted.values", ylab="fit1\\$residuals")
plot(fit2$fitted.values,fit2$residuals,pch=19,cex=0.5, xlab="fit2\\$fitted.values", ylab="fit2\\$residuals")


xplot <- seq(0,1,by=0.01)
plot(x, y, pch=19, cex=0.5, xlab="$x$", ylab="$y$")
lines(xplot,predict(fit1,newdata=data.frame(x=xplot)),col="green",lwd=2)
lines(xplot,predict(fit2,newdata=data.frame(x1=xplot,x2=xplot^2)),col="red",lwd=2)


set.seed(200)
n <- 100
x1 <- runif(n)
x2 <- x1 + rnorm(n, sd=0.01)
y <- x1 + x2 + rnorm(n, sd=0.5)
par(mfrow=c(1,2))
plot(x1, y, pch=19, cex=0.5, xlab=expression(x[1]))
plot(x2, y, pch=19, cex=0.5, xlab=expression(x[2]))



confint(lm(y ~ x1 + x2)) 


summary(lm(y ~ x2)) 


set.seed(200)
par(mfrow=c(1,2))
n <- 100
x1 <- runif(n)
x2 <- runif(n)
y <- x1 + 2*x2^2 + rnorm(n,sd=0.125)
plot(x1, y, pch=19, cex=0.5)
plot(x2, y, pch=19, cex=0.5)


par(mfrow=c(1,3), mar=c(4,3,0.2,0.2), cex=0.8)
fit <- lm(y ~ x1 + x2)
plot(fitted.values(fit), residuals(fit), pch=19, cex=0.7)
plot(x1, residuals(fit), pch=19, cex=0.7)
plot(x2, residuals(fit), pch=19, cex=0.7)


par(mfrow=c(1,3), mar=c(4,3,0.2,0.2), cex=0.8)
x3 <- x2^2
fit <- lm(y ~ x1 + x2 + x3)
plot(fitted.values(fit), residuals(fit), pch=19, cex=0.7)
plot(x1, residuals(fit), pch=19, cex=0.7)
plot(x2, residuals(fit), pch=19, cex=0.7)


summary(fit)
par(mfrow=c(2,3), mar=c(3,3,1,1)+0.1)
par(cex=0.8)

plotit <- function(n, p){
    plot(0:n, dbinom(0:n, n, p), type="h", xlab="", ylab="",
         main=paste0("$np = ",n*p,"$, $n(1-p) = ",n*(1-p),"$"))
}

plotit(n=6, p=0.5)
plotit(n=9, p=1/3)
plotit(n=18, p=1/3)
plotit(n=30, p=1/3)
plotit(n=45, p=1/3)
plotit(n=100, p=0.3)



# Testing the probability = 0.5 with a two-sided alternative
# We have observed 518 out of 1154
# Do it without continuity corrections

prop.test(x=518, n=1154, p = 0.5, correct = FALSE)


# Testing that the probabilities for the two groups are equal
# Calculating 99% confindece interval
prop.test(x=c(23,35), n=c(57,167), correct=FALSE, conf.level=0.99)


# Reading the data into R
pill.study <- matrix(c(23, 35, 34, 132), ncol = 2, byrow = TRUE)
rownames(pill.study) <- c("Blood Clot", "No Clot")
colnames(pill.study) <- c("Pill", "No pill")
pill.study

# Chi^2 test for tesing that the distribution for the two groups are equal
chisq.test(pill.study, correct = FALSE)

# If we want the expected numbers, then store the result in a variable
chi <- chisq.test(pill.study, correct = FALSE)

# In the result the expected values can be found
chi$expected


# Reading the data into R
poll <- matrix(c(79, 91, 93, 84, 66, 60, 37, 43, 47), ncol = 3, 
               byrow = TRUE)
colnames(poll) <- c("4 weeks", "2 weeks", "1 week")
rownames(poll) <- c("Cand1", "Cand2", "Undecided")

# Column percentages
colpercent <- prop.table(poll, 2)
colpercent

barplot(t(colpercent), beside = TRUE, col = 2:4, las = 1, 
        ylab = "Percent each week", xlab = "Candidate", 
        main = "Distribution of Votes")
legend( legend = colnames(poll), fill = 2:4,"topright", cex = 0.7)


# Testing same distribution in the three populations
chi <- chisq.test(poll, correct = FALSE)
chi

# Expected values
chi$expected


# Reading the data into R
results <- matrix(c(23, 60, 29, 28, 79, 60, 9, 49, 63), ncol = 3, 
                  byrow = TRUE)
colnames(results) <- c("MathBad", "MathAve", "MathGood")
rownames(results) <- c("EngBad", "EngAve", "EngGood")


# Percentages
prop.table(results)

# Row totals
margin.table(results, 1)

# Column totals
margin.table(results, 2)


# Testing independence between english and maths results
chi <- chisq.test(results, correct = FALSE)
chi

# Expected values
chi$expected
y <- c(2.8, 3.6, 3.4, 2.3,
     5.5, 6.3, 6.1, 5.7,
     5.8, 8.3, 6.9, 6.1)
treatm <- factor(c(1, 1, 1, 1,
                 2, 2, 2, 2,
                 3, 3, 3, 3))
plot(as.numeric(treatm), y, pch=19, xlim=c(0.5, 3.5), axes=FALSE, xlab="Treatment", ylab="", cex=0.7) 
lines(c(0,4),c(1,1)*mean(y),col=2)
lines(c(1,1),c(mean(y),mean(y[treatm==1])),col="blue")
lines(c(2,2),c(mean(y),mean(y[treatm==2])),col="blue")
lines(c(3,3),c(mean(y),mean(y[treatm==3])),col="blue")
box()
axis(1,at=c(1,2,3),labels=c("1","2","3"))
axis(2,at=c(mean(y),mean(y[treatm==1]),mean(y[treatm==2]),mean(y[treatm==3])),labels=c("$\\hat{\\mu}$","$\\hat{\\mu}_{1}$","$\\hat{\\mu}_{2}$","$\\hat{\\mu}_{3}$"), las=1)
points(c(1,2,3),c(mean(y[treatm==1]),mean(y[treatm==2]),mean(y[treatm==3])),pch=19,col=2,cex=1.2)
text(1,(mean(y)+mean(y[treatm==1]))/2,labels="$\\hat{\\alpha}_1$",adj=c(-1,0),col="blue")
text(2,(mean(y)+mean(y[treatm==2]))/2,labels="$\\hat{\\alpha}_2$",adj=c(-1,0),col="blue")
text(3,(mean(y)+mean(y[treatm==3]))/2,labels="$\\hat{\\alpha}_3$",adj=c(-1,0),col="blue")
text(1,min(y),labels="$y_{1,j}$",adj=c(-1,0),col=1)


par(mgp=c(5,1,0))
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))
plot(treatm,y)


mu <- mean(y)
muis <- tapply(y, treatm, mean)
alpha <- muis - mu
mu
muis
alpha


tapply(y, treatm, var)


SST <- sum((y - mu)^2)
SSE <- sum((y[treatm==1] - muis[1])^2)+
       sum((y[treatm==2] - muis[2])^2)+
       sum((y[treatm==3] - muis[3])^2)
SSTr <- 4 * sum(alpha^2)
c(SST, SSE, SSTr)


vars <- tapply(y, treatm, var)
(12-3)*mean(vars)


x<-seq(0,5,by=0.01)
plot(x,df(x,df1=2,df2=9),type="l", xlab="$x$", ylab="pdf")
lines(x,df(x,df1=4,df2=9),col=2)


F <- (SSTr/(3 - 1)/(SSE/(12 - 3)))
pv <- 1 - pf(F, df1 = 3 - 1, df2 = 12 - 3)
c(F , pv)


anova(lm(y ~ treatm))


muis[1] - muis[2] + c(-1, 1) *  
  qt(0.975, df = 12 - 3) * sqrt(SSE/(12 - 3) * (1/4 + 1/4))


tobs <- (muis[1] - muis[2])/sqrt(SSE/(12 - 3) * (1/4 + 1/4))
2 * (1 - pt(abs(tobs), 9))


alphaBonf <- 0.05/3 
# A-B
alpha[1] - alpha[2] + c(-1, 1) *  
  qt(1-alphaBonf/2, df = 12 - 3) * sqrt(SSE/(12 - 3) * (1/4 + 1/4))
# A-C
alpha[1] - alpha[3] + c(-1, 1) * 
  qt(1-alphaBonf/2, df = 12 - 3) * sqrt(SSE/(12 - 3) * (1/4 + 1/4))
# B-C
alpha[2] - alpha[3] + c(-1, 1) * 
  qt(1-alphaBonf/2, df = 12 - 3) * sqrt(SSE/(12 - 3) * (1/4 + 1/4))


c(qt(1 - alphaBonf/2, 9), qt(0.975, 9))


residuals <- lm(y ~ treatm)$residuals
qqnorm(residuals)
qqline(residuals)
residuals


D <- data.frame(
  strength=c(44.6, 52.8, 53.1, 51.5, 48.2, 50.5, 58.3, 50.0, 53.7, 40.8,
             46.3, 55.4, 54.4, 50.5, 44.5, 48.5, 57.4, 55.3, 54.4, 43.9,
             45.2, 58.1, 50.6, 47.5, 45.9, 52.3, 54.6, 53.4, 47.8, 42.5),
  plastictype=factor(rep(1:5, 6))
)
plot(D$plastictype, D$strength, xlab="Plastictype", ylab="Strength")
fit <- lm(strength ~ plastictype, data=D)
anova(fit)


library(xtable)
print(xtable(anova(fit)))


qqnorm(fit$residuals)
qqline(fit$residuals)


set.seed(138)
library(MESS)
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}
wallyplot(fit$residuals, FUN=qqwrap, ylim=c(-3,3))


tapply(D$strength, D$plastictype, mean)


LSD_0.005 <- qt(0.9975, 25) * sqrt(2*6.74/6)
LSD_0.005


y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))
block <- factor(c(1, 2, 3, 4, 
                  1, 2, 3, 4,
                  1, 2, 3, 4))


mu <- mean(y)
alpha <- tapply(y, treatm, mean) - mu
beta <- tapply(y, block, mean)  - mu 
mu
alpha
beta


SSBl <- 3 * sum(beta^2)
SSE <- SST - SSTr - SSBl
c(SST, SSE, SSTr, SSBl)                    


# Test statistics
Ftr <- SSTr / (3-1) / ( SSE / ((3-1) * (4-1)))
Fbl <- SSBl / (4-1) / ( SSE / ((3-1) * (4-1)))
# p-values
pv.tr <- 1 - pf(Ftr, df1=3-1, df2=(3-1)*(4-1))
pv.bl <- 1 - pf(Fbl, df1=4-1, df2=(3-1)*(4-1))
c(Ftr, Fbl)
c(pv.tr, pv.bl)


D <- data.frame(treatm, block, y)
fit <- lm(y ~ treatm + block, data=D)
anova(fit)


muis[1] - muis[2] + c(-1,1) * qt(0.975, df=(4-1)*(3-1)) * 
  sqrt(SSE/((4-1)*(3-1)) * (1/4+1/4))


tobs <- (muis[1] - muis[2])/sqrt(SSE/6 * (1/4 + 1/4))
2 * (1 - pt(abs(tobs), df=6))


alphaBonf <- 0.05/3 
# A vs. B
alpha[1] - alpha[2] + c(-1, 1) *  
  qt(1-alphaBonf/2, df = 6) * sqrt(SSE/6 * (1/4 + 1/4))
# A vs. C
alpha[1] - alpha[3] + c(-1, 1) * 
  qt(1-alphaBonf/2, df = 6) * sqrt(SSE/6 * (1/4 + 1/4))
# B vs. C
alpha[2] - alpha[3] + c(-1, 1) * 
  qt(1-alphaBonf/2, df = 6) * sqrt(SSE/6 * (1/4 + 1/4))


qqnorm(fit$residuals)
qqline(fit$residuals)
fit$residuals


par(mfrow=c(1,2))
plot(D$treatm, fit$residuals, xlab="Treatment", ylab="Residuals")
plot(D$block, fit$residuals, xlab="Block", ylab="Residuals")


# Collecting the data in a data frame
D <- data.frame(
  y=c(22.5, 24.3, 24.9, 22.4,
      21.5, 21.3, 23.9, 18.4,
      22.2, 21.9, 21.7, 17.9),
  car=factor(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)),
  tire=factor(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))
)

par(mfrow=c(1,2))
plot(D$tire, D$y, xlab="Tire", ylab="y")
plot(D$car, D$y, xlab="Car", ylab="y")


fit <- lm(y ~ car + tire, data=D)

anova(fit)


qqnorm(fit$residuals)
qqline(fit$residuals)


par(mfrow=c(1,2))
plot(D$car, fit$residuals, xlab="Car", ylab="Residuals")
plot(D$tire, fit$residuals, xlab="Tire", ylab="Residuals")


tapply(D$y, D$tire, mean)


LSD_bonf <- qt(1-0.05/6, df=6) * sqrt(2*1.19/4)
LSD_bonf
