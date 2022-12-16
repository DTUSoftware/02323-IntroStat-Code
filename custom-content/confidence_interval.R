# -----------------------------------------------------------------------------
#   CONFIDENCE INTERVALS
# -----------------------------------------------------------------------------

data <- c(22.3, 11.8, 14.8, 13.4, 14.8, 15.9, 6.6, 9.6, 16.5)
conf_interval <- 99.9
t.test(data, conf.level = conf_interval/100)

# To do it from the standard deviation, sample mean and sample size
# n is sample size
n <- 50
# x is sample mean
x <- 3505.7
# sample standard deviation is
s <- 467.9
# the confidence interval thingy
conf_interval <- 95
alpha <- (100-conf_interval)/100
x - qt(1-alpha/2,n-1)*s/sqrt(n)
x + qt(1-alpha/2,n-1)*s/sqrt(n)



# 1. Insert Variables

n <- 0                         # Number of Observations
x <- 0                         # Sample Mean
s <- 0                         # Standard Deviation

# 2. Insert Variables

alpha <- 0.05                   # Significane Value
t <- qt(1 - alpha/2, n-1)     # T-value

# 3. Calculate Interval

x - t*s/sqrt(n)                # Lower value
x + t*s/sqrt(n)                # Higher value

# ---------------------
#   From Hansen
# ---------------------

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