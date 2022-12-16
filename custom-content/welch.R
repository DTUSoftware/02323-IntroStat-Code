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
