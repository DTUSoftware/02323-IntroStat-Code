# Number of values
k <- 1000000
x1 <- rnorm(k,0,1)
x2 <- rnorm(k,0,1)
sd(exp(x1) + x2^4 + x1*x2)