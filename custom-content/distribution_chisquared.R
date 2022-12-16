# -----------------------------------------------------------------------------
#   CHISQUARE DISTRIBUTION
# -----------------------------------------------------------------------------

## 1D

#  Insert values
n <- 0
s <- 0
x <- 0
alpha <- 0.01

# Calculate values
df <- n - 1
alpha_1 <- alpha / 2
alpha_2 <- 1 - alpha_1

# Calculate Result
sqrt(df * s^2 / qchisq(alpha_1, df))
sqrt(df * s^2 / qchisq(alpha_2, df))


## 2D

#  Insert values
n1 <- 3
n2 <- 2
alpha <- 0.01

# Calculate values
df2d <- (n1 - 1) * (n2 - 1)

# Calculate Result
qchisq(1 - alpha, df2d)


