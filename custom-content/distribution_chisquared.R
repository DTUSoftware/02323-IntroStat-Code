# -----------------------------------------------------------------------------
#   CHISQUARE DISTRIBUTION
# -----------------------------------------------------------------------------

#  Insert value
n <- 0
s <- 0
x <- 0
alpha <- 0.01

# Calculate values
df <- n - 1
alpha_1 <- alpha/2
alpha_2 <- 1 - alpha_1

# Calculate Result
sqrt(df*s^2 / qchisq(alpha_1, df))
sqrt(df*s^2 / qchisq(alpha_2, df))
