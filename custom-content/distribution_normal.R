# -----------------------------------------------------------------------------
#   PNORM GUIDE
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
1-pnorm(q, x, s)

# ------------------------------------------------------------------------------
#   ROBOT OVERLORD
# ------------------------------------------------------------------------------

# Generate normal random variables
normal_data <- rnorm(n = 100, mean = 0, sd = 1)

# Summarize the normal data
summary(normal_data)