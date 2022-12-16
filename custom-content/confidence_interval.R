# -----------------------------------------------------------------------------
#   CONFIDENCE INTERVALS
# -----------------------------------------------------------------------------

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
