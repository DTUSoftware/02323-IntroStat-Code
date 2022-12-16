# -----------------------------------------------------------------------------
#   HYPOTHESIS TESTING
# -----------------------------------------------------------------------------

# 1. Insert Values

x = 0
mu = 0
s = 0
n = 0

# 2. Calculate T_obs value & df

t_obs = (x-mu)/(s/sqrt(n))
df = n - 1

# 3. Calculate p

P <- 2*(1-pt(t_obs, df))
P

# if P > 0.05 the hypothes is rejected
# if P < 0.05 the hypothes is NOT rejected

# --------------------------------------------------------------
# Nielsen
# --------------------------------------------------------------

n1 <- 2000
x1 <- 1920
n2 <- 2024
x2 <- 1801
x <- c(x1, x2)
n <- c(n1, n2)
alpha <- 0.01

# proportion test
prop.test(x, n, conf.level = 1 - alpha, correct = FALSE)

t.test(x, n, conf.level = 1 - alpha)