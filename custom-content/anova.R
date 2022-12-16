# -----------------------------------------------------------------------------
#   ANOVA
# -----------------------------------------------------------------------------

y <- c(1.89, 2.35, 1.68, 2.11, 3.15, 2.16, 2.40, 2.59, 1.54, 2.02, 2.01, 2.11)
grp <- c(rep("1", 4), rep("2", 4), rep("3", 4))
fit <- lm(y ~ grp)
anova(fit)
# if you want SST just sum the sq

## Fra Hansen ---------------

### 8) Anova ----
# Pairwise CI####
yi <- 4.0483
yj <- 5.5517
alpha <- 0.05
sse <- 4.1060
n <- 18
k <- 3
ni <- 6
nj <- 6
df <- n - k
yi - yj + c(-1, 1) *
  qt(1 - alpha / 2, df) *
  sqrt(SSE / (n - k) * (1 / ni + 1 / nj))

# F-størrelsen (test størrelsen)#####
F = (SSTr / (k - 1)) / (SSE / (n - k)); F
#OR
MSTr <- SSTr / (k - 1)
F = MSTr / MSE; F

# SST - Totalafvigelsessum ####
SST <- SSE + SSTr; SST

# SSE ####
SSE <- MSE * (n - k)
# MSTr  #####
MSTr <- SSTr / (k - 1)
# Kritisk værdi####
#1-way ANOVA critical value
alpha <- 0.05
k <- 7
n <- 7 * 5
qf(1 - alpha, k - 1, n - k)
# F - Teststørrelse#####
F <- (SSTr / (df1) / (SSE / (df2))); F
# P-værdi#####
pv <- 1 - pf(F, df1 <- df1, df2 <- df2); pv
### 2-way ANOVA -----
df1 <- 4
df2 <- 7
df3 <- 28
mstr <- 1.82
msbl <- 11.07
mse <- 3.18
ftr <- mstr / mse
fbl <- msbl / mse
pvtr <- 1 - pf(ftr, df1, df3); pvtr
pvbl <- 1 - pf(fbl, df2, df3); pvbl
