# -----------------------------------------------------------------------------
#   T-TEST
# -----------------------------------------------------------------------------

# FROM HANSEN

### 2-sample variance & sd ----
# 3) t-test typer ----
### Standard 1-sample test ----
t.test(x, mu = 3000, conf.level = 0.95)

#Test størrelse
mean <- 180.05
h0 <- 180
sd <- 0.0959
n <- 16
T <- (mean - h0) / (sd / sqrt(n))
T
df <- n - 1
#p-værdi:
2 * (1 - pt(abs(T), df))

### Kritisk værdi####
n <- length(x)
mu0 <- 180
alpha <- 0.01
df <- n - 1

c(-1, 1) * qt(1 - alpha / 2, df = df)

### Paired 2-sample test ----
### Welch 2-sample test ----
x1 <- c(1, 2, 1, 3, 2, 1, 2, 3, 1, 1)
x2 <- c(3, 4, 2, 4, 2, 3, 2, 4, 3, 2)
t.test(x1, x2)

mean1 <- 5.97
mean2 <- 8.25
h0 <- 0
sd1 <- sqrt(23)
sd2 <- sqrt(21)
n1 <- 51
n2 <- 68
T <- ((mean1 - mean2) - h0) / sqrt(sd1^2 / n1 + sd2^2 / n2)
T
df <- (sd1^2 / n1 + sd2^2 / n2)^2 / ((sd1^2 / n1)^2 / (n1 - 1) + (sd2^2 / n2)^2 / (n2 - 1))
#p-værdi:
2 * (1 - pt(abs(T), df))
