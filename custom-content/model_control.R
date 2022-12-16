# ---------------------
#   From Hansen
# ---------------------

# 3) model control ----
### QQ-plot ----
qqnorm(x)
qqline(x)
par(mfrow <- c(3, 3))
for (i in 1:9) {
  xr <- rnorm(9)
  qqnorm(xr, main = "")
  qqline(xr)
}
### Histogram ----
x <- c(3003, 3005, 2997, 3006, 2999, 2998, 3007, 3005, 3001)
hist(x, freq = F, col <- 4)
xp <- seq(2996, 3008, 0.1)
lines(xp, dnorm(xp, mean(x), sd(x)), lwd <- 2)
### Fordelingsfunktion ----
plot(ecdf(x), verticals <- TRUE)
xp <- seq(0.9 * min(x), 1.1 * max(x), length.out <- 100)
lines(xp, pnorm(xp, mean(x), sd(x)))