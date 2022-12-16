# ---------------------
#   From Hansen
# ---------------------

# 6) Multi linear regression -----
### p value ----
tobs <- 13.42
df <- 25
2*(1-(pt(abs(tobs), df)))
### Summary på data + konfidence interval ----
D <- data.frame(
  x1=c(0.58, 0.86, 0.29, 0.20, 0.56, 0.28, 0.08, 0.41, 0.22,
       0.35, 0.59, 0.22, 0.26, 0.12, 0.65, 0.70, 0.30, 0.70,
       0.39, 0.72, 0.45, 0.81, 0.04, 0.20, 0.95),
  x2=c(0.71, 0.13, 0.79, 0.20, 0.56, 0.92, 0.01, 0.60, 0.70,
       0.73, 0.13, 0.96, 0.27, 0.21, 0.88, 0.30, 0.15, 0.09,
       0.17, 0.25, 0.30, 0.32, 0.82, 0.98, 0.00),
  y=c(1.45, 1.93, 0.81, 0.61, 1.55, 0.95, 0.45, 1.14, 0.74,
      0.98, 1.41, 0.81, 0.89, 0.68, 1.39, 1.53, 0.91, 1.49,
      1.38, 1.73, 1.11, 1.68, 0.66, 0.69, 1.98))
fit <- lm(y ~ x1 + x2, data=D)
summary(fit)
#Confidence interval
confint(fit)

### Residual analysis ----
#definer data som fit.
par(mfrow=c(1, 2))
qqnorm(fit$residuals, pch=19)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19,
     xlab="Fitted.values", ylab="Residuals")
plot(D$x1, fit$residuals, xlab="x1", ylab="Residuals")
plot(D$x2, fit$residuals, xlab="x2", ylab="Residuals")

### Plot confidence band and prediction band ----
x1new <- seq(0, 1, by=0.01)
pred <- predict(fit, newdata=data.frame(x1=x1new),
                interval="prediction")
conf <- predict(fit, newdata=data.frame(x1=x1new),
                interval="confidence")
plot(x1new, pred[, "fit"], type="l", ylim=c(0.1, 2.4),
     xlab="x1", ylab="Prediction")
lines(x1new, conf[, "lwr"], col="green", lty=2)
lines(x1new, conf[, "upr"], col="green", lty=2)
lines(x1new, pred[, "lwr"], col="red", lty=2)
lines(x1new, pred[, "upr"], col="red", lty=2)
legend("topleft", c("Prediction", "Confidence band", "Prediction band"),
       lty=c(1, 2, 2), col=c(1, 3, 2), cex=0.7)

### Plot data ----
par(mfrow=c(1, 2))
plot(D$x1, D$y, xlab="x1", ylab="y")
plot(D$x2, D$y, xlab="x1", ylab="y")