# Manganese concentrations
x <- c(0, 0, 2, 2, 4, 4, 6, 6, 8, 8, 10, 10)
# ICP-AES values
y <- c(114, 14, 870, 1141, 2087, 2212, 3353, 2633, 3970, 4299, 4950, 5207)

# Wished x value
x_wish <- 5
# Confidence (percent)
conf <- 95

fit <- lm(y ~ x)
fit

# gives the conf interval for the wished x value in the data frame
predict(fit, newdata = data.frame(x = x_wish), interval = "prediction", level = conf/100)

# summary, can give you the p value for example, to look at the null hypothesis
summary(fit)


# -----------------------------
#   From Hansen
# -----------------------------

# 5) Linear regression ----
### Parameter confidence intervals ----
x <- c(0, 25, 50, 75, 100)
y <- c(14, 38, 54, 76, 95)
#Lave framwork
D <- data.frame(x=x, y=y)
fit <- lm(y ~ x, data=D)
summary(fit)
#læs summary og indsæt:
alpha <- 0.05
b1_mean <- 0.80000
b1_sd <- 0.02444
df <- 7
b1_mean+c(-1, 1)*qt(1-alpha/2, df)*b1_sd

### Confidence & Prediction interval ----
x <- c(0, 25, 50, 75, 100)
y <- c(14, 38, 54, 76, 95)
#Lave framwork
D <- data.frame(x=x, y=y)
fit <- lm(y ~ x, data=D)
summary(fit)
## finde det automatisk
## Indsæt data punkt:
predict(fit, newdata=data.frame(x=80), interval="confidence",
        level=0.95)

#læs summary og indsæt:
alpha <- 0.05
b1_mean <- 0.80000
b0_mean <- 15.40000
sd <- 1.932
df <- 3
n <- length(x)
x_punkt <- 80
x_mean <- 50
sx <- 39.52847
sxx <- (n-1)*sx^2
#Confidence interval for line
(b0_mean + b1_mean*x_punkt) + c(-1, 1)*qt(1-alpha/2, df)*sd*sqrt((1/n)+(x_punkt-x_mean)^2/sxx)
#Interval for new point prediction:
(b0_mean + b1_mean*x_punkt) + c(-1, 1)*qt(1-alpha/2, df)*sd*sqrt(1+(1/n)+(x_punkt-x_mean)^2/sxx)