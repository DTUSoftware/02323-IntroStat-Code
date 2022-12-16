# Manganese concentrations
x <- c(0, 0, 2, 2, 4, 4, 6, 6, 8, 8, 10, 10)
# ICP-AES values
y <- c(114, 14, 870, 1141, 2087, 2212, 3353, 2633, 3970, 4299, 4950, 5207)

fit <- lm(y ~ x)
fit

# gives the conf interval for the wished x value in the data frame
predict(fit, newdata = data.frame(x = 5), interval = "prediction",
        level = 0.95)

# summary, can give you the p value for example, to look at the null hypothesis
summary(fit)