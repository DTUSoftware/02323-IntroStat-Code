rm(list = ls())
# 3) Konfidens Interval ----
### 1-sample mean ----
t.test(x, mu = 180, conf.level = 0.99)

x_bar <- 39
alpha <- 0.05
sd <- 2.6458
n <- 7
x_bar + c(-1, 1) * qt(1 - alpha / 2, n - 1) * sd / sqrt(n)
### 1-sample variance & sd ----
## Variance
x_bar <- 39
alpha <- 0.05
s <- 2.6458
n <- 7
((n - 1) * s^2) / qchisq(1 - alpha / 2, n - 1)
((n - 1) * s^2) / qchisq(alpha / 2, n - 1)
##Standard Deviation
sqrt(((n - 1) * s^2) / qchisq(1 - alpha / 2, n - 1))
sqrt(((n - 1) * s^2) / qchisq(alpha / 2, n - 1))

### 2-sample mean ----
x_bar1 <- 122.4
x_bar2 <- 145.9
Sp1 <- 30.5
Sp2 <- 22.3
n_1 <- 15
n_2 <- 10
alpha <- 0.05

#Finder antal frihedsgrader
v <- ((((Sp1^2) / n_1) + ((Sp2^2) / n_2))^2) / (((((Sp1^2) / n_1)^2) / (n_1 - 1)) + (((Sp2^2) / n_2)^2) / (n_2 - 1))
v
#Finder KI:
(x_bar1 - x_bar2) + c(-1, 1) *
  qt(1 - (alpha / 2), v) *
  sqrt(((Sp1^2) / n_1) + ((Sp2^2) / n_2))


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
# 3) Power / sample size ----
### 1-sample power/ sample size ----
#find power
power.t.test(n = 50, delta = 4, sd = 12.21, sig.level = 0.05,
             type = "one.sample")
#find sample size
power.t.test(power = 0.8, delta = 4, sd = 12.21, sig.level = 0.05,
             type = "one.sample")

#1. metode
alpha <- 0.05
mean <- 178
sd <- 12.21
#me er afvigelsen i en retning, altså halvdelen af bredden
me <- 3
n <- ((qnorm(1 - alpha / 2) * sd) / me)^2
n

#2. metode power
sd <- 12.21
#tast ind forskellen der ønskes at findes eller angiv means.
meandiff <- 4
#beta er power. hvis det er 80% indtastes 0.2
beta <- 0.2
alpha <- 0.05
n <- (sd * (qnorm(1 - beta) + qnorm(1 - alpha / 2)) / (meandiff))^2
n

### 2-sample power/ sample size ----
# n er antal i hver gruppe.
#find power
power.t.test(n = 10, delta = 2, sd = 1, sig.level = 0.05)
#find sample size
power.t.test(power = 0.8, delta = 4, sd = 12.21, sig.level = 0.05)
#finde effekt / afvigelse (kaldet delta (forskel))
power.t.test(n = 50, power = 0.80, sd = 12.21, sig.level = 0.05)

# 4) Error defining/Error propagation  ----
### Theoretical derivation ----
rm(list = ls())
# indsæt formel i{}:
f <- function(x, y) { indsætformel}
# indsæt formel:
fd <- expression(indsæt formel)
dx <- D(fd, 'x')
dy <- D(fd, 'y')
#Finder værdien af et datapunkt:
x <- 240.48
y <- 9.987
f(x, y)
#Finder usikkerheden:
usikker_x <- 0.03
usikker_y <- 0.002
#Indsæt dx og dy definitioner med tal i:
dx
dy
sqrt((dx indsæt)^2*(usikker_x)^2+(dy indsæt)^2*(usikker_y)^2)

### Simulation ----
set.seed(28973)
k <- 10000
#Ændre variable navne og tal
Vs <- rnorm(k, 9.987, sd <- 0.002)
Ts <- rnorm(k, 289.12, sd <- 0.02)
#Indsæt formel
Ps <- 8.31*Ts/Vs
sd(Ps)

# 4) Bootstraping ----

###Paramatic 1-sample (kender fordelingen) ----

##exponentiel fordeling
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
rate <- 1/26.08
simsamples <- replicate(k, rexp(n, rate))
#indsæt værdi der ønskes:
sim <- apply(simsamples, 2, mean)
quantile(sim, c(0.025, 0.975))

##Normal fordeling
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
mean <- mean(x)
SD <- sd(x)
simsamples <- replicate(k, rnorm(n, mean, SD))
#indsæt værdi der ønskes:
sim <- apply(simsamples, 2, q1)
quantile(sim, c(0.025, 0.975))

##Log-normal fordeling
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
mean <- mean(log(x))
SD <- sd(log(x))
simsamples <- replicate(k, rlnorm(n, mean, SD))
#indsæt værdi der ønskes:
sim <- apply(simsamples, 2, mean)
quantile(sim, c(0.025, 0.975))

###Paramatic 2-sample ----

##Exponential fordeling
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(1, 2, 1, 3, 2,1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2,3, 2, 4, 3, 2)
n1 <- length(x)
n2 < length(y)
k <- 10000
rate1 <-
rate2 <-
simsamplesx <- replicate(k, rexp(n, rate1))
simsamplesy <- replicate(k, rexp(n, rate2))
#indsæt værdi der ønskes:
sim <- apply(simsamplesx, 2, mean) - apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))

##Normal fordeling
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(1, 2, 1, 3, 2,1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2,3, 2, 4, 3, 2)
n1 <- length(x)
n2 < length(y)
k <- 10000
simsamplesx <- replicate(k, rnorm(n, mean(x), sd(x)))
simsamplesy <- replicate(k, rnorm(n, mean(y), sd(y)))
#indsæt værdi der ønskes:
sim <- apply(simsamplesx, 2, mean) - apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))

##Log-normal fordeling
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(1, 2, 1, 3, 2,1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2,3, 2, 4, 3, 2)
n1 <- length(x)
n2 < length(y)
k <- 10000
simsamplesx <- replicate(k, rnorm(n, mean(log(x)), sd(log(x))))
simsamplesy <- replicate(k, rnorm(n, mean(log(y)), sd(log(y))))
#indsæt værdi der ønskes:
sim <- apply(simsamplesx, 2, mean) - apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))

###Non-Paramatic 1-sample (kender ikke fordelingen) -----
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
38.43, 38.31, 38.32, 38.48, 38.50)
n <- length(x)
k <- 10000
simsamples <- replicate(k, sample(x, replace=TRUE))
#indsæt værdi der ønskes(mean, median, etc):
sim <- apply(simsamples, 2, q1)
quantile(sim, c(0.025, 0.975))

###Non-Paramatic 2-sample (kender ikke fordelingen) -----
q1 <- function(x){ quantile(x, 0.25)}
set.seed(6287)
x <- c(1, 2, 1, 3, 2,1, 2, 3, 1, 1)
y <- c(3, 4, 2, 4, 2,3, 2, 4, 3, 2)
k <- 10000
simsamplesx <- replicate(k, sample(x, replace=TRUE))
simsamplesy <- replicate(k, sample(y, replace=TRUE))
#indsæt værdi der ønskes(mean, median, etc):
sim <- apply(simsamplesx, 2, mean)- apply(simsamplesy, 2, mean)
quantile(sim, c(0.025, 0.975))

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
# 7) proportion ----
#1. find ud af om det er 1-sample, eller to grupper/fordelinger der sammenlignes.
#Eller flere elementer der skal sammenlignes.
#Hvis det er 1-sample er man interesseret i en nulhypotese omkring en procentvis fordeling.
# er det 2-sample eller mange sample så tester man om fordelingerne er ens, ud fra en antagelse om at de er.
#Ved multi samle anvendes chi^2 test, ved 1-sample eller 2-sample kan prop.test anvendes.

### automatisk test ----
prop.test(x=c(6, 12), n=c(50, 50), correct <- FALSE)
prop.test(36, 200, correct=FALSE, conf.level=0.99)
### 1- sample CI, Z, p-værdi for fordeling ----
x <- 44
n <- 100
#fordelingen der testes efter
p <- x/n
alpha <- 0.05
#Z - teststørrelse
z_obs <- (x-n*p)/sqrt(n*p*(1-p))
#kritisk værdi:
qnorm(1-alpha/2)
#p-værdi
2*pnorm(z_obs)
#CI
p+c(-1, 1)*qnorm(1-alpha/2)*sqrt(p*(1-p)/n)

#Alternativt
n <- 2333+2536
p <- 2333/n
sigma_p <- sqrt(p*(1-p)/n)
l_limit <- p-1.96*sigma_p
u_limit <- p+1.96*sigma_p

### 2x2 forskel CI, Z og p-værdi ----
x1 <- 44
n1 <- 100
x2 <- 56
n2 <- 100
p1 <- x1/n1
p2 <- x2/n2
p <- (x1 + x2)/(n1+n2)
alpha <- 0.05
#Z - teststørrelse
Z <- (p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2))
Z
#kritisk værdi:
qnorm(1-alpha/2)
#p-værdi
2*pnorm(Z)
#CI
(p1-p2)+c(-1, 1)*qnorm(1-alpha/2)*sqrt((((x1/n1) *(1- (x1/n1))) / n1) +((x2/n2) * (1-(x2/n2)))/ n2)

### sample size ----
p <- 0.3
me <- 0.01
alpha <- 0.01

p*(1-p)*(qnorm(1-alpha/2)/(me/2))^2
#me er i en retning, så hvis der er angivet en brede i opgaven, så skal den skrives ind.
### multiple fordelinger /proportions ----
#teststørrelse bidrag
row_total <- 9+8
col_total <- 229
total <- 229
e <- (row_total * col_total)/total
o <- 9
(o-e)^2/e


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
df <- n-k
yi-yj+c(-1, 1)*qt(1-alpha/2, df)*sqrt(SSE/(n-k)*(1/ni+1/nj))

# F-størrelsen (test størrelsen)#####
F=(SSTr/(k-1))/(SSE/(n-k)); F
#OR
MSTr <- SSTr/(k-1)
F=MSTr/MSE; F

# SST - Totalafvigelsessum ####
SST <- SSE+SSTr; SST

# SSE ####
SSE <- MSE*(n-k)
# MSTr  #####
MSTr <- SSTr/(k-1)
# Kritisk værdi####
#1-way ANOVA critical value
alpha <- 0.05
k <- 7
n <- 7*5
qf(1-alpha, k-1, n-k)
# F - Teststørrelse#####
F <- (SSTr/(df1)/(SSE/(df2))); F
# P-værdi#####
pv <- 1 - pf(F, df1 <- df1, df2 <- df2); pv
### 2-way ANOVA -----
df1 <- 4
df2 <- 7
df3 <- 28
mstr <-1.82
msbl <- 11.07
mse <- 3.18
ftr <- mstr/mse
fbl <- msbl/mse
pvtr <- 1 - pf(ftr, df1, df3); pvtr
pvbl <- 1 - pf(fbl, df2, df3); pvbl
