# -----------------------------------------------------------------------------
#   POWER T-TEST
# -----------------------------------------------------------------------------

# FROM HANSEN ---------------------

# 3) Power / sample size ----
### 1-sample power/ sample size ----
# number of observasions
n <- 30
# true difference in means
delta <- 5
# variance
v <- 16
# stabdard devitation
sd <- sqrt(v)
# sig level
alpha <- 0.01
# power
power <- 0.98
# types
type <- c("two.sample", "one.sample", "paired")
# remove whatever you need to find for example if you need to find n remove n from below
power.t.test(n = n, delta = delta, sd = sd, sig.level = alpha, power = power,
             type = type[1])


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
