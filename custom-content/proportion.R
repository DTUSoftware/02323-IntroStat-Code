# -----------------------------------------------------------------------------
#   PROPORTION TESTING
# -----------------------------------------------------------------------------

# FROM HANSEN ---------------------


# 7) proportion ----
#1. find ud af om det er 1-sample, eller to grupper/fordelinger der sammenlignes.
#Eller flere elementer der skal sammenlignes.
#Hvis det er 1-sample er man interesseret i en nulhypotese omkring en procentvis fordeling.
# er det 2-sample eller mange sample så tester man om fordelingerne er ens, ud fra en antagelse om at de er.
#Ved multi samle anvendes chi^2 test, ved 1-sample eller 2-sample kan prop.test anvendes.

### automatisk test ----
prop.test(x = c(6, 12), n = c(50, 50), correct <- FALSE)
prop.test(36, 200, correct = FALSE, conf.level = 0.99)
### 1- sample CI, Z, p-værdi for fordeling ----
x <- 44
n <- 100
#fordelingen der testes efter
p <- x / n
alpha <- 0.05
#Z - teststørrelse
z_obs <- (x - n * p) / sqrt(n * p * (1 - p))
#kritisk værdi:
qnorm(1 - alpha / 2)
#p-værdi
2 * pnorm(z_obs)
#CI
p + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)

#Alternativt
n <- 2333 + 2536
p <- 2333 / n
sigma_p <- sqrt(p * (1 - p) / n)
l_limit <- p - 1.96 * sigma_p
u_limit <- p + 1.96 * sigma_p

### 2x2 forskel CI, Z og p-værdi ----
x1 <- 44
n1 <- 100
x2 <- 56
n2 <- 100
p1 <- x1 / n1
p2 <- x2 / n2
p <- (x1 + x2) / (n1 + n2)
alpha <- 0.05
#Z - teststørrelse
Z <- (p1 - p2) / sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
Z
#kritisk værdi:
qnorm(1 - alpha / 2)
#p-værdi
2 * pnorm(Z)
#CI
(p1 - p2) + c(-1, 1) *
  qnorm(1 - alpha / 2) *
  sqrt((((x1 / n1) * (1 - (x1 / n1))) / n1) + ((x2 / n2) * (1 - (x2 / n2))) / n2)

### sample size ----
p <- 0.3
me <- 0.01
alpha <- 0.01

p * (1 - p) * (qnorm(1 - alpha / 2) / (me / 2))^2
#me er i en retning, så hvis der er angivet en brede i opgaven, så skal den skrives ind.
### multiple fordelinger /proportions ----
#teststørrelse bidrag
row_total <- 9 + 8
col_total <- 229
total <- 229
e <- (row_total * col_total) / total
o <- 9
(o - e)^2 / e
