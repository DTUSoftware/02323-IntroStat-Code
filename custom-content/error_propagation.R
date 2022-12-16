# ---------------------
#   From Hansen
# ---------------------

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
