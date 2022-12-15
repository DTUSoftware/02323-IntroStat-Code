n1 <- 2000
x1 <- 1920
n2 <- 2024
x2 <- 1801
x <- c(x1, x2)
n <- c(n1, n2)
alpha <- 0.01

# proportion test
prop.test(x, n, conf.level = 1 - alpha, correct = FALSE)

t.test(x, n, conf.level = 1 - alpha)