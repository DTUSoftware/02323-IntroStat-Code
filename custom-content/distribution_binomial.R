# -----------------------------------------------------------------------------
#   ChatGPT Overlord
# -----------------------------------------------------------------------------

# How many "hits" you want / the value you want to hit
x <- 0
# size is how many times you try / draw
size <- 10
# the probability of getting a hit (percent - 3% -> 0.03)
prob <- 3/100

# remember that binomal doesn't "remove" options, and if you want the result to be above something use the 1-
# The dbinom function is used to compute the probability mass function (PMF) of the binomial distribution,
# which gives the probability that a random variable will take on a certain value.
# In general, you should use the dbinom function if you want to compute the probability of a specific
# number of successes in a certain number of trials
dbinom(x, size = size, prob = prob)
1- dbinom(x, size = size, prob = prob)

# The pbinom function is used to compute the cumulative distribution function (CDF) of the binomial
# distribution, which gives the probability that a random variable will take on a value less than or equal
# to a certain value.
# the pbinom function if you want to compute the probability of having a certain number of successes or
# fewer in a certain number of trials,
pbinom(x, size = size, prob = prob)
1 - pbinom(x, size = size, prob = prob)

# The qbinom function is used to compute the quantile function of the binomial distribution,
# which gives the value that a random variable will take on with a certain probability.
# the qbinom function if you want to find the number of successes that will occur with a certain probability
# in a certain number of trials.
qbinom(x, size = size, prob = prob)
1 - qbinom(x, size = size, prob = prob)


# -----------------------------------------------------------------------------
#   PBINOM GUIDE
# -----------------------------------------------------------------------------

# Pbinom return probability of a given number of successes with an amount of attempts.
# at something. Lets say the probability of rolling five 6's in 10 tries on a die

# successes = 1, Attempts = 100, succes_prob = 1/6 = 0.9999% of happening

successes <- 1
attempts <- 2
succes_prob <- 1/2

# Probability of AT MOST having this amount
pbinom(successes, attempts, succes_prob)

# Probability of having LESS successes
1-pbinom(successes, attempts, succes_prob)

# Chance of EXACTLY the amount of successes
dbinom(successes, attempts, succes_prob)

# ------------------------------------------------------------------------------
#   ROBOT OVERLORD
# ------------------------------------------------------------------------------

# Generate binomial random variables
binomial_data <- rbinom(n = 100, size = 10, prob = 0.5)

# Summarize the binomial data
summary(binomial_data)
