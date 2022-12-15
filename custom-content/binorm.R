# How many "hits" you want / the value you want to hit
x <- 0
# size is how many times you try / draw
size <- 10
# the probability of getting a hit
prob <- 0.03

# The dbinom function is used to compute the probability mass function (PMF) of the binomial distribution,
# which gives the probability that a random variable will take on a certain value.
# In general, you should use the dbinom function if you want to compute the probability of a specific
# number of successes in a certain number of trials
dbinom(x, size = size, prob = prob)
# The pbinom function is used to compute the cumulative distribution function (CDF) of the binomial
# distribution, which gives the probability that a random variable will take on a value less than or equal
# to a certain value.
# the pbinom function if you want to compute the probability of having a certain number of successes or
# fewer in a certain number of trials,
pbinom(x, size = size, prob = prob)
# The qbinom function is used to compute the quantile function of the binomial distribution,
# which gives the value that a random variable will take on with a certain probability.
# the qbinom function if you want to find the number of successes that will occur with a certain probability
# in a certain number of trials.
qbinom(0.98, size = size, prob = prob)