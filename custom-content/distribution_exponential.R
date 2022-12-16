# -----------------------------------------------------------------------------
#   EXPONENTIAL DISTRIBUION
# -----------------------------------------------------------------------------

# The exponential distribution is a continuous probability distribution used to model the time it takes for an event to
# occur. It is often used to model the time between events in a Poisson process, which is a statistical process that
# describes the occurrence of events at a constant average rate. The exponential distribution has a number of useful
# properties. For example, it is memoryless, which means that the probability of an event occurring at time t, given
# that it has not occurred up to time t, is the same as the probability of it occurring at time 0. This property makes
# the exponential distribution useful for modeling the lifetime of a system or the time it takes for a customer to make
# a purchase, for example.

# remember to use 1 - ..... if you want the chance of above something.

# whatever you want
x <- 2
# The rate (percent - 3% -> 0.03) - if you have a time of 3 years (mean), you need the inverse of the mean = 1/3
rate <- 1 / 3

# dexp is the density function of the exponential distribution. It calculates the probability density of a given value,
# given the parameters of the distribution.
dexp(x, rate = rate)
1 - dexp(x, rate = rate)

# pexp is the cumulative distribution function (CDF) of the exponential distribution. It calculates the probability
# that a random variable from the exponential distribution is less than or equal to a given value.
pexp(x, rate = rate)
1 - pexp(x, rate = rate)

# qexp is the inverse cumulative distribution function (ICDF) of the exponential distribution. It calculates the value
# at which a given probability occurs in the exponential distribution.
qexp(x, rate = rate)
1 - qexp(x, rate = rate)

# rexp is a function that generates random numbers from the exponential distribution. It is often used in statistical
# software to simulate data from an exponential distribution or to perform Monte Carlo simulations.
rexp(x, rate = rate)
