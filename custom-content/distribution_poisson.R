# In R, the dpois, ppois, qpois, and rpois functions are all related to the Poisson distribution.
#
# The Poisson distribution is a discrete probability distribution that expresses the probability of a given number of
# events occurring in a fixed interval of time or space, if these events occur with a known average rate and
# independently of the time since the last event. It is often used to model the number of times an event occurs within
# a time period, such as the number of calls received by a call center per minute or the number of customers arriving
# at a store per hour.

# In a Poisson distribution, the parameter lambda (λ) represents the average number of events that occur in a given
# interval of time or space. It is also known as the rate parameter or the expected value of the distribution.
lambda = 3.5

x = 0


# remember to use 1 - pois if you want something above a certain value, for example when you want the chance that two
# or more people is gonna visit a website then you set x to 1 because you want more than 1 person and then do the
# 1 - ppois

# dpois: This function calculates the probability density function (PDF) of the Poisson
# distribution at a given point. The PDF gives the probability of observing a value within
# a given range.
dpois(x, lambda = lambda)

1 - dpois(x, lambda = lambda)

# ppois: This function calculates the cumulative distribution function (CDF) of the Poisson
# distribution at a given point. The CDF gives the probability of observing a value less than
# or equal to a given point.
ppois(x, lambda = lambda)

1 - ppois(x, lambda = lambda)

# qpois: This function calculates the inverse CDF (quantile function) of the Poisson distribution
# at a given probability. It gives the value at which the CDF equals the given probability.
qpois(x, lambda = lambda)

1 - qpois(x, lambda = lambda)

# rpois: This function generates random samples from the Poisson distribution.
rpois(x, lambda = lambda)