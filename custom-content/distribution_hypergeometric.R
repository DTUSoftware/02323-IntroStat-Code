# The hypergeometric distribution is a discrete probability distribution used to model the probability of a certain
# number of successes in a sample drawn from a finite population without replacement. It is used in situations where
# the probability of success is dependent on the number of successes and failures that have already occurred.
# The hypergeometric distribution is often used in statistical hypothesis testing, for example, to test the
# significance of the difference between the observed number of successes in a sample and the expected number of
# successes based on the population proportions. It is also used in fields such as biology and engineering to analyze
# data from experiments and observations.

# remember to use 1 - ..... if you want the chance of above something.

# Hits that you want
x <- 3
# how many in the sample size that are hits
m <- 4
# size
s <- 10
# ammount of tries
k <- 5

# dhyper function can be used to calculate the probability of a certain number of successes in a sample from a
# hypergeometric distribution.
# Calculate the probability of 3 successes in a sample of 5 from a population of 10 with 4 successes
dhyper(x, m, s - m, k)
1 - dhyper(x, m, s - m, k)
# phyper function can be used to calculate the cumulative probability of a certain number of successes or fewer.
phyper(x, m, s - m, k)
1 - phyper(x, m, s - m, k)
# qhyper function can be used to calculate the number of successes at which a given cumulative probability occurs.
qhyper(x, m, s - m, k)
1 - qhyper(x, m, s - m, k)