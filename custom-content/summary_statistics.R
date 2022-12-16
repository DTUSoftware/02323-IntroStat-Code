# input the array
x <- c(22.3, 11.8, 14.8, 13.4, 14.8, 15.9, 6.6, 9.6, 16.5)
mean(x)
median(x)
var(x)
sd(x)
summary(x)

# estimate of standard error of the estimated proportion
# the ammount thatare hits
x <- 1801
# the sample size
n <- 2024
# the proportion
p <- x / n
# estimated variance
s2 <- p * (1 - p) / n
# estimated standard error
s <- sqrt(s2)
s



###########################################################################
## Read data into R

# Read the dataset 'soenderborg2_data.csv' into R
D <- read.table("soenderborg2_data.csv", sep = ";", header = TRUE)


###########################################################################
## Processing of data

# Make 't' a date variable in R
D$t <- as.Date(D$t, format = "%d/%m/%Y")

# Choose data from 15 Oct 2009 to 15 Apr 2010 for the four houses
D_model <- subset(D, ("2009-10-15" <= t & t < "2010-04-16") &
  (houseId %in% c(3, 5, 10, 17)))

# Remove observations with missing values
D_model <- na.omit(D_model)

###########################################################################
## Short descriptive analysis and summary of the data

variables <- c("Q", "Ta", "G")

# Summary Table
Tbl <- apply(D_model[, variables], 2, function(x) {
    c(
      n = sum(!is.na(x)),                                   ## Total number of observations (doesn't include missing values if there are any)
      mean = mean(x, na.rm = TRUE),                         ## Sample mean of daily heat consumption
      var = var(x, na.rm = TRUE),                           ## Sample variance of daily heat consumption
      sd = sd(x, na.rm = TRUE),                             ## Sample standard deviance
      lq = unname(quantile(x, probs = 0.25, na.rm = TRUE)), ## Lower quartile, Q1
      median = median(x, na.rm = TRUE),                     ## Median, Q2 (could also have used "quantile(x, probs=0.5, na.rm=TRUE)")
      hq = unname(quantile(x, probs = 0.75, na.rm = TRUE))  ## Upper quartile, Q3
    )
})
Tbl


# -----------------------------------------------------------------------------
#   STANDARD ERROR
# -----------------------------------------------------------------------------

s <- 0
n <- 0

SE <- s / sqrt(n)