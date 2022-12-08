
###########################################################################
## Set the working directory

## In RStudio the working directory is easily set via the menu
## "Session -> Set Working Directory -> To Source File Location" 
## Note: In R only "/" is used for separating in paths 
## (i.e. no backslash).
setwd("Replace with path to directory containing project files.")


###########################################################################
## Read data into R

## Read data from skivefjord1_data.csv
D <- read.table("skivefjord1_data.csv", header=TRUE, sep=";",
                as.is=TRUE)


###########################################################################
## Simple overview of the data

## Dimensions of D (number of rows and columns)
dim(D)
##  Column/variable names
names(D)
## The first rows/observations
head(D)
## The last rows/observations
tail(D)
## Selected summary statistics
summary(D)
## Another type of summary of the dataset
str(D)


###########################################################################
## Histogram (empirical density)

## Histogram describing the empirical density of the annual nitrate
## loads (histogram of annual emissions normalized to have an area of 1)
hist(D$Nload, xlab="Nitrate load", prob=TRUE)


###########################################################################
## Dividing the data into subsets

## Subset with VMP0 observations (before the first VMP was implemented)
VMP0 <- subset(D, vmp == 0)
## Check that it is a data.frame with the observations from VMP 0
VMP0
## Subset with VMP1 observations
VMP1 <- subset(D, vmp == 1)
## Subset with VMP2 observations
VMP2 <- subset(D, vmp == 2)
## Subset with VMP3 observations
VMP3 <- subset(D, vmp == 3)


###########################################################################
## Plot af udvikling over tid

## Plot of nitrate load over time (coloured according to applicable VMP)
plot(D$year, D$Nload, type="b", xlab="Year", ylab="Nitrate load")
lines(VMP0$year, VMP0$Nload, type="b", col=2)
lines(VMP1$year, VMP1$Nload, type="b", col=3)
lines(VMP2$year, VMP2$Nload, type="b", col=4)
lines(VMP3$year, VMP3$Nload, type="b", col=5)
## Add a legend
legend("topright", paste0("VMP", 0:3), lty=1, col=2:5)


###########################################################################
## Box plot by VMP

## Box plot of nitrate emission by VMP
boxplot(VMP0$Nload, VMP1$Nload, VMP2$Nload, VMP3$Nload, 
        names=c("VMP0", "VMP1", "VMP2", "VMP3"), 
        xlab="VMP", ylab="Nitrate load")


###########################################################################
## Summary statistics for annual nitrate emissions

## Total number of observations during VMP0
## (doesn't include missing values if there are any)
sum(!is.na(VMP0$Nload))
## Sample mean of annual nitrate emissions during VMP0
mean(VMP0$Nload, na.rm=TRUE)
## Sample variance of annual nitrate emissions during VMP0
var(VMP0$Nload, na.rm=TRUE)
## etc.
##
## The argument 'na.rm=TRUE' ensures that the statistic is
## computed even in cases where there are missing values.


###########################################################################
## qq-plot for model validation

## qq-plot for annual nitrate emission during VMP0
qqnorm(VMP0$Nload)
qqline(VMP0$Nload)


###########################################################################
## Confidence interval for the mean

## CI for the mean annual nitrate emission during VMP0
t.test(VMP0$Nload, conf.level=0.95)$conf.int


###########################################################################
## One-sample t-test

##  Testing hypothesis mu=2000 for annual nitrate emission during VMP0
t.test(VMP0$Nload, mu=2000)


###########################################################################
## Welch t-test for comparing two (independent) samples

## Comparison of annual nitrate emission during VMP0 and VMP3
t.test(VMP0$Nload, VMP3$Nload)


###########################################################################
## Computing correlations

## Computing the correlation between nitrate and phosphorus emissions
cor(D[, c("Nload","Pload")], use="pairwise.complete.obs")


###########################################################################
## Subsets in R
  
## Optional extra remark about taking subsets in R
##
## A logical vector with a TRUE or FALSE for each row in D,
## indicating the observations from VMP0 as TRUE
D$vmp == 0
## Can be used to take a subset of the data with VMP0 observations
D[D$vmp == 0, ]
## Yields the same result as
subset(D, vmp == 0)
## May be used in a 'for'-loop to plot data from each VMP separately
plot(D$year, D$Nload, type="n")
for(i in 0:3){
  lines(D$year[D$vmp == i], D$Nload[D$vmp == i], type="b", col=i+2)
}
## More complex logical expressions can be made, e.g.: 
## Find all observations recorded during VMP0, but after 1984
D[D$year > 1984 & D$vmp == 0, ]


###########################################################################
## More R tips

## Use a 'for'-loop to calculate the summary statistics for each VMP
## and assign the result to a new data.frame
Tbl <- data.frame()
for(i in 0:3){
  Tbl[i+1,"mean"] <- mean(D$Nload[D$vmp == i])
  Tbl[i+1,"var"] <- var(D$Nload[D$vmp == i])
}
## See what Tbl contains
Tbl

## In R there are even more condensed ways to perform such 
## calculations, e.g.:
aggregate(D$Nload, by=list(D$vmp), function(x){ c(mean(x), var(x)) })
## See more useful functions with: ?apply, ?aggregate and ?lapply
## For extremely efficient data handling see, e.g., the packages: 
## dplyr, tidyr, reshape2 and ggplot2

## LaTeX tips:
##
## The R package "xtable" can generate LaTeX tables written to a file 
## and thereby they can automatically be included in a .tex document.
## 
## The R package "knitr" can be used very elegantly to generate .tex 
## documents with R code written directly in the document. This  
## document and the book were generated using knitr.
