##
## [testexercise1.r]
##
## author     : Ed Goodwin
## project    : crsraeconometrics
## createdate : 05.21.2016
##
## description:
##    Coursera Econometrics course Test Exercise 1
##    https://www.coursera.org/learn/erasmus-econometrics/peer/EqUc3/test-exercise-1
##
## version: 0.01
## changelog:
##
library(ggplot2)

uri = "data/TestExer1-sales-round1.txt"
tsalesdat = read.csv(uri, sep='\t', header=TRUE)
names(tsalesdat) = c("Observ", "Advert", "Sales")

p = ggplot(tsalesdat, aes(Advert, Sales)) +
  geom_point() +
  ggtitle("Sales vs Advertising Spend")
p

## What do you expect to find if you fit a regression line to this data?
## The regression line would be misleading because of the outlier in Observation 12.

regmodel = lm(tsalesdat$Sales ~ tsalesdat$Observ)
summary(regmodel)

## Estimate the coefficients a and b in the simple regression model with sales
## as dependent variable and advertising as explanatory factor.
## Also compute the standard error and t-value of b. Is b significantly different from 0?
## Coefficients: a = 26.5211, b = -0.0211
## Std error b = 0.2294
## t-value: -0.09, not statistically significant from 0

## Residuals
residmodel = data.frame("resid" = resid(regmodel))
p = ggplot(residmodel, aes(resid)) + geom_histogram(binwidth = 1)
p
summary(residmodel)
## Linear regression is not a good fit...residuals are not normally distributed
## probably due to the outlier


## Apparently, the regression result of part (b) is not satisfactory.
## Once you realize that the large residual corresponds to the week with opening
## hours during the evening, how would you proceed to get a more satisfactory
## regression model?
## You could remove the outlier to get cleaner data. This is acceptable because
## the data represents a point that was not operating under the same model as the
## other data points (open in the evenings)

## Delete this special week from the sample and use the remaining 19 weeks to estimate
## the coefficients a and b in the simple regression model with sales as dependent variable
## and advertising as explanatory factor. Also compute the standard error and t-value of b.
## Is b significantly different from 0?

## remove outlier
tsalesdatclean = tsalesdat[-12,]
resmodelclean = lm(tsalesdatclean$Sales ~ tsalesdatclean$Sales)
summary(resmodelclean)
## Coefficients: a = 25.86, b = -0.0776
## Std error b = 0.0557
## t-value: -1.39, not statistically significant from 0

##
## Discuss the differences between your findings in parts (b) and (e).
## Describe in words what you have learned from these results.
## Standard error was significantly reduced but the model is still not very descriptive
par(mfrow=c(2,1))
plot(resid(resmodelclean))
hist(resid(resmodelclean))
