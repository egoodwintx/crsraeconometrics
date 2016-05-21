##
## [lesson1.r]
##
## author     : Ed Goodwin
## project    : crsraeconometrics
## createdate : 05.20.2016
##
## description:
##    Coursera Econometrics course https://www.coursera.org/learn/erasmus-econometrics/
##
## version: 0.01
## changelog:
##
require(ggplot2)
require(gridExtra)


lesson1 = function() {
  uri="data/Dataset1.txt"
  salesdat = read.csv(uri, header=TRUE, sep='\t')
  summary(salesdat)
  str(salesdat)
}

lesson1.1 = function() {
  uri="data/TrainExer11.txt"
  survdat = read.csv(uri, header=TRUE, sep='\t')
  p = ggplot(survdat, aes(Age)) + geom_histogram() + ggtitle("Age")
  p2 = ggplot(survdat, aes(Expenditures)) + geom_histogram() + ggtitle("Expend")
  p3 = ggplot(survdat, aes(Age, Expenditures)) + geom_point() + ggtitle("Age vs. Expend")
  grid.arrange(p, p2, p3, nrow=2, ncol=2)

  ## sample mean
  means = c(mean(survdat$Expenditures),
    mean(subset(survdat, Age >= 40)$Expenditures),
    mean(subset(survdat, Age < 40)$Expenditures)
  )
  means
}

lesson1.1()
