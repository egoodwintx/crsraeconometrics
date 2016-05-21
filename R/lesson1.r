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

lesson1.2 = function() {
  ## no data file...purely math questions
  x = 225/150
  y = 400/250
  options(digits=4)
  print(x)
  print(y)

}

lesson1.3 = function() {
  uri = "data/TrainExer13.txt"
  rundat = read.csv(uri, sep='\t', header=TRUE)
  rundat


  reg = lm(rundat$Winning.time.men ~ rundat$Year)
  summary(reg)

  p = ggplot(rundat, aes(Year, Winning.time.men)) + geom_point() +
    geom_abline(intercept = reg[[1]][1], slope = reg[[1]][2])
  p

  # predict 2008, 2012, 2016
  pyears = c(2008, 2012, 2016)
  predictdf = data.frame()
  for(year in pyears) {
    p = reg[[1]][1] + year*reg[[1]][2]
    adf = data.frame("Year" = year,
                     "Time" = p)
    predictdf = rbind(predictdf, adf)

  }
  predictdf
}


lesson1.4 = function() {
## handwritten
}

lesson1.5 = function(x) {
  # icept = 186.5
  # slope = -1.750
  # y = icept + slope*1.189
  # y
  #
  ## Olympic time
  men = 10.386 + (-0.038)*x
  women = 11.606 + (-0.063)*x
  menlog = 2.341 - 0.0038*x
  womenlog = 2.452 - 0.0056*x

  answerdf = data.frame("men" = men,
                        "menlog" = menlog,
                        "women" = women,
                        "womenlog" = womenlog)
  answerdf
}

# lesson1.1()
# lesson1.2()
# lesson1.3()
# lesson1.4()
lesson1.5(16) # 2008 Olympics
lesson1.5(17) # 2012 Olympics
