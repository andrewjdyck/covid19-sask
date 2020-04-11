
library(readr)
# library(xts)
# library(zoo)
source('./utils.R')

indata <- readr::read_csv('./data/cases-sk.csv')
# cases <- ts(indata$Cases, order.by=indata$Date)
cases <- indata$Cases


tenDayProj <- function(cases) {
  projectedCases <- floor(as.data.frame(projSimple(cases, 1:length(cases), inWindow = 10, outWindow = 10)$y)$fit)
  fcastCases <- projectedCases[length(projectedCases)]
  return(fcastCases)
}

forecast <- c(
  rep(NA, 18), 
  sapply(9:length(cases), function(x) tenDayProj(cases[1:x]))
)

dd <- data.frame(
  cases = c(cases, rep(NA, 10)),
  forecast = forecast
)

