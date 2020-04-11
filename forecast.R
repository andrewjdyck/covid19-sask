
library(readr)
# library(xts)
# library(zoo)
library(plyr)
source('./utils.R')

indata <- readr::read_csv('./data/cases-sk.csv')
# cases <- ts(indata$Cases, order.by=indata$Date)
cases <- indata$Cases


tenDayProj <- function(cases, nn) {
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



# forecasted series over time

projDf <- function(df, endDay) {
  nn <- length(df$Cases[1:endDay])
  casesSubset <- df$Cases[1:endDay]
  fcastdate <- df$Date[nn]
  newdates <- c(indata$Date, as.Date(1:10 + as.numeric(df$Date[nrow(df)]), origin = '1970-01-01'))
  pc <- floor(as.data.frame(projSimple(casesSubset, 1:nn, inWindow = 10, outWindow = (length(cases)+10-endDay))$y))
  pc[1:nn, c('fit', 'lwr', 'upr')] <- cbind(casesSubset, casesSubset, casesSubset)
  pc$fcastday <- df$Date[nn]
  pc$Date <- newdates
  return(pc)
}

projAll <- function(df) {
  lapply(10:nrow(df), function(x) projDf(df, x))
}

tt <- projAll(indata)

# x <- 19
# ggplot(tt[x][[1]], aes(x = day, y = fit)) + 
#   # geom_line(colour='blue') +
#   geom_smooth(aes(x=day, y=fit, ymax=upr, ymin=lwr), 
#               colour='red', data=tt[x][[1]], stat='identity')


# dd <- rbind(tt[[10]], tt[[20]])
dd <- ldply(tt)
pa <- ggplot(dd, aes(x = Date, y = fit, group=fcastday)) + 
  # geom_line(colour='blue') +
  geom_smooth(aes(group=fcastday, ymax=upr, ymin=lwr), 
               stat='identity') 

pa
pa_out <- animate(pa + 
  transition_states(fcastday) +
  labs(title = 'Forecast day: {previous_state}', y='COVID-19 Cases', x='Date') + 
  view_follow(), end_pause = 30)
anim_save('./output/forecast.gif', pa_out)
