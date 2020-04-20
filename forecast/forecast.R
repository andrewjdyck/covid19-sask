
library(readr)
# library(xts)
# library(zoo)
library(plyr)
library(ggplot2)
library(gganimate)
source('./utils.R')

indata <- readr::read_csv('../data/cases-sk.csv')


# forecasted series over time

projDf <- function(df, endDay) {
  nn <- length(df$Cases[1:endDay])
  casesSubset <- df$Cases[1:endDay]
  fcastdate <- df$Date[nn]
  newdates <- c(df$Date, as.Date(1:10 + as.numeric(df$Date[nrow(df)]), origin = '1970-01-01'))
  pc <- floor(as.data.frame(projSimple(casesSubset, 1:nn, inWindow = 10, outWindow = (nrow(df)+10-endDay))$y))
  pc[1:nn, c('fit', 'lwr', 'upr')] <- cbind(casesSubset, casesSubset, casesSubset)
  pc$fcastday <- df$Date[nn]
  pc$Date <- newdates
  return(pc)
}



# Prepare forecasted data for ggplot2 and save
dd <- ldply(lapply(10:nrow(indata), function(x) projDf(indata, x)))
write.csv(dd, paste0('./simple_forecasts.csv'), row.names=FALSE)

forecastedCases <- read_csv('./simple_forecasts.csv')
fanim <- ggplot(forecastedCases, aes(x = Date, y = fit, group=fcastday)) + 
  geom_smooth(aes(group=fcastday, ymax=upr, ymin=lwr), 
               stat='identity') 


fanim_out <- animate(fanim + 
  transition_states(fcastday) +
  labs(title = 'COVID-19 forecasted cases: {previous_state}', y='COVID-19 Cases', x='Date') + 
  view_follow(), end_pause = 30)
anim_save('./forecast.gif', fanim_out)
