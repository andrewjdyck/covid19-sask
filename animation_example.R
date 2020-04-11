

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gganimate)
indata <- read_csv('./data/cases-sk.csv')
indata$Active <- indata$Cases-indata$Recovered-indata$Deaths
ggdata <- indata %>%
  select(Date, Recovered, Active, Deaths) %>%
  melt(id="Date")
  
anim <- ggplot(ggdata, aes(x=Date, y=value, fill=variable)) + 
  geom_area() +
  scale_fill_manual(values=c("green", "orange", "red")) + 
  labs(title = 'COVID-19 Cases in Saskatchewan', y = 'Cases') + 
  transition_reveal(Date)

anim_out <- animate(anim, end_pause=10)
anim_save('./output/cases_gif.gif', anim_out)


dd <- data.frame(time=c(1:10, 1:10), day=c(rep(1, 10), rep(2, 10)))
dd$value <- dd$day*dd$time + 10

pp <- ggplot(dd, aes(x=time, y=value, group=day)) + 
  geom_line()

pp

pp + 
  transition_states(day, transition_length = 2, state_length = 1)



library(gapminder)
library(ggplot2)
library(gganimate)
p <- ggplot(gapminder, aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")

p + transition_time(year) +
  labs(title = "Year: {frame_time}")



library(ggplot2)
library(forecast)
wine.fit <- hw(wineind,h=48)
plot(wine.fit)
autoplot(wine.fit)


aa <- autoplot(wine.fit) +
  geom_forecast()
aa +
  transition_time(Point)







library(ggplot2)
tt <- autoplot(USAccDeaths) + geom_forecast()

tt +
  transition_reveal()

lungDeaths <- cbind(mdeaths, fdeaths)
autoplot(lungDeaths) + geom_forecast()

# Using fortify.ts
p <- ggplot(aes(x=x, y=y), data=USAccDeaths)
p <- p + geom_line()
p + geom_forecast()

# Without fortify.ts
data <- data.frame(USAccDeaths=as.numeric(USAccDeaths), time=as.numeric(time(USAccDeaths)))
p <- ggplot(aes(x=time, y=USAccDeaths), data=data)
p <- p + geom_line()
p + geom_forecast()

p + geom_forecast(h=60)
p <- ggplot(aes(x=time, y=USAccDeaths), data=data)
p + geom_forecast(level=c(70,98))
p + geom_forecast(level=c(70,98),colour="lightblue")

#Add forecasts to multivariate series with colour groups
lungDeaths <- cbind(mdeaths, fdeaths)
autoplot(lungDeaths) + geom_forecast(forecast(mdeaths), series="mdeaths")



fcast <- splinef(airmiles,h=5)
plot(fcast)
autoplot(fcast)

