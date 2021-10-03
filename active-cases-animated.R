

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gganimate)
indata <- read_csv('./data/cases-sk.csv')
indata$Active <- indata$Cases-indata$Recovered-indata$Deaths

# Data melted so that it can be used by ggplot2
ggdata <- indata %>%
  select(Date, Recovered, Active, Deaths) %>%
  melt(id="Date")
  

# Animated bell curve of cumulative cases and recoveries
anim <- ggplot(ggdata, aes(x=Date, y=value, fill=variable)) + 
  geom_area() +
  scale_fill_manual(values=c("green", "orange", "red")) + 
  labs(title = 'COVID-19 Cases in Saskatchewan', y = 'Cases') + 
  transition_reveal(Date)

anim_out <- animate(anim, end_pause=30, renderer = gifski_renderer())
anim_save('./output/covid-cases-growth-sk.gif', anim_out)


# Data melted so that it can be used by ggplot2
ggdata2 <- indata %>%
  select(Date, Active) %>% 
  melt(id="Date")


# Animated bell curve of cumulative cases and recoveries
anim <- ggplot(ggdata2, aes(x=Date, y=value, fill=variable)) + 
  geom_area() +
  # scale_fill_manual(values=c("green", "orange", "red")) + 
  labs(title = 'COVID-19 Cases in Saskatchewan', y = 'Cases') + 
  transition_reveal(Date)

anim_out <- animate(anim, end_pause=30, renderer = gifski_renderer())
anim_out
anim_save('./output/covid-cases-growth-sk-2021-04-22.gif', anim_out)


