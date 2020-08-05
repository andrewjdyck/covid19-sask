

library(readr)
library(tidyr)
library(ggplot2)


gdata <- read_csv('./data/case-types.csv') %>%
  gather(variable, value, -date)

ggplot(gdata, aes(x=date, y=value, fill=variable)) +
  geom_area(aes(color=variable)) +
  theme_gray()

case_types <- read_csv('./data/case-types.csv')
cases <- read_csv('./data/cases-sk.csv')




library(stringr)
ct <- read_csv('./data/case-types.csv') %>%
  mutate(Total = Community + Contacts + Investigation + Travellers) %>%
  mutate(pct_Community = Community/Total) %>%
  mutate(pct_Contacts = Contacts/Total) %>%
  mutate(pct_Investigation = Investigation/Total) %>%
  mutate(pct_Travellers = Travellers/Total) %>%
  select(date, pct_Community, pct_Contacts, pct_Investigation, pct_Travellers) %>%
  rename_all(funs(str_replace(., "pct_", ""))) %>%
  gather(variable, value, -date)


ggplot(ct, aes(x=date, y=value, fill=variable)) +
  geom_area(aes(color=variable)) +
  theme_gray()
