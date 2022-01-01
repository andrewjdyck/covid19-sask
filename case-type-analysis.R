library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)


gdata <- read_csv('./data/case-types.csv') %>%
  gather(variable, value, -date)

ggplot(gdata, aes(x=date, y=value, fill=variable)) +
  geom_area(aes(color=variable)) +
  theme_gray()

case_types <- read_csv('./data/case-types.csv')
cases <- read_csv('./data/cases-sk.csv')




ct <- read_csv('./data/case-types.csv') %>%
  mutate(Total = Community + Contacts + Travellers) %>%
  # mutate(Total = Community + Contacts + Investigation + Travellers) %>%
  mutate(pct_Community = 100*Community/Total) %>%
  mutate(pct_Contacts = 100*Contacts/Total) %>%
  # mutate(pct_Investigation = Investigation/Total) %>%
  mutate(pct_Travellers = 100*Travellers/Total) %>%
  select(-Community, -Contacts, -Investigation, -Travellers, -Total) %>%
  rename_all(funs(str_replace(., "pct_", ""))) 

# THis is to plot ALL types, including investigation
ct <- read_csv('./data/case-types.csv') %>%
  mutate(Total = Community + Contacts + Investigation + Travellers) %>%
  mutate(pct_Community = 100*Community/Total) %>%
  mutate(pct_Contacts = 100*Contacts/Total) %>%
  mutate(pct_Investigation = 100*Investigation/Total) %>%
  mutate(pct_Travellers = 100*Travellers/Total) %>%
  select(-Community, -Contacts, -Investigation, -Travellers, -Total) %>%
  rename_all(funs(str_replace(., "pct_", ""))) 
  


pp <- ggplot(ct %>% gather(variable, value, -date)) +
  geom_area(aes(x=date, y=value, fill=variable)) +
  theme_gray() %>%
  labs(x='Date', 
       y='Share of all cases (%)', 
       title='Saskatchewan COVID-19 cases by source of transmission',
       fill='Transmission source')
pp
nlprivate::finalise_plot(pp, logo_image = "opendatask")

# Percentage of cases under investigation.
ggplot(ct %>% select(date, Investigation) %>% gather(variable, value, -date)) + geom_area(aes(x=date, y=value, fill=variable))

# 7 day moving average

dd <- read_csv('./data/case-types.csv') %>%
  # mutate(Total = Community + Contacts + Travellers) %>%
  mutate(chng_Community = Community - lag(Community, 7)) %>%
  mutate(chng_Contacts = Contacts - lag(Contacts, 7)) %>%
  mutate(chng_Travellers = Travellers - lag(Travellers, 7)) %>%
  mutate(Total = chng_Community + chng_Contacts + chng_Travellers) %>%
  mutate(pct_Community = chng_Community/Total) %>%
  mutate(pct_Contacts = chng_Contacts/Total) %>%
  mutate(pct_Travellers = chng_Travellers/Total) %>%
  select(-Community, -Contacts, -Investigation, -Travellers, -Total) %>%
  select(-chng_Community, -chng_Contacts, -chng_Travellers) %>%
  rename_all(funs(str_replace(., "pct_", ""))) 


ggplot(dd %>% gather(variable, value, -date), aes(x=date, y=value, fill=variable)) +
  geom_area(aes(color=variable)) +
  theme_gray()

