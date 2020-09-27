library(rvest)
library(readr)
library(stringr)
library(dplyr)

# Downloading CSV

get_cases_dl <- function() {
  link_stub <- read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19/cases') %>%
    html_nodes('.indicator-export') %>%
    html_nodes('.list-unstyled') %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    tbl_df() %>%
    filter(str_detect(value, 'csv')) %>%
    unlist()
  names(link_stub) <- NULL
  outdf <- readr::read_csv(paste0('https://dashboard.saskatchewan.ca', link_stub))
  return(outdf)
}

get_tests_dl <- function() {
  link_stub <- read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19/tests') %>%
    html_nodes('.indicator-export') %>%
    html_nodes('.list-unstyled') %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    tbl_df() %>%
    filter(str_detect(value, 'csv')) %>%
    unlist()
  names(link_stub) <- NULL
  outdf <- readr::read_csv(paste0('https://dashboard.saskatchewan.ca', link_stub))
  return(outdf)
}

get_hospitalizations_dl <- function() {
  link_stub <- read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19-cases/hospitalized') %>%
    html_nodes('.indicator-export') %>%
    html_nodes('.list-unstyled') %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    tbl_df() %>%
    filter(str_detect(value, 'csv')) %>%
    unlist()
  names(link_stub) <- NULL
  outdf <- readr::read_csv(paste0('https://dashboard.saskatchewan.ca', link_stub))
  return(outdf)
}

cases_export <- get_cases_dl()
tests_export <- get_tests_dl()
hospitalized_export <- get_hospitalizations_dl()

readr::write_csv(cases_export, './data/dashboard-export-cases.csv')
readr::write_csv(tests_export, './data/dashboard-export-tests.csv')
readr::write_csv(hospitalized_export, './data/dashboard-export-hospitalized.csv')

regina_new <- cases_export %>% filter(Region == "Regina") %>% tail(10)
regina_new

# update running aggregated total CSV
agg_df <- readr::read_csv('./data/cases-sk.csv')
max_agg_df_dt <- agg_df %>% summarize(dt = last(Date))

new_cases <- cases_export %>% 
  filter(Date > max_agg_df_dt$dt) %>%
  select(-Region) %>%
  group_by(Date) %>%
  summarise_all(sum)
new_tests <- tests_export %>% 
  filter(Date > max_agg_df_dt$dt) %>%
  select(-Region) %>%
  group_by(Date) %>%
  summarise_all(sum)

update_df <- new_cases[, c('Date', 'New Cases', 'Total Cases', 'Inpatient Hospitalizations', 'ICU Hospitalizations', 'Recovered Cases', 'Deaths')]
update_df$Tests <- new_tests$`New Tests`

names(update_df) <- c('Date', 'New', 'Cases', 'Hospitalized', 'ICU', 'Recovered', 'Deaths', 'Tests')
agg_df <- agg_df %>%
  bind_rows(update_df)


readr::write_csv(agg_df, './data/cases-sk.csv')

##################
# Older updates scraper
##################
##################


latest_updates_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/latest-updates'
older_updates_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/latest-updates/step-details/news-releases/older-covid-19-news-releases'

library(stringr)
library(rvest)
d1 <- read_html(older_updates_url) %>%
  html_nodes('li') %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  tbl_df() %>%
  filter(str_detect(value, 'covid-19-update'))

d2 <- read_html(latest_updates_url) %>%
  html_nodes('li') %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  tbl_df() %>%
  filter(str_detect(value, 'covid-19-update'))


extract_date_from_url <- function(url) {
  mm <- str_match(url, "news-and-media/\\s*(.*?)\\s*/covid-19-update")[,2]
  return(as.Date(mm, '%Y/%B/%d'))
}


gen_case_typs_df <- function(url) {
  case_types <- read_html(url) %>%
    html_nodes('.general-content') %>%
    html_nodes('li') %>%
    html_text() %>%
    tbl_df() %>%
    filter(
      str_detect(value, 'cases are travellers') |
        str_detect(value, 'are community contacts') |
        str_detect(value, 'have no known exposures') |
        str_detect(value, 'are under investigation by local public health')
    ) %>%
    mutate(vv = as.numeric(str_trim(gsub("([0-9]+).*$", "\\1", value)))) %>%
    mutate(variable = ifelse(
      str_detect(value, 'travellers'), 'Travellers', ifelse(
        str_detect(value, 'community contacts'), 'Contacts', ifelse(
          str_detect(value, 'no known exposures'), 'Community', ifelse(
            str_detect(value, 'under investigation'), 'Investigation', ''
          )
        )
      )
    )) %>%
    select('variable', 'vv') %>%
    rename(value = 'vv') %>%
    mutate(date = extract_date_from_url(url))
}



library(tidyr)
# latest updates
latest_case_types_df <- lapply(d2$value, gen_case_typs_df) %>%
  bind_rows() %>%
  spread(variable, value)

# older updates
older_case_types_df <- lapply(d1$value, gen_case_typs_df) %>%
  bind_rows() %>%
  spread(variable, value)

# update running aggregated Case Types CSV
agg_ct_df <- readr::read_csv('./data/case-types.csv')
max_agg_ct_df_dt <- agg_ct_df %>% summarize(dt = last(date))

update_ct_df <- latest_case_types_df %>% 
  bind_rows(older_case_types_df) %>% 
  arrange(date) %>%
  filter(date > max_agg_ct_df_dt$dt)

agg_ct_df <- agg_ct_df %>%
  bind_rows(update_ct_df)


readr::write_csv(agg_ct_df, './data/case-types.csv')








################################
################################
# Other stuff

library(rvest)
library(jsonlite)

# Current date
dt <- as.character(format(Sys.Date(), '%Y%m%d'))
url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/cases-and-risk-of-covid-19-in-saskatchewan'

# Read summary highlight notes
highlights_url <- 'https://dashboard.saskatchewan.ca/health-wellness/covid-19/cases'
summ <- read_html(highlights_url) %>%
  html_nodes('.indicator-highlights')

lst <- html_node(summ, 'ul')
ll <- lst[[1]]

l1 <- ll %>%
  html_text() %>% 
  strsplit(split = "\n") %>%
  unlist()

write(toJSON(l1), paste0('./data/lst_', dt, '.json'))



##################
# not used yet
outbreaks_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/latest-updates'

##################
##################


# read age distribution
dd <- read_html(url) %>%
  html_nodes('.compacttable') %>%
  html_table()

# age distribution table
ages <- dd[[1]][2:nrow(dd[[1]]), ]
names(ages) <- dd[[1]][1, ]

write.csv(ages, paste0('./data/covid_sk_age_dist_', dt, '.csv'), row.names=FALSE)



