library(rvest)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)


# Downloading CSV

get_cases_dl <- function() {
  link_stub <- read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19/cases') %>%
    html_nodes('.indicator-export') %>%
    html_nodes('.list-unstyled') %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    as_tibble() %>%
    filter(str_detect(value, 'csv')) %>%
    unlist()
  names(link_stub) <- NULL
  # This needs to be fixed because the format of the CSV has changed.
  # There are now columns for age groups and vaccination status
  outdf <- readr::read_csv(paste0('https://dashboard.saskatchewan.ca', link_stub)) %>%
    # filter(!is.na(`Total Cases`))
    group_by(Date, Region) %>% summarise_all('sum', na.rm=TRUE) %>% as_tibble()
  return(outdf)
}

get_tests_dl <- function() {
  link_stub <- read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19-tests/tests') %>%
    html_nodes('.indicator-export') %>%
    html_nodes('.list-unstyled') %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    as_tibble() %>%
    filter(str_detect(value, 'csv')) %>%
    unlist()
  names(link_stub) <- NULL
  # This needs to be fixed too. The New Tests column can be NA for a given day (eg. '2021-06-01')
  # but have valid data for other rows
  outdf <- readr::read_csv(paste0('https://dashboard.saskatchewan.ca', link_stub)) %>%
    filter(!is.na(`New Tests`))
  return(outdf)
}

get_hospitalizations_dl <- function() {
  link_stub <- read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19-cases/hospitalized') %>%
    html_nodes('.indicator-export') %>%
    html_nodes('.list-unstyled') %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    as_tibble() %>%
    filter(str_detect(value, 'csv')) %>%
    unlist()
  names(link_stub) <- NULL
  # There is never a cases when Inpatient is NA, but ICU is not.
  # I should probably update this to account for cases where this could be true.
  outdf <- readr::read_csv(paste0('https://dashboard.saskatchewan.ca', link_stub)) %>%
    filter(!is.na(`Inpatient Hospitalizations`))
  return(outdf)
}

# I'm probably going to need to do something about the doses CSV file
# Example: https://dashboard.saskatchewan.ca/export/vaccines/4327.csv


cases_export <- get_cases_dl()
tests_export <- get_tests_dl()
hospitalized_export <- get_hospitalizations_dl()

# regina_new
cases_export %>% filter(Region == "Regina") %>% tail(10)

# total_new 
cases_export %>% group_by(Date) %>% select(-Region) %>% summarize_all(sum) %>% tail(10)

# Output export datasets
readr::write_csv(cases_export, './data/dashboard-export-cases.csv')
readr::write_csv(tests_export, './data/dashboard-export-tests.csv')
readr::write_csv(hospitalized_export, './data/dashboard-export-hospitalized.csv')



# update running aggregated total CSV
agg_df <- readr::read_csv('./data/cases-sk.csv')
max_agg_df_dt <- agg_df %>% summarize(dt = last(Date))

new_cases <- cases_export %>% 
  filter(Date > max_agg_df_dt$dt) %>%
  select(-Region) %>%
  group_by(Date) %>%
  summarise_all(sum, na.rm=TRUE)

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
# vaccine scraper
##################
##################

vaccine_url <- 'https://dashboard.saskatchewan.ca/health-wellness/covid-19-vaccines/vaccines#new-doses-tab'

##################
# New case types scraper
##################
##################

daily_case_types_url <- 'https://dashboard.saskatchewan.ca/health-wellness/covid-19/cases'
url <- daily_case_types_url
read_html(url) %>%
  html_nodes('.indicator-main-content') %>%
  html_nodes('.col-sm-9')
  html_attr('.indicator-highlights')

# looks like may not be able to scrape because its under the data-* tag.

get_new_case_types <- function(url) {
  
}


##################
# Older updates scraper
##################
##################

latest_updates_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/latest-updates'
# older_updates_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/latest-updates/step-details/news-releases/older-covid-19-news-releases'
# older updates url is deprecated. Notices are now located at the health ministry news pages
health_min_news_updates_url <- 'https://www.saskatchewan.ca/government/news-and-media?text=%22COVID-19+Update%3a%22&ministry=5FD58D569A72474B8D543396985C0409'
# health_min_news_updates_url <- 'https://www.saskatchewan.ca/government/news-and-media?text=%22COVID-19+Update%3a%22&ministry=5FD58D569A72474B8D543396985C0409&page=27'

extract_health_min_daily_update_urls <- function(base_url) {
  read_html(base_url) %>%
    html_nodes('.results') %>%
    html_children() %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    as_tibble() %>%
    filter(str_detect(value, 'covid19-update'))
}

health_min_daily_update_urls <- extract_health_min_daily_update_urls(health_min_news_updates_url)
  

extract_date_from_url <- function(url) {
  mm <- str_match(url, "news-and-media/\\s*(.*?)\\s*-update")[,2]
  return(as.Date(mm, '%Y/%B/%d'))
}


gen_case_typs_df <- function(url) {
  case_types <- read_html(url) %>%
    html_nodes('.general-content') %>%
    html_nodes('li') %>%
    html_text() %>%
    as_tibble() %>%
    filter(
      str_detect(value, 'cases are travellers') |
        str_detect(value, 'cases are travelers') |
        str_detect(value, 'are community contacts') |
        str_detect(value, 'have no known exposures') |
        str_detect(value, 'are under investigation by local public health')
    ) %>%
    mutate(vv = as.numeric(str_trim(gsub("([0-9]+).*$", "\\1", gsub(",", "", value))))) %>%
    mutate(variable = ifelse(
      str_detect(value, 'travellers'), 'Travellers', ifelse(
        str_detect(value, 'travelers'), 'Travellers', ifelse(
          str_detect(value, 'community contacts'), 'Contacts', ifelse(
            str_detect(value, 'no known exposures'), 'Community', ifelse(
              str_detect(value, 'under investigation'), 'Investigation', ''
            )
          )
        )
      )
    )) %>%
    select('variable', 'vv') %>%
    rename(value = 'vv') %>%
    mutate(date = extract_date_from_url(url))
  return(case_types)
}

# Health Ministry Updates
latest_case_types_df <- lapply(health_min_daily_update_urls$value, gen_case_typs_df) %>%
  bind_rows() %>%
  spread(variable, value)


# update running aggregated Case Types CSV
agg_ct_df <- readr::read_csv('./data/case-types.csv')
max_agg_ct_df_dt <- agg_ct_df %>% summarize(dt = last(date))

update_ct_df <- latest_case_types_df %>% 
  arrange(date) %>%
  filter(date > max_agg_ct_df_dt$dt)

agg_ct_df <- agg_ct_df %>%
  bind_rows(update_ct_df)


readr::write_csv(agg_ct_df, './data/case-types.csv')

# To run a longer time series of updates
all_updates <- lapply(1:27, function(x) 
  extract_health_min_daily_update_urls(
    paste0(health_min_news_updates_url, '&page=', x)
  )
) %>%
  unlist() %>%
  lapply(gen_case_typs_df) %>%
  bind_rows() %>%
  spread(variable, value)

update_ct_df <- all_updates %>% 
  arrange(date) %>%
  filter(date > max_agg_ct_df_dt$dt)

agg_ct_df <- agg_ct_df %>%
  bind_rows(update_ct_df)


readr::write_csv(agg_ct_df, './data/case-types.csv')

# fixing up some old data
agg_ct_df <- readr::read_csv('./data/case-types.csv')
fix_df <- all_updates %>% filter(date >= '2020-10-24' & date <= '2020-11-03')

agg_ct_df <- agg_ct_df %>%
  left_join(fix_df[, c('date', 'Travellers')], by=c('date'), ) %>%
  mutate(Travellers.x = ifelse(!is.na(Travellers.y), Travellers.y, Travellers.x))

agg_ct_df$Travellers.y <- NULL

agg_ct_df <- agg_ct_df %>%
  rename(Travellers = Travellers.x)

readr::write_csv(agg_ct_df, './data/case-types.csv')

################################
################################
# Outbreaks

outbreaks_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/latest-updates'
new_outbreak_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/latest-updates/covid-19-active-outbreaks'

oo <- read_html(new_outbreak_url) %>%
  html_table()
oo1 <- oo[c(1:3, 6)] %>% bind_rows() %>% filter(X1 != 'Location') %>% setNames(c('Location', 'Name', 'Zone', 'DeclarationDate', 'Information'))
oo2 <- oo[4:5] %>% bind_rows() %>% filter(X1 != 'Location') %>% setNames(c('Location', 'Name', 'DeclarationDate', 'Information'))
oot <- oo1 %>% bind_rows(oo2)

ymd <- format(Sys.time(), '%Y%m%d')

readr::write_csv(oot, paste0('./data/outbreaks/outbreaks_', ymd, '.csv'))

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
##################


# read age distribution
dd <- read_html(url) %>%
  html_nodes('.compacttable') %>%
  html_table()

# age distribution table
ages <- dd[[1]][2:nrow(dd[[1]]), ]
names(ages) <- dd[[1]][1, ]

write.csv(ages, paste0('./data/covid_sk_age_dist_', dt, '.csv'), row.names=FALSE)


##################
##################
# Public health orders

# https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/public-health-measures/public-health-ordershttps://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/public-health-measures/public-health-orders

##################
##################
# Vaccine roll-out

# https://www.saskatchewan.ca/-/media/files/coronavirus/vaccine/saskatchewan-covid-19-vaccine-delivery-plan.pdf

# Vaccine approved Dec 9, 2020.
# Phase 0: Pilot of 1,950 doses in Regina
# Phase 1: 202,052 planned administrations of doses to priority populations
  # LTC residents and staff: 30, 584
  # HCW: 10,000 - 15,000
  # Older people: 
    # 80+: 50,302
    # 75-79: 32,474
    # 70-74: 47,343
  # > 50 yrs old in Northern communities: 8,921
  # Pfizer plan was 10,725 doses per week.
# Phase 2: Starting April 2021 admin to general population.



