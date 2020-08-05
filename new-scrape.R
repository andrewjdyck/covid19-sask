library(rvest)
library(readr)

# Downloading CSV & JSON
cases_num <- 1080
test_num <- 1081
hosp_num <- 1082

# Cases
cases_url_csv <- paste0('https://dashboard.saskatchewan.ca/export/cases/', cases_num, '.csv')
cases_url_json <- paste0('https://dashboard.saskatchewan.ca/export/cases/', cases_num, '.json')
download.file(cases_url_csv, destfile = paste0("./data/", cases_num, ".csv"), method="curl")
download.file(cases_url_json, destfile = paste0("./data/", cases_num, ".json"), method="curl")


# Tests
tests_url_csv <- paste0('https://dashboard.saskatchewan.ca/export/tests/', test_num, '.csv')
tests_url_json <- paste0('https://dashboard.saskatchewan.ca/export/tests/', test_num, '.json')
download.file(tests_url_csv, destfile = paste0("./data/", test_num, ".csv"), method="curl")
download.file(tests_url_json, destfile = paste0("./data/", test_num, ".json"), method="curl")

# Hospitalizations
hospitals_url_csv <- paste0('https://dashboard.saskatchewan.ca/export/hospitalized/', hosp_num, '.csv')
hospitals_url_json <- paste0('https://dashboard.saskatchewan.ca/export/hospitalized/', hosp_num, '.json')
download.file(hospitals_url_csv, destfile = paste0("./data/", hosp_num, ".csv"), method="curl")
download.file(hospitals_url_json, destfile = paste0("./data/", hosp_num, ".csv"), method="curl")



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
dd <- lapply(d2$value, gen_case_typs_df) %>%
  bind_rows() %>%
  spread(variable, value)
dd

ddd <- lapply(d1$value, gen_case_typs_df) %>%
  bind_rows() %>%
  spread(variable, value)
ddd



qq <- dd %>% bind_rows(ddd) %>% arrange(date)
