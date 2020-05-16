library(rvest)
library(readr)

# Downloading CSV & JSON
# Cases
cases_url_csv <- 'https://dashboard.saskatchewan.ca/export/cases/1077.csv'
cases_url_json <- 'https://dashboard.saskatchewan.ca/export/cases/1077.json'
download.file(cases_url_csv, destfile = "./data/1077.csv", method="curl")
download.file(cases_url_json, destfile = "./data/1077.json", method="curl")


# Tests
tests_url_csv <- 'https://dashboard.saskatchewan.ca/export/tests/1054.csv'
tests_url_json <- 'https://dashboard.saskatchewan.ca/export/tests/1054.json'
download.file(tests_url_csv, destfile = "./data/1054.csv", method="curl")
download.file(tests_url_json, destfile = "./data/1054.json", method="curl")

# Hospitalizations
hospitals_url_csv <- 'https://dashboard.saskatchewan.ca/export/hospitalized/1060.csv'
hospitals_url_json <- 'https://dashboard.saskatchewan.ca/export/hospitalized/1060.json'
download.file(hospitals_url_csv, destfile = "./data/1060.csv", method="curl")
download.file(hospitals_url_json, destfile = "./data/1060.json", method="curl")



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


