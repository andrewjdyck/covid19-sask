library(rvest)
library(jsonlite)

# Current date
dt <- as.character(format(Sys.Date(), '%Y%m%d'))
url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/cases-and-risk-of-covid-19-in-saskatchewan'



dd <- read_html(url) %>%
  html_nodes('.compacttable') %>%
  html_table()

# Cases table
cases <- dd[[1]][2:nrow(dd[[1]]), ]
names(cases) <- dd[[1]][1, ]

write.csv(cases, paste0('./data/covid_sk_', dt, '.csv'), row.names=FALSE)

# age distribution table
ages <- dd[[2]][2:nrow(dd[[2]]), ]
names(ages) <- dd[[2]][1, ]

write.csv(ages, paste0('./data/covid_sk_age_dist_', dt, '.csv'), row.names=FALSE)

# testing table
testing <- dd[[3]][2:nrow(dd[[3]]), ]
names(testing) <- dd[[3]][1, ]

write.csv(testing, paste0('./data/covid_sk_testing_', dt, '.csv'), row.names=FALSE)

# Read summary
summ <- read_html(url) %>%
  html_nodes('.general-content')


lst <- html_node(summ, 'ul')
ll <- lst[[1]]

l1 <- ll %>%
  html_text() %>% 
  strsplit(split = "\n") %>%
  unlist()

write(toJSON(l1), paste0('./data/lst_', dt, '.json'))

readdate <- html_nodes(summ, 'p strong')
