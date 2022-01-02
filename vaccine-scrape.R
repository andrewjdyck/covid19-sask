##################
# Vaccine scraper
##################
##################

latest_updates_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/latest-updates'
# older_updates_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/latest-updates/step-details/news-releases/older-covid-19-news-releases'
# older updates url is deprecated. Notices are now located at the health ministry news pages
health_min_news_updates_url <- 'https://www.saskatchewan.ca/government/news-and-media?text=%22COVID-19+Update%3a%22&ministry=5FD58D569A72474B8D543396985C0409'
# health_min_news_updates_url <- 'https://www.saskatchewan.ca/government/news-and-media?text=%22COVID-19+Update%3a%22&ministry=5FD58D569A72474B8D543396985C0409&page=27'
vaccine_delivery_update_url <- 'https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/covid-19-vaccine/vaccine-delivery-update'

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



get_vaccine_str <- function(url) {
  read_html(url) %>%
    html_nodes('.general-content') %>%
    html_nodes('p') %>%
    html_text() %>%
    as_tibble() %>%
    mutate(vaccine_yn = ifelse(str_detect(value, 'vaccine'), 1, 0)) %>%
    filter(vaccine_yn == 1) %>%
    select(value) %>%
    unlist() %>%
    strsplit("\n") %>%
    as_tibble() %>%
    # mutate(vaccine_yn = ifelse(str_detect(value, 'vaccine'), 1, 0)) %>%
    # mutate(doses_yn = ifelse(str_detect(value, 'doses'), 1, 0)) %>%
    # mutate(total_yn = ifelse(str_detect(value, 'total'), 1, 0)) %>%
    mutate(number_yn = ifelse(str_detect(value, 'number'), 1, 0)) %>%
    filter(number_yn == 1) %>%
    select(value) %>%
    unlist()
}



url <- 'https://www.saskatchewan.ca/government/news-and-media/2021/january/21/covid19-update-for-january-21-29781-vaccines-delivered-227-new-cases-816-new-recoveries-13-new-death'
ss <- get_vaccine_str(url)

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