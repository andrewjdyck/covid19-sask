# Covid Tracker API

library(httr)

#' Download data from covid tracker API
#'
#' Get cases from covid tracker API
#' @param province Province of Canada abbreviation
#' @keywords COVID, tracker, cases
#' @importFrom httr GET content
#' @export
#' @examples
#' get_cases()
#'
get_cases <- function(province='SK') {
  
  url <- 'https://api.covid19tracker.ca/cases'
  
  req <- GET(
    url,
    query=list(province=province)
  )
  response <- content(req)
  return(response)
}



#' Download data from covid tracker API
#'
#' Get fatalities from covid tracker API
#' @param province Province of Canada abbreviation
#' @keywords COVID, tracker, cases
#' @importFrom httr GET content
#' @export
#' @examples
#' get_fatalities()
#'
get_fatalities <- function(province='SK') {
  
  url <- 'https://api.covid19tracker.ca/fatalities'
  
  req <- GET(
    url,
    query=list(province=province)
  )
  response <- content(req)
  return(response)
}

