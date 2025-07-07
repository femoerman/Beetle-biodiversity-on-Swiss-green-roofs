# These functions will be available through a package at some time.
# You should not need to change them. If you do, please inform klav@zhaw.ch

# not in operator
`%ni%` <- Negate(`%in%`) 

# REDCap functions (rely on REDCapR package) ------------------------------

# uncomment this functions if you retrieve data from REDCap

#' get_data_dictionary
#'
#' @param url REDCap url
#' @param token personal token for the project
#' @param instrument_name instrument name for which to get the data dictionary
#'
#' @return dataframe resulting from redcap api call
#' @export
#'
#' @examples
get_data_dictionary <- function(url, token, instrument_name) {
  formData <- list("token"=token,
                 content='metadata',
                 format='csv',
                 returnFormat='json',
                   'forms[0]'=instrument_name
  )
  response <- httr::POST(url, body = formData, encode = "form")
  result <- httr::content(response)
  return(result)
}


#' read_from_redcap
#'
#' @param url REDCap url
#' @param token personal token
#'  
#' @return the downloaded data as dataframe
#' @export
#'
#' @examples
read_from_redcap <- function(url, token) {
  result <- REDCapR::redcap_read_oneshot(url, token)$data
  print(result$outcome_message)
  
  return(result)
}