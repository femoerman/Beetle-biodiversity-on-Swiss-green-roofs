


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


#' import_in_redcap
#' import data using the REDCapR package
#'
#' @param df dataframe to be imported
#' @param url REDCap url
#' @param token personal token
#'
#' @return result of REDCapR function call
#' @export
#'
#' @examples
import_in_redcap <- function(df, url, token) {
  res <- REDCapR::redcap_write(df, redcap_uri = url, token = token)
  return(res)
}

# import_observations <- function(df, url, token, delim=CSV_DELIM) {
#   formData <- list("token"=token,
#                    content='record',
#                    format='csv',
#                    type='flat',
#                    csvDelimiter=delim,
#                    overwriteBehavior='overwrite',
#                    forceAutoNumber='true',
#                    data=data_str,
#                    returnContent='auto_ids',
#                    returnFormat='json'
#   )
#   response <- httr::POST(url, body = formData, encode = "form")
#   result <- httr::content(response)
#   return(result)
# }


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


#' delete_all_records from a project
#'
#' @param url REDCap url
#' @param token personal token
#'
#' @return response of POST command
#' @export
#'
#' @examples
delete_all_records <- function(url, token) {
  formData <- list("token"=token,
                   action='delete',
                   content='record'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  result <- httr::content(response)
  return(result)
}