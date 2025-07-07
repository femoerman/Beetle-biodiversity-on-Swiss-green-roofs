# Function definitions that manipulate REDCap instruments


#' get_fieldnames_with_choices
#'
#' @param df_dict dataframe containing REDCap data dictionary
#'
#' @return vector with field names with field types that provide choices
#' dropdown, checkbox, radio
#' @export
#'
#' @examples
get_fieldnames_with_choices <- function(df_dict) {
  df_dict %>% 
  filter(field_type %in% c("dropdown", "checkbox", "radio")) %>%
  pull(field_name)
}


#' create_choices_string
#'
#' @param new_choices_vector vector containing the new labels for choices
#' @param start_value numer from which to start the coding numbers
#' @param coding_vector alternatively: numeric vector containing the codes
#' @param usage_manual_update default FALSE; 
#' if TRUE, create string with line break, 
#' otherwise with | ready to use in data dictionary
#'
#' @return string containing all choices together with their coding
#' @export
#'
#' @examples
create_choices_string <- function(new_choices_vector, 
                                  start_value=1, 
                                  coding_vector=Null, 
                                  usage_manual_update=FALSE) {
  if (is.null(coding_vector)) {
    coding_vector <- 
      create_vector_numbering_elements(new_choices_vector, start_value)
  }
  
  if (usage_manual_update) {
    collapse_symbol <- "\n "
  } else {
    collapse_symbol <-  " | "
  }
  
  new_choices_str <- 
    paste0(coding_vector, ", ", new_choices_vector) %>%
    paste(collapse = collapse_symbol)
  
  return(new_choices_str)
}


#' update_data_dict_choices
#'
#' @param df_dict dataframe containing the REDCap data dictionary
#' @param new_choices_vector the vector containing the new choices; 
#' once data collection has started, only appending to the existing ones works
#' @param redcap_field fieldname in data dictionary to be updated
#' @param start_value number from which to start the coding numbers
#' @param coding_vector alternatively: numeric vector containing the codes
#'
#' @return updated data dictionary ready to be imported to REDCap
#' @export
#'
#' @examples
update_data_dict_choices <- function(df_dict, 
                                     new_choices_vector, 
                                     redcap_field,
                                     start_value=1,
                                     coding_vector=NULL) {
  
  new_choices_str <- 
    create_choices_string(new_choices_vector, start_value, coding_vector, FALSE)
  df_dict$select_choices_or_calculations[df_dict_obs$field_name == redcap_field] <- new_choices_str
  
  return(df_dict)
}

