# functions to create other oupt than plots
# e.g., downloads, tables, datasets, etc.

#' write_data_redcap
#'
#' @param df dataframe with column record_id
#' @param write_key API key to write data to redcap
#' @param datatype defines whether beetle observation data or roof data will be uploaded
#'
#' @return 
#' @export
#'
#' @examples
write_data_redcap <- function(df, write_key, datatype){
  
  #Check if the provided API token is correct
  tryCatch(expr={ sanitize_token(write_key) == write_key},
           error=function(write_key){return("Not a valid API token, please try again")},
           warning=function(write_key){return("Not a valid API token, please try again")},
           finally=function(write_key){return("Not a valid API token, please try again")})
  
    
    #If the API token was valid, first rename the columns to match the REDCap names, and then rename the record_ID's to ensure that no values are overwritten
    {
      #Change the column names based on the type of data
      if(datatype == "observations"){
        colnames(df) <- c("record_id", "gemeinde", "ort", "areal", "bezeichnung", "datum", "familia", "gattung", "art", "autor", "art_autor_korrigiert_jascha",
                          "art_autor_vorher_alex", "anzahl", "kommentar", "ausfall_fallen", "observations_beetles_complete")
      }
      
      #Create a new record_ID for each entry
      df$record_id <- paste(runif(1, 0, 1), Sys.Date(), df$record_id, sep = "_")
      
    }
    
    #Next, write the data to REDCap
    output_message <- tryCatch(expr={
      REDCapR::redcap_write(df, redcap_uri = URL, token = write_key, overwrite_with_blanks = F)
      return("Succesfully attempted to send data to REDCap, check there if data upload was received")}, 
      error=function(df){return("Data upload was unsuccesful")},
      warning=function(df){return("Something may have gone wrong, check data upload online")},
      finally=function(df){return("Something may have gone wrong, check data upload online")})
    
    return(output_message)
}