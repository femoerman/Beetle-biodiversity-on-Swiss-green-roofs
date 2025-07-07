

#' create_lookup_from_redcap_dict
#'
#' @param df_dict dataframe containing the REDCap instrument data dictionary
#' @param field_name REDCap field name for which a lookup vector shall be created
#'
#' @return
#' @export
#'
#' @examples
create_lookup_from_redcap_dict <- function(df_dict, field_name) {
  create_lookup <- function(x) {
    x <- trimws(x)
    res <- as.numeric(x[1])
    names(res) <- x[2]
    return(res)
  }

  lookup_values <- df_dict[df_dict$field_name == field_name,]$select_choices_or_calculations
  lookup_values <- unlist(strsplit(lookup_values, "|", fixed=TRUE))
  lookup_values <- strsplit(lookup_values, ",", fixed=TRUE)
  lookup_values <- lapply(lookup_values, create_lookup) %>% unlist
  
  return(lookup_values)
}

#' create_lookup_list_for_choices
#'
#' @param df_dict dataframe containing REDCap data dictionary
#'
#' @return
#' @export
#'
#' @examples
create_lookup_list_for_choices <- function(df_dict) {
  fieldnames_with_choices <- get_fieldnames_with_choices(df_dict_obs)
  lookup_list <- lapply(fieldnames_with_choices, function(x) {
    create_lookup_from_redcap_dict(df_dict_obs, x)
  })
  
  names(lookup_list) <- fieldnames_with_choices
  
  return(lookup_list)
}


#' prepare_data_for_import
#' This function 
#'
#' @param df dataframe containing the records to be imported to REDCap 
#' @param lookup_list a list with all lookup vectors for single/multiple choices
#' @param factor default FALSE; if TRUE convert to factor variables
#'
#' @return
#' @export
#'
#' @examples
prepare_data_for_import <- function(df, lookup_list, factor = FALSE) {
  df <- df %>% as_tibble
  for (fieldname in names(lookup_list)) {
    lookuplist_fieldname <- lookup_list[[fieldname]]
    df <- df %>%
      mutate(!!fieldname := lookuplist_fieldname[df[[fieldname]]])
  } 
  
  if (factor) {
    for (fieldname in names(lookup_list)) {
      lookuplist_fieldname <- lookup_list[[fieldname]]
      print(lookuplist_fieldname)
      print(df)
      df <- df %>%
        mutate(!!fieldname := factor(df[[fieldname]], 
                                     levels = lookuplist_fieldname, 
                                     labels = names(lookuplist_fieldname)))
    } 
  } 
  return(df)
}


#' Import and process beetle data
#' This function reads the beetle data from redcap, and processes it for use. The function will be performed upon startup
#' of the app, and whenever a "reload data" button is pressed by the user
#'
#' @param URL REDCap URL 
#' @param obs_token API token for the beetle observation data
#' @param roof_token API token for the beetle roof data
#' @param spec_token API token for the beetle species data
#'
#' @return
#' @export
#'
#' @examples
import_beetle_data <- function(env.deploy="posit") {
  #1) Read in the data from the REDCap beetle projects. This only happens once

  if(env.deploy!="posit"){
    data.beetles <- read_from_redcap(redcap_url , redcap_ind_beetles_token)
    data.observations <- read_from_redcap(redcap_url, redcap_obs_beetles_token)
    data.roofs <- read_from_redcap(redcap_url, redcap_ind_roofs_token)
  } else {
    data.beetles <- read_from_redcap(Sys.getenv("redcap_url") , Sys.getenv("redcap_ind_beetles_token"))
    data.observations <- read_from_redcap(Sys.getenv("redcap_url"), Sys.getenv("redcap_obs_beetles_token"))
    data.roofs <- read_from_redcap(Sys.getenv("redcap_url"), Sys.getenv("redcap_ind_roofs_token"))
  }
  
  colnames(data.beetles) <- c("record_id", "familia", "genus", "genusauthoryear", "species", "subspecies", "speciesauthor", 
                              "authoryear", "author", "author_abbrev", "comment", "fhl_code", "index_beetles_complete")


  #2) Perform basic data processing. This only happens once
  
  #Extract the year from the observations
  data.observations$year <- as.numeric(format(as.Date(data.observations$date, format="%d/%m/%Y"),"%Y"))
  
  #Create a summarized data by year and roof to sum up the total observations for each species
  data.observations.by_year.by_roof.by_species <- data.observations %>% filter(count>0 & !is.na(speciesauthor)) %>%
    group_by(municipality, roof, speciesauthor, familia, genus, species, year) %>% summarize(total.count = sum(count), 
                                                                                             total.subareas=length(unique(subarea)), )
  
  #Create a summarized data by year and subarea to sum up the total observations for each species
  data.observations.by_year.by_subarea.by_species <- data.observations %>% filter(count>0 & !is.na(speciesauthor)) %>%
    group_by(municipality, roof, subarea, roof_id, speciesauthor, familia, genus, species, year) %>% summarize(total.count = sum(count))
  
  #Create a summary by year and roof, listing the total number of individuals, families, genuses, and species
  data.observations.by_year.by_roof <- summarize_roof_year(data.observations.by_year.by_roof.by_species)
  data.observations.by_year.by_roof$roof_id <- data.observations.by_year.by_roof$roof
  
  
  #Create a summary by year and subarea, listing the total number of individuals, families, genera, and species
  data.observations.by_year.by_subarea <- summarize_subarea_year(data.observations.by_year.by_subarea.by_species)
  
  
  #Create a summarized data by year, listing the total number of individuals, families, genera, and species
  data.observations.by_year <- summarize_year(data.observations.by_year.by_roof.by_species)
  
  #Create a summarized data overall, listing the total number of individuals, families, genuses, and species
  data.observations.summ <- data.observations %>% filter(count>0 & !is.na(speciesauthor)) %>%
    summarize(total.species = length(unique(speciesauthor)), total.genus=length(unique(genus)),
              total.family = length(unique(familia)), total.count = sum(count),
              total.roofs=length(unique(roof)), total.subareas=length(unique(subarea)))
  
  #Create a species list to use
  data.observations.spec <- data.observations %>% filter(count>0 & !is.na(speciesauthor)) %>%
    arrange(familia, genus, speciesauthor) %>% group_by(familia, genus, speciesauthor) %>% 
    summarize(total.count = sum(count),
              total.roofs=length(unique(roof)), total.subareas=length(unique(subarea)))
  
  #Create a dataframe with the observations by species by year, counting the number of sites it has been found
  data.observations.by_year.by_species <- data.observations %>% filter(count>0 & !is.na(speciesauthor)) %>%
    arrange(familia, genus, speciesauthor, year) %>% group_by(familia, genus, speciesauthor, year) %>% 
    summarize(total.count = sum(count),
              total.roofs=length(unique(roof)), total.subareas=length(unique(subarea)))
  
  #Creata a list of roofs to use
  data.roof.list <- data.roofs %>% select(roof, subarea, roof_id, municipality, streetname, east_wgs84, north_wgs84, sizesampledarea, totalcolonizablearea) %>% filter(subarea != "Total")
  #Convert the spatial reference data from CH1903+ to longitude/latitude data
  
  
  
  #Calculate shannon and simpson indices for biodiversity
  
  #Start by making roof_year and subarea_year identifiers for the four datasets
  data.observations.by_year.by_roof <- mutate(data.observations.by_year.by_roof, roofyear = paste(roof, year, sep=""))
  data.observations.by_year.by_subarea <- mutate(data.observations.by_year.by_subarea, subareayear = paste(subarea, year, sep=""))
  data.observations.by_year.by_roof.by_species <- mutate(data.observations.by_year.by_roof.by_species, roofyear = paste(roof, year, sep=""))
  data.observations.by_year.by_subarea.by_species <- mutate(data.observations.by_year.by_subarea.by_species, subareayear = paste(subarea, year, sep=""))
  
  #Create variables to store the shannon and simposon indices in the .by_roof and .by_subarea datasets
  data.observations.by_year.by_roof$shannon <- 0
  data.observations.by_year.by_roof$simpson <- 0
  data.observations.by_year.by_subarea$shannon <- 0
  data.observations.by_year.by_subarea$simpson <- 0
  
  #First per roof per year
  for (i in unique(data.observations.by_year.by_roof$roofyear)){
    #Calculate the proportions for the species, per roof
    counts <- data.observations.by_year.by_roof.by_species[which(data.observations.by_year.by_roof.by_species$roofyear==i), ]$total.count
    prop <- counts/sum(counts)
    
    #Calculate the biodiversity indices per roof
    data.observations.by_year.by_roof[which(data.observations.by_year.by_roof$roofyear==i), "shannon"] <- -sum(prop*log(prop))
    data.observations.by_year.by_roof[which(data.observations.by_year.by_roof$roofyear==i), "simpson"] <- sum(prop^2)
  }
  
  #next per subarea per year
  for (i in unique(data.observations.by_year.by_subarea$subareayear)){
    #Calculate the proportions for the species, per subarea
    counts <- data.observations.by_year.by_subarea.by_species[which(data.observations.by_year.by_subarea.by_species$subareayear==i), ]$total.count
    prop <- counts/sum(counts)
    
    #Calculate the biodiversity indices per subarea
    data.observations.by_year.by_subarea[which(data.observations.by_year.by_subarea$subareayear==i), "shannon"] <- -sum(prop*log(prop))
    data.observations.by_year.by_subarea[which(data.observations.by_year.by_subarea$subareayear==i), "simpson"] <- sum(prop^2)
  }
  
  #Calculate the turnover rates for the roofs/subareas
  data.turnover.roof <- summarize_turnover(data.observations.by_year.by_roof.by_species, data.observations.by_year.by_subarea.by_species, 1)
  data.turnover.subarea <- summarize_turnover(data.observations.by_year.by_roof.by_species, data.observations.by_year.by_subarea.by_species, 2)
  return(list(data.beetles = data.beetles, data.observations = data.observations, data.roofs= data.roofs, 
           data.observations.by_year.by_roof.by_species=data.observations.by_year.by_roof.by_species, 
           data.observations.by_year.by_subarea.by_species=data.observations.by_year.by_subarea.by_species,
           data.observations.by_year.by_roof=data.observations.by_year.by_roof,
           data.observations.by_year.by_subarea=data.observations.by_year.by_subarea,
           data.observations.by_year=data.observations.by_year,
           data.observations.summ=data.observations.summ, data.observations.spec=data.observations.spec,
           data.observations.by_year.by_species=data.observations.by_year.by_species, data.roof.list=data.roof.list,
           data.turnover.roof=data.turnover.roof,
           data.turnover.subarea=data.turnover.subarea))
}
