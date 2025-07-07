# put here functions that transform your data

# examples are

#' add_id_to_df
#'
#' @param df dataframe with columns key and record_id
#'
#' @return dataframe with additional id column
#' @export
#'
#' @examples
add_id_to_df <- function(df) {
  df <- df %>%
    mutate(id = create_id(key, record_id))
  return(df)
}

#' clean_columns
#' transform numeric columns to integer columns
#'
#' @param df dataframe
#'
#' @return dataframe with all numeric columns converted to integer columns
#' @export
#'
#' @examples
clean_columns <- function(df) {
  df <- df %>%
    mutate_if(is.numeric, as.integer)
  return(df)
}

#' get_data_for_id
#' filter for a specific id
#'
#' @param df dataframe with id column
#' @param the_id the id to filter for
#'
#' @return
#' @export
#'
#' @examples
get_data_for_id <- function(df, the_id) {
  if (the_id == 0)
    return(df)
  
  df <- df %>% 
    filter(id == the_id)
  return(df)
}

reverse_items_in_df <- function(df, reversed_items, max_value){
  # x <- matrix(ncol = length(reversed_items), nrow= 1)
  # colnames(x) <- reversed_items
  # df_bfi <- cbind(df_bfi, x)
  # for (i in 1:length(reversed_items)){
  #   df_bfi[,reversed_items[i]] <- max_value- df_bfi[,original_to_reverse[i]]
  # }
  
  # tidyverse version
  df_res <- df %>%
    mutate_at(reversed_items, reverse_item, max_value)
  
  return(df_res)
}

standardize_bfi <- function(df_bfi, df_bfi_scales, df_bfi_norms) {
  # scale values - z-transformation
  # [klav] ideally, this should be done with dplyr
  x <- matrix(ncol = dim(df_bfi_scales)[1], nrow= dim(df_bfi)[1])
  colnames(x) <- df_bfi_scales$Scale
  df_bfi <- cbind(df_bfi, x)
  for (i in 1:dim(df_bfi_scales)[1]){
    my_command <- paste0("df_bfi$", df_bfi_scales[i,"Scale"], " <- apply(select(df_bfi,", df_bfi_scales[i,"Item"], "),1,mean, na.rm=TRUE)")
    eval(parse(text=my_command)) # is there a way to do this without eval??
  }
  names_scale <- df_bfi_norms[["Scale"]]
  
  vector_scales <- df_bfi[,names_scale]
  standardized_scales <- (vector_scales - df_bfi_norms$M_All)/df_bfi_norms$SD_All
  standardized_scales <- data.frame(scale = df_bfi_norms$EN, value = unlist(standardized_scales))
  standardized_scales$scale <- factor(standardized_scales$scale, levels = df_bfi_norms$EN)
  
  return(standardized_scales)
}


#Create a function to summarize the observation data by roof and year
summarize_roof_year <- function(df){
  df %>% ungroup() %>% arrange(year, roof, municipality) %>% 
    group_by(roof, municipality, year) %>% 
    mutate(spec_dupl=!duplicated(speciesauthor), genus_dupl=!duplicated(genus), family_dupl=!duplicated(familia)) %>% 
    group_by(roof, municipality, year) %>% 
    summarize(total.species=length(speciesauthor), total.genus=length(unique(genus)), total.family=length(unique(familia)),
              spec_dupl=sum(spec_dupl), genus_dupl=sum(genus_dupl), family_dupl=sum(family_dupl), 
              total.count=sum(total.count)) %>%
    group_by(roof, municipality) %>% 
    summarize(year=year, 
              total.species=total.species, total.genus=total.genus, total.family=total.family,
              cumul.species=cumsum(spec_dupl), cumul.genus=cumsum(genus_dupl), cumul.family=cumsum(family_dupl),
              total.count=total.count, cumul.count=cumsum(total.count)) %>%
    ungroup()
}

#Create a function to summarize the observation data by subarea and year
summarize_subarea_year <- function(df){
  df %>% ungroup() %>% arrange(year, roof, subarea, municipality) %>% 
    group_by(roof, subarea, municipality, roof_id, year) %>% 
    mutate(spec_dupl=!duplicated(speciesauthor), genus_dupl=!duplicated(genus), family_dupl=!duplicated(familia)) %>% 
    group_by(roof, subarea, municipality, roof_id, year) %>% 
    summarize(total.species=length(speciesauthor), total.genus=length(unique(genus)), total.family=length(unique(familia)),
              spec_dupl=sum(spec_dupl), genus_dupl=sum(genus_dupl), family_dupl=sum(family_dupl), 
              total.count=sum(total.count)) %>%
    group_by(roof, subarea, municipality, roof_id) %>% 
    summarize(year=year, 
              total.species=total.species, total.genus=total.genus, total.family=total.family,
              cumul.species=cumsum(spec_dupl), cumul.genus=cumsum(genus_dupl), cumul.family=cumsum(family_dupl),
              total.count=total.count, cumul.count=cumsum(total.count)) %>%
    ungroup()
}

#Create a function to summarize the observation data by year
summarize_year <- function(df){
  df %>% ungroup() %>% arrange(year) %>% 
    group_by(year, familia, genus, speciesauthor) %>% summarize(total.count=sum(total.count)) %>% ungroup() %>%
    mutate(spec_dupl=!duplicated(speciesauthor), genus_dupl=!duplicated(genus), family_dupl=!duplicated(familia)) %>% 
    ungroup() %>% group_by(year) %>%
    summarize(total.species=length(speciesauthor), total.genus=length(unique(genus)), total.family=length(unique(familia)),
              spec_dupl=sum(spec_dupl), genus_dupl=sum(genus_dupl), family_dupl=sum(family_dupl), 
              total.count=sum(total.count)) %>% ungroup() %>%
    mutate(year=year, total.species=total.species, total.genus=total.genus, total.family=total.family, cumul.count=cumsum(total.count),
              cumul.species=cumsum(spec_dupl), cumul.genus=cumsum(genus_dupl), cumul.family=cumsum(family_dupl)) %>%
    ungroup() %>% select( -spec_dupl, -genus_dupl, -family_dupl)
}

#Create a function to filter and process the data to show the turnover rate between each two subsequent years
summarize_turnover <- function(data_turn_roof, data_turn_subarea, rooflevel_choice){
  #In case the rooflevel choice is 1, work with the roof data
  if(rooflevel_choice==1){
    data.turnover <- data_turn_roof %>% group_by(year, roof) %>% summarize(spec_list=list(unique(speciesauthor))) %>% arrange(roof, year) %>%
      ungroup() %>% mutate(turnover=0, prev.year="")
    data.turnover[1, "turnover"] <- NA
    data.turnover[1, "prev.year"] <- NA
    for (i in 2:nrow(data.turnover)){
      prev=data.turnover[i-1, ]
      curr = data.turnover[i, ]
      turnover = (sum(! unlist(curr$spec_list) %in% unlist(prev$spec_list)) + sum(! unlist(prev$spec_list) %in% unlist(curr$spec_list)))/(length(unlist(curr$spec_list))+length(unlist(prev$spec_list)))
      if(prev$roof==curr$roof){
        data.turnover[i, "prev.year"] <- paste0(as.character(prev$year), "-", as.character(curr$year))
        data.turnover[i, "turnover"] <- turnover
      }
      else{
        data.turnover[i, "prev.year"] <- NA
        data.turnover[i, "turnover"] <- NA
      }
    }
  } else {
    data.turnover <- data_turn_subarea %>% group_by(year, roof, roof_id, subarea) %>% summarize(spec_list=list(unique(speciesauthor))) %>% arrange(roof, subarea, roof_id, year) %>%
      ungroup() %>% mutate(turnover=0, prev.year="")
    data.turnover[1, "turnover"] <- NA
    data.turnover[1, "prev.year"] <- NA
    for (i in 2:nrow(data.turnover)){
      prev=data.turnover[i-1, ]
      curr = data.turnover[i, ]
      turnover = (sum(! unlist(curr$spec_list) %in% unlist(prev$spec_list)) + sum(! unlist(prev$spec_list) %in% unlist(curr$spec_list)))/(length(unlist(curr$spec_list))+length(unlist(prev$spec_list)))
      if(prev$roof_id==curr$roof_id){
        data.turnover[i, "prev.year"] <- paste0(as.character(prev$year), "-", as.character(curr$year))
        data.turnover[i, "turnover"] <- turnover
      }
      else{
        data.turnover[i, "prev.year"] <- NA
        data.turnover[i, "turnover"] <- NA
      }
    }
  }
  data.turnover <- filter(data.turnover, !is.na(turnover)) %>% select(-spec_list)
  data.turnover$rooflabel <- paste(data.turnover$roof, data.turnover$subarea, sep=" ")
  return(data.turnover)
}

#Create a function to generate an output table for the turnover rate
table_turnover <- function(data_turnover, rooflevel_choice, roof_choice, roof_choice_det){
  if(rooflevel_choice==1){
    #Filter data based on species choice
    if(roof_choice=="00_None" | is.null(roof_choice) | length(roof_choice_det)<1){
      plotdata <- data_turnover$data.turnover.roof
    } else {
      plotdata <- filter(data_turnover$data.turnover.roof, roof%in%roof_choice_det)
    }
    
  } else {
    #Filter data based on species choice
    if(roof_choice=="00_None" | is.null(roof_choice) | length(roof_choice_det)<1){
      plotdata <- data_turnover$data.turnover.subarea
    } else {
      plotdata <- filter(data_turnover$data.turnover.subarea, roof%in%roof_choice_det)
    }
  }
  return(plotdata)
}
