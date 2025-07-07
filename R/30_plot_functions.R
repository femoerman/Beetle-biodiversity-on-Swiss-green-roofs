# parallelplot_id <- function(df, the_id) {
#   other_text <- ifelse(the_id == 0, "all", "the others")
#    df %>%
#     select(id, happiness, activity) %>%
#     pivot_longer(!id, names_to = "variable") %>%
#     mutate(Legend = factor(ifelse(id == the_id, "you", other_text))) %>%
#     #mutate(is_id = factor(ifelse(id == the_id), "YOU", "others")) %>%
#     ggplot(aes(x=variable, y=value, colour=Legend, group=factor(id))) +
#     geom_path(position = "identity") +
#     geom_point()
# }
# 
# 
# create_overview_plot <- function(df) {
#   df %>%
#     select(happiness, activity) %>%
#     pivot_longer(everything(), names_to = "variable") %>%
#     ggplot(aes(x=variable, y=value)) + 
#     geom_boxplot()
# }


  

#1) Create a plot to show the number of beetles on each roof/subarea, depending on the choice
plot_count_byroof_subarea <- function(df, rooflevel_choice, timeseries_choice){
  
  #In case the dataframe does not contain data, make an empty plot, otherwise plot a normal plot
  if(nrow(df$df)==0){
    plot_output <- ggplot() +  theme_void()
  }else if(rooflevel_choice==1){
     #In case we plot on the roof level, make the following plot
     plot_output <- ggplot(df$df,
            aes(
              x=as.integer(year),
              y = counts,
              colour=roof,
              group=roof
            )
     ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
       ylab(df$countslab) +scale_color_discrete(name="Roof")
  } else if(rooflevel_choice==2){
    
    #Make this plot in case the plot should be displayed on the subarea level
    plot_output <- ggplot(df$df,
           aes(
             x=as.integer(year),
             y = counts,
             colour=rooflabel,
             group=roof_id
           )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab(df$countslab) +scale_color_discrete(name="Roof and subarea")
  }
  
  plot_output
  
}

#2) Create a plot to show the number of species on each roof/subarea, depending on the choice
plot_species_byroof_subarea <- function(df, rooflevel_choice, timeseries_choice){
  
  #In case the dataframe does not contain data, make an empty plot, otherwise plot a normal plot
  if(nrow(df$df)==0){
    plot_output <- ggplot() +  theme_void()
  }else if(rooflevel_choice==1){
    #In case we plot on the roof level, make the following plot
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = species,
                            colour=roof,
                            group=roof
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab(df$specieslab) +scale_color_discrete(name="Roof")
  } else if(rooflevel_choice==2){
    
    #Make this plot in case the plot should be displayed on the subarea level
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = species,
                            colour=rooflabel,
                            group=roof_id
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab(df$specieslab) +scale_color_discrete(name="Roof and subarea")
  }
  
  plot_output
  
}

#3) Create a plot to show the number of genera on each roof/subarea, depending on the choice
plot_genus_byroof_subarea <- function(df, rooflevel_choice, timeseries_choice){
  
  #In case the dataframe does not contain data, make an empty plot, otherwise plot a normal plot
  if(nrow(df$df)==0){
    plot_output <- ggplot() +  theme_void()
  }else if(rooflevel_choice==1){
    #In case we plot on the roof level, make the following plot
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = genus,
                            colour=roof,
                            group=roof
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab(df$genuslab) +scale_color_discrete(name="Roof")
  } else if(rooflevel_choice==2){
    
    #Make this plot in case the plot should be displayed on the subarea level
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = genus,
                            colour=rooflabel,
                            group=roof_id
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab(df$genuslab) +scale_color_discrete(name="Roof and subarea")
  }
  
  plot_output
  
}

#4) Create a plot to show the number of families on each roof/subarea, depending on the choice
plot_family_byroof_subarea <- function(df, rooflevel_choice, timeseries_choice){
  
  #In case the dataframe does not contain data, make an empty plot, otherwise plot a normal plot
  if(nrow(df$df)==0){
    plot_output <- ggplot() +  theme_void()
  }else if(rooflevel_choice==1){
    #In case we plot on the roof level, make the following plot
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = family,
                            colour=roof,
                            group=roof
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab(df$familylab) +scale_color_discrete(name="Roof")
  } else if(rooflevel_choice==2){
    
    #Make this plot in case the plot should be displayed on the subarea level
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = family,
                            colour=rooflabel,
                            group=roof_id
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab(df$familylab) +scale_color_discrete(name="Roof and subarea")
  }
  
  plot_output
  
}

#5) Create a plot to show the shannon index on each roof/subarea, depending on the choice
plot_shannon_byroof_subarea <- function(df, rooflevel_choice, timeseries_choice){
  
  #In case the dataframe does not contain data, make an empty plot, otherwise plot a normal plot
  if(nrow(df$df)==0){
    plot_output <- ggplot() +  theme_void()
  }else if(rooflevel_choice==1){
    #In case we plot on the roof level, make the following plot
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = shannon,
                            colour=roof,
                            group=roof
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab(df$familylab) +scale_color_discrete(name="Roof")
  } else if(rooflevel_choice==2){
    
    #Make this plot in case the plot should be displayed on the subarea level
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = shannon,
                            colour=rooflabel,
                            group=roof_id
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab("Shannon index") +scale_color_discrete(name="Roof and subarea")
  }
  
  plot_output
  
}

#6) Create a plot to show the simpson index on each roof/subarea, depending on the choice
plot_simpson_byroof_subarea <- function(df, rooflevel_choice, timeseries_choice){
  
  #In case the dataframe does not contain data, make an empty plot, otherwise plot a normal plot
  if(nrow(df$df)==0){
    plot_output <- ggplot() +  theme_void()
  }else if(rooflevel_choice==1){
    #In case we plot on the roof level, make the following plot
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = simpson,
                            colour=roof,
                            group=roof
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab(df$familylab) +scale_color_discrete(name="Roof")
  } else if(rooflevel_choice==2){
    
    #Make this plot in case the plot should be displayed on the subarea level
    plot_output <- ggplot(df$df,
                          aes(
                            x=as.integer(year),
                            y = simpson,
                            colour=rooflabel,
                            group=roof_id
                          )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
      ylab("Simpson index") +scale_color_discrete(name="Roof and subarea")
  }
  
  plot_output
  
}

#Create a function to plot the turnover plot
plot_turnover <- function(data_turnover, rooflevel_choice, roof_choice, roof_choice_det){
  
  
  if(rooflevel_choice==1){
    #Filter data based on species choice
    if(roof_choice=="00_None" | is.null(roof_choice) | length(roof_choice_det)<1){
      plotdata <- data_turnover$data.turnover.roof
    } else {
      plotdata <- filter(data_turnover$data.turnover.roof, roof%in%roof_choice_det)
    }
    
    #Plot the figure
    if(nrow(plotdata)==0){
      plot_output <- ggplot() +  theme_void()
    } else {
      plot_output <- ggplot(plotdata, aes(x=prev.year, colour=roof, y=turnover*100, group=1)) + 
        geom_point(size=2.5) + stat_summary(fun.y=sum, geom="line", size=0.5)  + 
        xlab("Years") + ylab("Turnover (%)") + theme(axis.text.x = element_text(angle=90)) + 
        facet_wrap(~roof, ncol = 4, scales = "free_x")  +scale_color_discrete(name="Roof")
    }
  } else {
    #Filter data based on species choice
    if(roof_choice=="00_None" | is.null(roof_choice) | length(roof_choice_det)<1){
      plotdata <- data_turnover$data.turnover.subarea
    } else {
      plotdata <- filter(data_turnover$data.turnover.subarea, roof%in%roof_choice_det)
    }
    
    #Plot the figure
    if(nrow(data_turnover$data.turnover.subarea)==0){
      plot_output <- ggplot() +  theme_void()
    } else {
      plot_output <- ggplot(plotdata, aes(x=prev.year, colour=rooflabel, y=turnover*100, group=1)) + 
        geom_point(size=2.5) + geom_line(mapping=aes(group=rooflabel)) + 
        xlab("Years") + ylab("Turnover (%)") + theme(axis.text.x = element_text(angle=45)) + 
        facet_wrap(~roof, ncol=4, scales = "free_x")  +scale_color_discrete(name="Roof and subarea")
    }
  }
  return(plot_output)
}
