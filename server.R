# Define server logic that 
# * observes inputs
# * based on changed input, decides which data and objects are affected and need to be recalculated/updated
# * performs all calculations
# * builds and updates the output objects in a list called output

server <- function(input, output, session) {
  # THEME RELATED STUFF -----------------------------------------------------
  
  # switch between light and dark mode
  # use observe if you want to perform an action
  # the code in observe is executed immediately when the used input/reactive objects change
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light
  ))

  # PROCESS INPUTS & DEFINE REACTIVE OBJECTS --------------------------------
  
  # REACTIVE EXPRESSIONS
  # use the reactive expression when you want to calculate a value
  # and the calculation is also consuming other reactive values/expressions (e.g. input, session)
  # using reactiveVal would lead to this error:
  # Can't access reactive value 'url_search' outside of reactive consumer.
  # ref: https://mastering-shiny.org/basic-reactivity.html
  # a reactive expression is like a function, so to access its value, you need to call it like a function
  
  # PERFORMANCE CONSIDERATION
  # don't put things in one reactive expression together that can change independently
  # example: we separate reading the data from redcap from filtering it or changing it according to input
  # in that way, the expensive data access from redcap is performed only once
  # and not repeated with any changes of input elements
  
  # REACTIVE VALUES
  # reactiveVal: define one reactive value
  # reactiveValues: define a list of reactive values similar to the list-like object "input"
  
  
  #1) Load the dataset as a list of dataframes
  data <- import_beetle_data(env.deploy)
  
  
  #3) Create output lists for the roofs, species, genus and family dropdown lists
  
    updateMultiInput(session, "roofs_choice_det", choices=unique(data$data.observations.by_year.by_roof$roof))
    updateSelectizeInput(session, "genus_choice", choices=c("00_None", data$data.observations.spec$genus), server=T)
    updateSelectizeInput(session, "family_choice", choices=c("00_None", data$data.observations.spec$familia), server=T)
    updateSelectizeInput(session, "species_choice", choices=c("00_None", data$data.observations.spec$speciesauthor), server=T)
  
    
  
  
  #4) Create a reactive object to get a list of species, after the family and/or genus have been given
    
    #Create the updateable list of species
    output$species_selection <- renderDataTable({
      if(is.null(input$family_choice) | input$family_choice == "00_None"){
        if(is.null(input$genus_choice)| input$genus_choice == "00_None"){
          data$data.observations.spec
        } else {
          data$data.observations.spec[which(data$data.observations.spec$genus==input$genus_choice), ]
        }
      } else {
        if(is.null(input$genus_choice)| input$genus_choice == "00_None"){
          data$data.observations.spec[which(data$data.observations.spec$familia==input$family_choice), ]
        } else {
          data$data.observations.spec[which(data$data.observations.spec$genus==input$genus_choice), ]
        }
      }
    })
    
    #Reset the species list upon pressing the "Reset" button
    #Reset the input fields when the reset button is pushed
    observeEvent(input$reset_choices, {
      updateSelectizeInput(session, "genus_choice", choices=c("00_None", data$data.observations.spec$genus), server=T)
      updateSelectizeInput(session, "family_choice", choices=c("00_None", data$data.observations.spec$familia), server=T)
      updateSelectInput(session, "genus_choice", selected = "00_None")
      updateSelectInput(session, "family_choice", selected = "00_None")
    })
    
    observeEvent(input$genus_choice, {
      if(input$genus_choice != "" & input$genus_choice != "00_None"){
        if(input$family_choice != data$data.observations.spec[which(data$data.observations.spec$genus == input$genus_choice), ]$familia[1]){
          updateSelectizeInput(session, "family_choice", choices= data$data.observations.spec[which(data$data.observations.spec$genus == input$genus_choice), ]$familia[1], server=T)
        }
      } 
    })
    
    observeEvent(input$family_choice, {
      if(input$family_choice != "" & input$family_choice != "00_None"){
        if(input$genus_choice == "" | input$genus_choice == "00_None") {
          genus <- data$data.observations.spec[which(data$data.observations.spec$familia == input$family_choice), ]$genus
          updateSelectizeInput(session, "genus_choice", choices=c("00_None", genus), server=T, selected = "00_None")
        } else {
          genus <- data$data.observations.spec[which(data$data.observations.spec$familia == input$family_choice), ]$genus
          updateSelectizeInput(session, "genus_choice", choices=c("00_None", genus), server=T, selected = input$genus_choice)
        }
       
      }
    })
    
    #5) Create a reactive object to get a list of roofs/subareas, and allow for the reset of the roof via a button, 
    # and plot all roofs on the map according to the selection
    
    #Create a reactive datset to plot the roofs
    plot_roofs <- reactiveValues(df=data$data.roof.list)
    observeEvent(list(input$roofs_choice, input$roofs_choice_det), {
      plot_roofs$df <- if(is.null(input$roofs_choice ) | input$roofs_choice=="00_None"){
        data$data.roof.list
      } else if (length(input$roofs_choice_det)>0){
        data$data.roof.list[which(data$data.roof.list$roof%in% input$roofs_choice_det), ]
      } else {
        data$data.roof.list
      }
    })
    
    
    #Create the updateable list of roofs
    output$roof_selection <- renderDataTable({
      if(is.null(input$roofs_choice ) | input$roofs_choice == "00_None") {
        data$data.roof.list 
      } else  if (length(input$roofs_choice_det)>0){
        data$data.roof.list[which(data$data.roof.list$roof%in% input$roofs_choice_det), ]
      } else {
        data$data.roof.list
      }
    })
    
    
    
    output$download_roof <- downloadHandler(
      filename = function(){"roofdata.csv"}, 
        content = function(filename){
        write_delim(data$data.roof.list, file=filename, delim = ";", quote = "none", na="")
      }
    )
    output$download_roof_xlsx <- downloadHandler(
      filename = function(){"roofdata.xlsx"},
      content = function(filename){
        write_xlsx(data$data.roof.list, path=filename)
      }
    )
    
    #Reset the species list upon pressing the "Reset" button
    observeEvent(input$reset_roofs, {
      updateSelectInput(session, "roofs_choice", selected = "00_None")
      updateMultiInput(session, "roofs_choice_det", selected = NULL)
      
    })
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    output$roofmap <- renderPlot({ggplot(data=world) + geom_sf() + coord_sf(xlim=c(5.807774,10.586826), ylim=c(45.814714, 47.939607), expand=F) + 
        annotation_scale(location = "bl", width_hint = 0.5) + xlab("Coordinates East (WGS84)") + ylab("Coordinates North (WGS84)") +
        annotation_north_arrow(location = "bl", which_north = "true", 
                               pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                               style = north_arrow_fancy_orienteering) +
        geom_point(inherit.aes = F, data = plot_roofs$df, 
                    mapping = aes(x=as.numeric(east_wgs84), y=as.numeric(north_wgs84), colour=roof)) +scale_color_discrete(name="Roof")
    })
    
    
    
    #6) Reset the species choice button
    observeEvent(input$reset_species, {
      updateSelectInput(session, "species_choice", selected = "00_None")
      
    })
    
    
    #7) Create a reactive output table and figure for the "Summary page, and update values if visualization is update
 
    #Create reactive df
    df.sum.plot <- reactiveValues(df=
      data.frame(
      xval = data$data.observations.by_year$year,
      counts=data$data.observations.by_year$total.count,
      species = data$data.observations.by_year$total.species,
      genus = data$data.observations.by_year$total.genus,
      family = data$data.observations.by_year$total.family,
      countslab = "Total number of observed beetles",
      specieslab = "Total number of observed beetle species",
      genuslab = "Total number of observed beetle genera)",
      familylab = "Total number of observed beetle families"
    )
      )
    
    observeEvent(input$timeseries_choice,
                 {
                  temp <- if(input$timeseries_choice==2){
                    data.frame(
                       xval = data$data.observations.by_year$year,
                       counts=data$data.observations.by_year$cumul.count,
                       species = data$data.observations.by_year$cumul.species,
                       genus = data$data.observations.by_year$cumul.genus,
                       family = data$data.observations.by_year$cumul.family,
                       countslab = "Total number of observed \n beetles (Cumulative)",
                       specieslab = "Total number of observed \n beetle species (Cumulative)",
                       genuslab = "Total number of observed \n beetle genera (Cumulative)",
                       familylab = "Total number of observed \n beetle families (Cumulative)"
                     )

                   }else {
                     data.frame(
                       xval = data$data.observations.by_year$year,
                       counts=data$data.observations.by_year$total.count,
                       species = data$data.observations.by_year$total.species,
                       genus = data$data.observations.by_year$total.genus,
                       family = data$data.observations.by_year$total.family,
                       countslab = "Total number of observed \n beetles",
                       specieslab = "Total number of observed \n beetle species",
                       genuslab = "Total number of observed \n beetle genera)",
                       familylab = "Total number of observed \n beetle families"
                     )
                   }
                  
                  df.sum.plot$df <- temp
                 })
    
    #Create a text output for the top of the summary page
    output$summtext <- renderText(ifelse(input$timeseries_choice=="1", "Summary of beetle observations accross all roofs", 
                                         "Summary of beetle observations accross all roofs (Cumulative)"))
    
    #Create an output table for the summary data
    output$summtable <- renderTable({
      mutate(df.sum.plot$df, Year = as.character(xval), `Number of observations` = as.character(counts),
                                   `Number of species` = as.character(species), `Number of genera` = as.character(genus), `Number of families` = as.character(family)) %>% 
        select(Year, `Number of observations`, `Number of species`, `Number of genera`, `Number of families`)
      })
    
    
    #Make the beetle count plot
    output$plot.count.sum <- renderPlot({ggplot(df.sum.plot$df,
                                              aes(
                                                x=xval,
                                                y = counts
                                              )
                                       ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
                                         ylab(df.sum.plot$df$countslab[1])
                                     })


    #Make the beetle species plot
    output$plot.species.sum <- renderPlot({ggplot(df.sum.plot$df,
                                           aes(
                                             x=xval,
                                             y = species
                                           )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
        ylab(df.sum.plot$df$specieslab[1])
    })

    #Make the beetle genera plot
    output$plot.genus.sum <- renderPlot({ggplot(df.sum.plot$df,
                                         aes(
                                           x=xval,
                                           y = genus
                                         )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
        ylab(df.sum.plot$df$genuslab[1])
    })

    #Make the beetle families plot
    output$plot.family.sum <- renderPlot({ggplot(df.sum.plot$df,
                                            aes(
                                              x=xval,
                                              y = family
                                            )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
        ylab(df.sum.plot$df$familylab[1])
    })

    

    #8) Generate an output table and plots for a specific roof. This should be reactive to
    # both the choice of roof, and the way that the data should be represented
    #Create reactive df for the roof based results
    df.roof.plot.byroof <- reactiveValues(df=
                                    data.frame(
                                      xval = data$data.observations.by_year.by_roof$year,
                                      counts=data$data.observations.by_year.by_roof$total.count,
                                      species = data$data.observations.by_year.by_roof$total.species,
                                      genus = data$data.observations.by_year.by_roof$total.genus,
                                      family = data$data.observations.by_year.by_roof$total.family,
                                      shannon = data$data.observations.by_year.by_roof$shannon,
                                      simpson = data$data.observations.by_year.by_roof$simpson
                                      ), countslab = "Total number of observed beetles",
                                    specieslab = "Total number of observed beetle species",
                                    genuslab = "Total number of observed beetle genera)",
                                    familylab = "Total number of observed beetle families"
                                    
    )
    
    
    observeEvent(list(input$timeseries_choice, input$rooflevel_choice, input$roofs_choice, input$roofs_choice_det),
                 {
                   tempdf <- if(is.null(input$roofs_choice)){
                     if(input$rooflevel_choice == 1) {
                       data$data.observations.by_year.by_roof
                     } else {
                       data$data.observations.by_year.by_subarea
                     }
                   } else if(input$roofs_choice == "00_None" | length(input$roofs_choice_det)<1){
                     if(input$rooflevel_choice == 1) {
                       data$data.observations.by_year.by_roof
                     } else {
                       data$data.observations.by_year.by_subarea
                     }
                   } else {
                     if(input$rooflevel_choice == 1) {
                       data$data.observations.by_year.by_roof[which(data$data.observations.by_year.by_roof$roof %in% input$roofs_choice_det), ]
                     } else {
                       data$data.observations.by_year.by_subarea[which(data$data.observations.by_year.by_subarea$roof  %in% input$roofs_choice_det), ]
                     }
                   }
                   if(input$rooflevel_choice==1){
                     tempdf$subarea=""
                   }
                 
                   temp <- if(input$timeseries_choice==2){
                     data.frame(
                       year = tempdf$year,
                       counts=tempdf$cumul.count,
                       species = tempdf$cumul.species,
                       genus = tempdf$cumul.genus,
                       family = tempdf$cumul.family,
                       shannon = tempdf$shannon,
                       simpson = tempdf$simpson,
                       roof = tempdf$roof,
                       roof_id = tempdf$roof_id,
                       roof_subarea = tempdf$subarea,
                       rooflabel=paste(tempdf$roof, tempdf$subarea)
                       
                     )
                     

                   }else {
                     data.frame(
                       year = tempdf$year,
                       counts=tempdf$total.count,
                       species = tempdf$total.species,
                       genus = tempdf$total.genus,
                       family = tempdf$total.family,
                       shannon = tempdf$shannon,
                       simpson = tempdf$simpson,
                       roof = tempdf$roof,
                       roof_id = tempdf$roof_id,
                       roof_subarea = tempdf$subarea,
                       rooflabel=paste(tempdf$roof, tempdf$subarea)
                     )
                     
                   }
                   countslab = ifelse(input$timeseries_choice ==2, "Total number of observed \n beetles (Cumulative)", "Total number of observed \n beetles")
                   specieslab = ifelse(input$timeseries_choice ==2, "Total number of observed \n beetle species (Cumulative)", "Total number of observed \n beetle species")
                   genuslab = ifelse(input$timeseries_choice ==2, "Total number of observed \n beetle genera (Cumulative)", "Total number of observed \n beetle genera")
                   familylab =ifelse(input$timeseries_choice ==2, "Total number of observed \n beetle families (Cumulative)", "Total number of observed \n beetle families")

                   df.roof.plot.byroof$df <- temp
                   df.roof.plot.byroof$countslab <- countslab
                   df.roof.plot.byroof$specieslab <- specieslab
                   df.roof.plot.byroof$genuslab <- genuslab
                   df.roof.plot.byroof$familylab <- familylab
                 })
    
    #Create output plots for the
    #Make the beetle count plot
    output$plot.count.roof <- renderPlot({plot_count_byroof_subarea(df.roof.plot.byroof, input$rooflevel_choice, input$timeseries_choice)})


    #Make the beetle species plot
    output$plot.species.roof <- renderPlot({plot_species_byroof_subarea(df.roof.plot.byroof, input$rooflevel_choice, input$timeseries_choice)})

    #Make the beetle genera plot
    output$plot.genus.roof <- renderPlot({plot_genus_byroof_subarea(df.roof.plot.byroof, input$rooflevel_choice, input$timeseries_choice)})

    #Make the beetle families plot
    output$plot.family.roof <- renderPlot({plot_family_byroof_subarea(df.roof.plot.byroof, input$rooflevel_choice, input$timeseries_choice)})

    #Make the Shannon index plot
    output$plot.shannon.roof <- renderPlot({plot_shannon_byroof_subarea(df.roof.plot.byroof, input$rooflevel_choice, input$timeseries_choice)})

    #Make the Simpson index plot
    output$plot.simpson.roof <-  renderPlot({plot_simpson_byroof_subarea(df.roof.plot.byroof, input$rooflevel_choice, input$timeseries_choice)})
    
    
    #9) Create reactive data by species, and display the results in reactive graphs
    # both the choice of roof, and the way that the data should be represented
    #Create reactive df for the roof based results
    df.spec.plot <- reactiveValues(df=data.frame(
                                              year = data$data.observations.by_year.by_species$year,
                                              counts=data$data.observations.by_year.by_species$total.count,
                                              roofs = data$data.observations.by_year.by_species$total.roofs,
                                              subareas = data$data.observations.by_year.by_species$total.subareas
                                            ), countslab = "Total number of observed beetles",
                                          rooflab = "Total number of roofs where \n the species was found",
                                          subarealab = "Total number of subareas where \n the species was found)",
                                          querytext = "Please provide a species"
                                          
    )
    
    
    observeEvent(list(input$timeseries_choice, input$species_choice),
                 {
                   tempdf <- if(is.null(input$species_choice)){
                     data.frame(
                       year = c(0),
                       counts=c(0),
                       roofs = c(0),
                       subareas = c(0))
                   } else if(input$species_choice == "00_None"){
                     data.frame(
                       year = c(0),
                       counts=c(0),
                       roofs = c(0),
                       subareas = c(0))
                   } else {
                     data.frame(
                       year = data$data.observations.by_year.by_species[which(data$data.observations.by_year.by_species$speciesauthor == input$species_choice), ]$year,
                       counts = data$data.observations.by_year.by_species[which(data$data.observations.by_year.by_species$speciesauthor == input$species_choice), ]$total.count,
                       roofs = data$data.observations.by_year.by_species[which(data$data.observations.by_year.by_species$speciesauthor == input$species_choice), ]$total.roofs,
                       subareas = data$data.observations.by_year.by_species[which(data$data.observations.by_year.by_species$speciesauthor == input$species_choice), ]$total.subareas
                       
                     )
                   }
                     
                   
                   
                   countslab = ifelse(input$timeseries_choice ==2, "Total number of observed \n beetles (Cumulative)", "Total number of observed \n beetles")
                   querytext = if(is.null(input$species_choice)){
                     "Please provide a species"
                   } else if(input$species_choice == "00_None"){
                     "Please provide a species"
                   } else {
                       paste0("Information for species: ", input$species_choice)
                     }
                   
                   
                   df.spec.plot$df <- tempdf
                   df.spec.plot$countslab <- countslab
                   df.spec.plot$querytext <- querytext
                   
                 })
    
    #Make the reactive subtitle
    output$querytext <- renderText(df.spec.plot$querytext)
    
    #Make the reactive plots
    #Make the beetle count plot
    output$plot.count.spec <- renderPlot({if(input$timeseries_choice == 1){
      ggplot(df.spec.plot$df,
             aes(
               x=round(year, digits=0),
               y = counts
             )
      ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
        ylab(df.spec.plot$countslab)
    } else {
      ggplot(df.spec.plot$df,
             aes(
               x=round(year, digits=0),
               y = cumsum(counts)
             )
      ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
        ylab(df.spec.plot$countslab)
    }
      
    })
    
    
    #Make the beetle species plot
    output$plot.byspecies.roof <- renderPlot({ggplot(df.spec.plot$df,
                                                   aes(
                                                     x=round(year, digits=0),
                                                     y = roofs
                                                   )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
        ylab(df.spec.plot$rooflab)
    })
    
    #Make the beetle genera plot
    output$plot.byspecies_subarea <- renderPlot({ggplot(df.spec.plot$df,
                                                 aes(
                                                   x=round(year, digits=0),
                                                   y = subareas
                                                 )
    ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
        ylab(df.spec.plot$subarealab)
    })
    
    
    #10) Make a reactive data frame for the combination of species and roof, and display the data
    #Create reactive df for the roof+species based results
    df.spec.roof.plot <- reactiveValues(df=data.frame(
      year = data$data.observations.by_year.by_species$year,
      counts=data$data.observations.by_year.by_species$total.count
    ), countslab = "Total number of observed beetles",
    querytext = "Please provide a species and roof"
    
    )
    
    #Make object react to changes in inputs
    observeEvent(list(input$timeseries_choice, input$species_choice, input$roofs_choice, input$roofs_choice_det, input$rooflevel_choice),
                 {
                   #Check if plots should be generated (both species and roof name supplied)
                   check_plot <- if(is.null(input$roofs_choice)){
                     F
                   } else {
                     if(is.null(input$species_choice)) {
                       F
                     } else {
                       if(input$roofs_choice == "00_None" | input$species_choice == "00_None" ){
                         F
                       } else {
                         T
                       }
                     }
                   }
                     
                     
                   tempdf <- if(! check_plot){
                     data.frame(
                       year = c(0),
                       counts=c(0),
                       roof=c(""),
                       subareas=c("")
                     )
                   } else {
                     if(input$rooflevel_choice == 1)
                     {
                       data.frame(
                         year = data$data.observations.by_year.by_roof.by_species[which(data$data.observations.by_year.by_roof.by_species$speciesauthor == input$species_choice & 
                                                                                          data$data.observations.by_year.by_roof.by_species$roof %in% input$roofs_choice_det), ]$year,
                         counts = data$data.observations.by_year.by_roof.by_species[which(data$data.observations.by_year.by_roof.by_species$speciesauthor == input$species_choice & 
                                                                                            data$data.observations.by_year.by_roof.by_species$roof  %in% input$roofs_choice_det), ]$total.count,
                         roof = data$data.observations.by_year.by_roof.by_species[which(data$data.observations.by_year.by_roof.by_species$speciesauthor == input$species_choice & 
                                                                                           data$data.observations.by_year.by_roof.by_species$roof  %in% input$roofs_choice_det), ]$roof,
                         subareas = rep("", nrow(data$data.observations.by_year.by_roof.by_species[which(data$data.observations.by_year.by_roof.by_species$speciesauthor == input$species_choice & 
                                                                                                           data$data.observations.by_year.by_roof.by_species$roof  %in% input$roofs_choice_det), ]))
                       )
                     } else {
                       data.frame(
                         year = data$data.observations.by_year.by_subarea.by_species[which(data$data.observations.by_year.by_subarea.by_species$speciesauthor == input$species_choice & 
                                                                                             data$data.observations.by_year.by_subarea.by_species$roof  %in% input$roofs_choice_det), ]$year,
                         counts = data$data.observations.by_year.by_subarea.by_species[which(data$data.observations.by_year.by_subarea.by_species$speciesauthor == input$species_choice & 
                                                                                               data$data.observations.by_year.by_subarea.by_species$roof  %in% input$roofs_choice_det), ]$total.count,
                         roof = data$data.observations.by_year.by_subarea.by_species[which(data$data.observations.by_year.by_subarea.by_species$speciesauthor == input$species_choice & 
                                                                                              data$data.observations.by_year.by_subarea.by_species$roof  %in% input$roofs_choice_det), ]$roof,
                         subareas = data$data.observations.by_year.by_subarea.by_species[which(data$data.observations.by_year.by_subarea.by_species$speciesauthor == input$species_choice & 
                                                                                                 data$data.observations.by_year.by_subarea.by_species$roof  %in% input$roofs_choice_det), ]$subarea
                       )
                     }
                     
                   }
                   
                   
                   
                   countslab = ifelse(input$timeseries_choice ==2, "Total number of observed \n beetles (Cumulative)", "Total number of observed \n beetles")
                   querytext = if(is.null(input$species_choice)){
                     "Please provide a species and roof"
                   } else if(is.null(input$roofs_choice)){
                     "Please provide a species and roof"
                   } else if(input$species_choice == "00_None" | input$roofs_choice == "00_None" ) {
                     "Please provide a species and roof"
                   } else {
                     paste0("Information for species: ", input$species_choice, " on roof(s): ", paste(input$roofs_choice_det, collapse = " & ", sep = ""))
                   }
                   
                   
                   df.spec.roof.plot$df <- tempdf
                   df.spec.roof.plot$countslab <- countslab
                   df.spec.roof.plot$querytext <- querytext
                   
                 })
    
    #Render the reactive plot and text
    #Make the reactive subtitle
    output$querytext2 <- renderText(df.spec.roof.plot$querytext)
    #Make the reactive plot
    output$plot.count.spec.roof <- renderPlot({
      if(input$rooflevel_choice==1){
        if(input$timeseries_choice == 1){
          ggplot(df.spec.roof.plot$df,
                 aes(
                   x=round(year, digits=0),
                   y = counts,
                   colour = roof
                 )
          ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
            ylab(df.spec.roof.plot$countslab) +scale_color_discrete(name="Roof")
        } else {
          ggplot(df.spec.roof.plot$df,
                 aes(
                   x=round(year, digits=0),
                   y = cumsum(counts),
                   colour = roof
                 )
          ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
            ylab(df.spec.roof.plot$countslab) +scale_color_discrete(name="Roof")
        }
        } else {
          if(input$timeseries_choice == 1){
            ggplot(df.spec.roof.plot$df,
                   aes(
                     x=round(year, digits=0),
                     y = counts,
                     colour = subareas
                   )
            ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
              ylab(df.spec.roof.plot$countslab) +scale_color_discrete(name= "Subarea")
          } else {
            ggplot(df.spec.roof.plot$df,
                   aes(
                     x=round(year, digits=0),
                     y = cumsum(counts),
                     colour = subareas
                   )
            ) + geom_point(size=2.5) + geom_line(size=0.5) + xlab("Year of observation") +
              ylab(df.spec.roof.plot$countslab) +scale_color_discrete(name="Subarea")
          }
      }
    })
    
    #11) Create a reactive environment to create output tables for raw data, and export them
    #11.0) Create the selectize values
    updateSelectizeInput(session, "roofs_choice_export", choices=c("00_None", unique(data$data.observations.by_year.by_subarea$roof)), server=T, selected="00_None")
    updateSelectizeInput(session, "subarea_choice_export", choices=c("00_None", unique(data$data.observations.by_year.by_subarea$roof_id)), server=T, selected="00_None")
    updateSelectizeInput(session, "genus_choice_export", choices=c("00_None", data$data.observations.spec$genus), server=T, selected="00_None")
    updateSelectizeInput(session, "family_choice_export", choices=c("00_None", data$data.observations.spec$familia), server=T, selected="00_None")
    updateSelectizeInput(session, "species_choice_export", choices=c("00_None", data$data.observations.spec$speciesauthor), server=T, selected="00_None")
    
    ##11.1 
    #Reset the species list upon pressing the "Reset" button and make the lists reactive to the taxonomic and roof inputs
    observeEvent(input$reset_choices_export, {
      updateSelectizeInput(session, "genus_choice_export", choices=c("00_None", data$data.observations.spec$genus), server=T, selected="00_None")
      updateSelectizeInput(session, "family_choice_export", choices=c("00_None", data$data.observations.spec$familia), server=T, selected="00_None")
      updateSelectizeInput(session, "species_choice_export", choices=c("00_None", data$data.observations.spec$speciesauthor), server=T, selected="00_None")
      updateSelectizeInput(session, "roofs_choice_export", choices=c("00_None", unique(data$data.observations.by_year.by_subarea$roof)), server=T, selected="00_None")
      updateSelectizeInput(session, "subarea_choice_export", choices=c("00_None", unique(data$data.observations.by_year.by_subarea$roof_id)), server=T, selected="00_None")
      updateDateInput(session, "startdate_export", value=Sys.Date()-(200*365))
      updateDateInput(session, "enddate_export", value=Sys.Date())
    })
    
    observeEvent(input$species_choice_export, {

      if(input$species_choice_export %in% data$data.observations$speciesauthor){
        genus <- data$data.observations[which(data$data.observations$speciesauthor == input$species_choice_export), ]$genus[1]
        updateSelectizeInput(session, "genus_choice_export", choices=genus, server=T, selected=genus)
        family <- data$data.observations[which(data$data.observations$speciesauthor == input$species_choice_export), ]$familia[1]
        updateSelectizeInput(session, "family_choice_export", choices=family, server=T, selected=family)
      } 
    })
    
    observeEvent(input$genus_choice_export, {
      if(input$species_choice_export == "00_None" & input$genus_choice_export !="00_None"){
        family <- data$data.observations[which(data$data.observations$genus == input$genus_choice_export), ]$familia[1]
        updateSelectizeInput(session, "family_choice_export", choices=family, server=T, selected=family)
        spec <- data$data.observations[which(data$data.observations$genus == input$genus_choice_export), ]$speciesauthor
        updateSelectizeInput(session, "species_choice_export", choices=c("00_None", spec), server=T, selected="00_None")
      }
    })
    
    observeEvent(input$family_choice_export, {
      
      if(input$genus_choice_export=="00_None" & input$species_choice_export=="00_None" & input$family_choice_export != "00_None"){
        genus <- data$data.observations[which(data$data.observations$familia == input$family_choice_export), ]$genus
        updateSelectizeInput(session, "genus_choice_export", choices=c("00_None", genus), server=T, selected="00_None")
        spec <- data$data.observations[which(data$data.observations$familia == input$family_choice_export), ]$speciesauthor
        updateSelectizeInput(session, "species_choice_export", choices=c("00_None", spec), server=T, selected="00_None")
      }
    })
    
    observeEvent(input$subarea_choice_export, {
      if(input$subarea_choice_export != "" & input$subarea_choice_export != "00_None"){
        if(input$roofs_choice_export != unique(data$data.observations.by_year.by_subarea[which(data$data.observations.by_year.by_subarea$roof_id == input$subarea_choice_export), ]$roof)){
          updateSelectizeInput(session, "roofs_choice_export", choices= data$data.observations.by_year.by_subarea[which(data$data.observations.by_year.by_subarea$roof_id == input$subarea_choice_export), ]$roof[1], server=T)
        }
      } 
    })
    
    observeEvent(input$roofs_choice_export, {
      if(input$roofs_choice_export != "" & input$roofs_choice_export != "00_None"){
        if(input$subarea_choice_export=="00_None" | input$subarea_choice_export=="" | ! input$subarea_choice_export %in% data$data.observations.by_year.by_subarea[which(data$data.observations.by_year.by_subarea$roof == input$roofs_choice_export), ]$roof_id){
          subarea <- data$data.observations.by_year.by_subarea[which(data$data.observations.by_year.by_subarea$roof == input$roofs_choice_export), ]$roof_id
          updateSelectizeInput(session, "subarea_choice_export", choices=c("00_None", subarea), server=T, selected="00_None")
        } else {
          subarea <- data$data.observations.by_year.by_subarea[which(data$data.observations.by_year.by_subarea$roof == input$roofs_choice_export), ]$roof_id
          updateSelectizeInput(session, "subarea_choice_export", choices=c("00_None", subarea), server=T, selected=input$subarea_choice_export)
        }
       
      }
    })
    
    
    
    ##11.2: Create a reactive dataframe to be displayed and exported
    #Create reactive df for the roof+species based results
    df.data.export <- reactiveValues(
      df=data$data.observations
    )
    
    ##11.3: Update the reactive dataframe whenever the inputs are changed
    observeEvent(list(input$family_choice_export, input$genus_choice_export, input$roofs_choice_export, input$species_choice_export, input$startdate_export,
                      input$enddate_export, input$subarea_choice_export),
                 {
                   tempdf <- data$data.observations
                   validate(
                     need(input$startdate_export, "Needs valid start date"),
                     need(input$enddate_export, "Needs valid end date")
                   )
                   tempdf <- tempdf %>% filter(date >= input$startdate_export)
                   
                   if(! input$subarea_choice_export == "00_None"){
                     tempdf <- filter(tempdf, roof_id == input$subarea_choice_export)
                   } else if(! input$roofs_choice_export == "00_None"){
                     tempdf <- filter(tempdf, roof == input$roofs_choice_export)
                   }
                   if(! input$species_choice_export == "00_None"){
                     tempdf <- filter(tempdf, speciesauthor == input$species_choice_export)
                   } else if(! input$genus_choice_export == "00_None") {
                     tempdf <- filter(tempdf, genus == input$genus_choice_export)
                   } else if(! input$family_choice_export == "00_None") {
                     tempdf <- filter(tempdf, familia == input$family_choice_export)
                   }
                   
                   df.data.export$df = tempdf
                 }
    )
    
    #11.4) render output as table and provide a download link for the data as csv
    output$exporttable_raw <- renderDataTable(df.data.export$df)
    output$download_raw <- downloadHandler(
      filename = function(){"rawdata.csv"}, 
      content = function(filename){
        write_delim(df.data.export$df, file=filename, delim = ";", quote = "none", na="")
      }
    )
    output$download_raw_xlsx <- downloadHandler(
      filename = function(){"rawdata.xlsx"}, 
      content = function(filename){
        write_xlsx(df.data.export$df, path=filename)
      }
    )
    
    #12) Create a reactive environment to create output tables for summarized data, and export them
   
    ##12.1 
    #Reset the species list upon pressing the "Reset" button
    observeEvent(input$reset_choices_export_sum, {
      updateDateInput(session, "startdate_export_sum", value=Sys.Date()-(200*365) )
      updateDateInput(session, "enddate_export_sum", value=Sys.Date() )
      
    })
    
    ##12.2: Create a reactive dataframe to be displayed and exported
    #Create reactive df for the roof+species based results
    df.data.export.sum <- reactiveValues(
      df=data$data.observations.by_year.by_subarea
    )
    
    ##12.3: Update the reactive dataframe whenever the inputs are changed
    observeEvent(list(input$startdate_export_sum, input$enddate_export_sum),
                 {
                   validate(
                     need(input$startdate_export_sum, "Needs valid start date"),
                     need(input$enddate_export_sum, "Needs valid end date")
                   )
                   tempdf <- data$data.observations.by_year.by_subarea %>% filter(as.numeric(year) >= as.numeric(format(input$startdate_export_sum, format="%Y")) & as.numeric(year) <= as.numeric(format(input$enddate_export_sum, format="%Y")))
                   df.data.export.sum$df = tempdf
                 }
    )
    
    #12.4) render output as table and provide a download link for the data as csv
    output$exporttable_sum <- renderDataTable(df.data.export.sum$df)
    output$download_sum <- downloadHandler(
      filename = function(){"sumdata.csv"}, 
      content = function(filename){
        write_delim(df.data.export.sum$df, file=filename, delim = ";", quote = "none", na="")
      }
    )
    output$download_sum_xlsx <- downloadHandler(
      filename = function(){"sumdata.xlsx"}, 
      content = function(filename){
        write_xlsx(df.data.export.sum$df, path=filename)
      }
    )
    
    
    #13) Make a reactive environment to enter data and import to REDCap
    
    #13.0) Create the selectize values
    updateSelectizeInput(session, "roof_choice_import", choices=c("", unique(data$data.roofs$roof_id)), server=T, selected="")
    updateSelectizeInput(session, "genus_choice_import", choices=c("", data$data.beetles$genus), server=T, selected="")
    updateSelectizeInput(session, "family_choice_import", choices=c("", data$data.beetles$familia), server=T, selected="")
    updateSelectizeInput(session, "species_choice_import", choices=c("", data$data.beetles$speciesauthor), server=T, selected="")
    
    #13.1) Reset the input fields when the reset button is pushed
    #Reset the species list upon pressing the "Reset" button
    observeEvent(input$reset_choices_import, {
      updateSelectizeInput(session, "genus_choice_import", choices=c("", data$data.beetles$genus), server=T, selected="")
      updateSelectizeInput(session, "family_choice_import", choices=c("", data$data.beetles$familia), server=T, selected="")
      updateSelectizeInput(session, "species_choice_import", choices=c("", data$data.beetles$speciesauthor), server=T, selected="")
      updateSelectizeInput(session, "roof_choice_import", choices=c("", unique(data$data.roofs$roof_id)), server=T, selected="")
      updateDateInput(session, "observation_date", value=Sys.Date() )
      updateTextInput(session, "input_count", value="")
      updateTextInput(session, "input_comment", value="")
      updateTextInput(session, "input_trap_fail", value="")
    })
    
    #13.2) Create reactive functions to update the choices lists, whenever a species/genus/family is selected
        observeEvent(input$species_choice_import, {
          
          if(input$species_choice_import %in% data$data.beetles$speciesauthor & input$species_choice_import != ""){
            genus <- data$data.beetles[which(data$data.beetles$speciesauthor == input$species_choice_import), ]$genus[1]
            updateSelectizeInput(session, "genus_choice_import", choices=genus, server=T, selected=genus)
            family <- data$data.beetles[which(data$data.beetles$speciesauthor == input$species_choice_import), ]$familia[1]
            updateSelectizeInput(session, "family_choice_import", choices=family, server=T, selected=family)
          } 
    })
    
    observeEvent(input$genus_choice_import, {
      if(input$species_choice_import == "" & input$genus_choice_import !=""){
        family <- data$data.beetles[which(data$data.beetles$genus == input$genus_choice_import), ]$familia[1]
        updateSelectizeInput(session, "family_choice_import", choices=family, server=T, selected=family)
        spec <- data$data.beetles[which(data$data.beetles$genus == input$genus_choice_import), ]$speciesauthor
        updateSelectizeInput(session, "species_choice_import", choices=c("", spec), server=T, selected="")
      }
    })

    observeEvent(input$family_choice_import, {
      if(input$genus_choice_import=="" & input$species_choice_import=="" & input$family_choice_import != ""){
        genus <- data$data.beetles[which(data$data.beetles$familia == input$family_choice_import), ]$genus
        updateSelectizeInput(session, "genus_choice_import", choices=c("", genus), server=T, selected="")
        spec <- data$data.beetles[which(data$data.beetles$familia == input$family_choice_import), ]$speciesauthor
        updateSelectizeInput(session, "species_choice_import", choices=c("", spec), server=T, selected="")
      }
      
    })

    #13.3) Turn all the information into a dataframe, that can be displayed
    df.data.import <- reactiveValues(
      df=filter(data$data.observations, year < 1900),
      maxID=1
    )
    
    #13.4) add the new data to the dataframe, whenever the "add record to data" button is pressed
    observeEvent(input$add_to_import,
                 {
                 case <- data.frame(record_id=df.data.import$maxID, municipality = "", roof = "", subarea ="", roof_id ="", date = input$observation_date,
                                    familia="", genus="", species="", author="", speciesauthor="", speciesauthor_old="",
                                    count =input$input_count, comment =input$input_comment, trap_failure =input$input_trap_fail, observations_beetles_complete="1")
                 
                   #First define cases where different taxonomy levels have or have not been defined
                   if (input$family_choice_import != ""){
                     case$familia = input$family_choice_import
                     if(input$genus_choice_import != ""){
                       case$genus = input$genus_choice_import
                       if(input$species_choice_import != ""){
                         case$species = input$species_choice_import
                         spec = filter(data$data.beetles, speciesauthor == input$species_choice_import )
                         case$speciesauthor = spec$speciesauthor[1]
                         case$author = spec$author[1]
                       }
                     }
                   }
                   
                   #Now add info on roof, if provided
                   if(input$roof_choice_import != ""){
                     Roofinfo <- filter(data$data.roofs, roof_id == input$roof_choice_import)
                     case$municipality <- Roofinfo$municipality
                     case$roof=Roofinfo$roof
                     case$subarea <- Roofinfo$subarea
                     case$roof_id <- Roofinfo$roof_id
                   }
                   
                   #Merge the new case with the other cases to add to REDCap
                   df.data.import$df <- rbind(df.data.import$df, case)   
                   
                   #Increase the counter for max ID value used
                   df.data.import$maxID = df.data.import$maxID + 1
                   
                   #Reset all the values after entering the data
                   updateSelectizeInput(session, "genus_choice_import", choices=c("", data$data.beetles$genus), server=T)
                   updateSelectizeInput(session, "family_choice_import", choices=c("", data$data.beetles$familia), server=T)
                   updateSelectizeInput(session, "species_choice_import", choices=c("", data$data.beetles$speciesauthor), server=T)
                   updateSelectInput(session, "species_choice_import", selected = "")
                   updateSelectInput(session, "genus_choice_import", selected = "")
                   updateSelectInput(session, "family_choice_import", selected = "")
                   updateSelectInput(session, "roof_choice_import", selected = "")
                   updateDateInput(session, "observation_date", value=Sys.Date() )
                   updateTextInput(session, "input_count", value="")
                   updateTextInput(session, "input_comment", value="")
                   updateTextInput(session, "input_trap_fail", value="")
                 })
    
    #Display the data frame on the bottom of the page
    output$importtable_obs <- renderDataTable(df.data.import$df)
    
    #Remove the record from the data once the button is pressed to remove it
    observeEvent(input$remove_from_import, {
      if(input$input_ID %in% df.data.import$df$record_id ){
        df.data.import$df <- filter(df.data.import$df, record_id != input$input_ID)
      }
    })
    
    #Try to send data to REDCap when the button is pressed
    attempt.text=""
    observeEvent(input$import_data_to_redcap, {
      #Write to REDCap, and print output text
      attempt.text <- ifelse(nrow(df.data.import$df) > 0, write_data_redcap(df.data.import$df, input$input_API, datatype="observations"), "No data to import")
      output$att_obs_import <- renderText(attempt.text)
    })
    
    #Clear the upload data when the button is pressed
    observeEvent(input$clear_import_data, {
      df.data.import$df <- filter(data$data.observations, year < 1900)
    })
    
    
    #14) Create a reactive object for the turnover rates, and make the turnover plots
    output$plot_turnover_all <- renderPlot(plot_turnover(data, input$rooflevel_choice, input$roofs_choice, input$roofs_choice_det))
    
    #15) Create an export table for turnover rate
    output$download_turnover_csv <- downloadHandler(
      filename = function(){"turnover.csv"}, 
      content = function(filename){
        write_delim(table_turnover(data, input$rooflevel_choice, input$roofs_choice, input$roofs_choice_det), file=filename, delim = ";", quote = "none", na="")
      }
    )
    output$download_turnover_xlsx <- downloadHandler(
      filename = function(){"turnover.xlsx"}, 
      content = function(filename){
        write_xlsx(table_turnover(data, input$rooflevel_choice, input$roofs_choice, input$roofs_choice_det), path=filename)
      }
    )
}