# Define UI for application that imports and shows the beetle data
ui <- fluidPage(
  
  theme = light,
  
  # Application title
  titlePanel("Beetle biodiversity on Swiss green roofs"),
  
  
  
  # Sidebar: put Input Elements here
  # https://shiny.rstudio.com/
  sidebarLayout(
    sidebarPanel( 
      helpText(
        "Overall information of the project is shown",
        "in the 'Summary' tab. For more information on",
        "specific species or specific roofs, input",
        "the roof/species identifier, and explore the",
        "other tabs."
      ),
      
      #Provide an input for the choice of roof
      selectizeInput(
        inputId = "roofs_choice",
        label = h5("Specify whether all roofs (00_None) or a selection (Specify below) should be included"),
        choices=c("00_None", "Specify below"), selected="00_None"),
      
      #Multiple selection input
      multiInput(
        inputId = "roofs_choice_det",
        label = h5("Choose (a) roof(s)"),
        choices="", selected=""),
      
      #Make a reset button for the roof
      actionButton(
        inputId="reset_roofs", label = "Reset roof"
      ),
      
      #Provide an input for the choice of species
      selectizeInput(
        inputId = "species_choice",
        label = h5("Provide the species name"),
                   choices=NULL, selected=NULL
      ),
      
      #Make a reset button for the roof
      actionButton(
        inputId="reset_species", label = "Reset species"
      ),
      
      #Provide an input for the way timeseries are displayed
      radioButtons(
        inputId = "timeseries_choice",
        label = h5("Should timeseries be non-cumulative or cumulative?"),
        choices = list(
          "Non-cumulative" = 1,
          "Cumulative" =2
        )
      ),
      
      #Provide an input for whether data is displayed per roof or per subarea
      radioButtons(
        inputId = "rooflevel_choice",
        label = h5("Should data be displayed per roof or per subarea?"),
        choices = list(
          "Roof" = 1,
          "Subarea" =2
        )
      )
      
      
    ),
    
    # a second panel
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 h3(textOutput("summtext")),
                 tableOutput("summtable"),
                 br(),
                 h3("Total number of observed beetles"),
                 plotOutput("plot.count.sum"),
                 br(),
                 h3("Total number of observed beetle species"),
                 plotOutput("plot.species.sum"),
                 br(),
                 h3("Total number of observed beetle genera"),
                 plotOutput("plot.genus.sum"),
                 br(),
                 h3("Total number of observed beetle families"),
                 plotOutput("plot.family.sum")
        ),
        tabPanel("By roof",
                 
                 h2("Information on beetle biodiversity by roof"),
                 br(),
                 h3("Total number of observed beetles"),
                 plotOutput("plot.count.roof"),
                 br(),
                 h3("Total number of observed beetle species"),
                 plotOutput("plot.species.roof"),
                 br(),
                 h3("Total number of observed beetle genera"),
                 plotOutput("plot.genus.roof"),
                 br(),
                 h3("Total number of observed beetle families"),
                 plotOutput("plot.family.roof"),
                 br(),
                 h3("Shannon index"),
                 plotOutput("plot.shannon.roof"),
                 br(),
                 h3("Simpson index"),
                 plotOutput("plot.simpson.roof"),
                 br(),
                 h3("Species turnover"),
                 plotOutput("plot_turnover_all", height="1000px"),
                 br(),
                 downloadButton('download_turnover_csv',"Download the turnover data as .csv file"),
                 br(),
                 downloadButton('download_turnover_xlsx',"Download the turnover data as .xlsx file"),
                 br()
        ),
        tabPanel("By species",
                
                 h2("Information on beetle population sizes per species (over all roofs)"),
                 h3(textOutput("querytext")),
                 plotOutput("plot.count.spec"),
                 plotOutput("plot.byspecies.roof"),
                 plotOutput("plot.byspecies_subarea")
        ),
        tabPanel("By observation",
                 
                 h2("This tab shows population densities of individual beetle species on specific roofs or subareas"),
                 h3(textOutput("querytext2")),
                 plotOutput("plot.count.spec.roof")
        ),
        tabPanel("Roof list",
                 
                 #Helper text explaining tab
                 h2("Roof list"),
                 helpText(
                   "List of green roofs and the subareas",
                   "sampled within those green roofs.",
                   "Select a single roof (side panel) to get a",
                   "list of subroofs for that roof only."
                 ),
                 
                 plotOutput("roofmap"),
                 br(),
                 
                 downloadButton('download_roof',"Download the filtered roof data as .csv file"),
                 br(),
                 
                 downloadButton('download_roof_xlsx',"Download the filtered roof data as .xlsx file"),
                 br(),
                 
                 #Place the table
                 dataTableOutput("roof_selection")
        ),
        tabPanel("Species list",
                 # This page lists all species alphabetically, and can be refined with family and or genus.
                 
                 #Helper text explaining tab
                 h2("Species list"),
                 helpText(
                   "List of species found on the green roofs",
                   "refine list by entering genus or family.",
                   "Genus choice takes priority over family.",
                   "Press the reset button to go back to the",
                   "complete list of choices."
                 ),
                 
                 #Get input for family
                 selectizeInput(
                   inputId = "family_choice",
                   label = h5("Provide the family"), 
                   choices=NULL, selected=NULL
                 ),
                 
                 #Get input for genus
                 selectizeInput(
                   inputId = "genus_choice",
                   label = h5("Provide the genus"),
                   choices=NULL, selected=NULL
                 ),
                 
                 #Make a reset button
                 actionButton(
                   inputId="reset_choices", label = "Reset"
                 ),
                 
                 #Place the table
                 dataTableOutput("species_selection")
        ),
        
        tabPanel("Export data (raw)",
                 
                 h5("This tab allows the export of parts of the beetle observations dataset, based on specific criteria."),
                 h5("One can select family, genus, or species (priority of lowest taxonomic level), "),
                 h5("roof or subarea (subarea has priority over roof), and start and end date of the observations"),
                 h5("When no choice is provided, all possible values are exported (e.g., all species, all dates, all roofs"),
                 #Get input for family
                 selectizeInput(
                   inputId = "family_choice_export",
                   label = h5("Provide the family"), 
                   choices=NULL, selected=NULL
                 ),
                 
                 #Get input for genus
                 selectizeInput(
                   inputId = "genus_choice_export",
                   label = h5("Provide the genus"),
                   choices=NULL, selected=NULL
                 ),
                 
                 #Provide an input for the choice of species
                 selectizeInput(
                   inputId = "species_choice_export",
                   label = h5("Provide the species name"),
                   choices=NULL, selected=NULL
                 ),
                 
                 #Provide a starting date
                 dateInput(
                   inputId = "startdate_export",
                   label = "Provide a start date",
                   value = Sys.Date()-(200*365),
                   format = "dd-mm-yyyy"
                 ),
                 
                 #Provide an end date
                 dateInput(
                   inputId = "enddate_export",
                   label = "Provide an end date",
                   value = Sys.Date(),
                   format = "dd-mm-yyyy"
                 ),
                 
                 #Provide an input for the choice of roof
                 selectizeInput(
                   inputId = "roofs_choice_export",
                   label = h5("Choose a roof"),
                   choices=NULL, selected=NULL),
                 
                 #Provide an input for the choice of subarea
                 selectizeInput(
                   inputId = "subarea_choice_export",
                   label = h5("Choose a subarea"),
                   choices=NULL, selected=NULL),
                 
                 #Make a reset button
                 actionButton(
                   inputId="reset_choices_export", label = "Reset"
                 ),
                 br(),
                 
                 downloadButton('download_raw',"Download the filtered raw data as .csv file"),
                 br(),
                 downloadButton('download_raw_xlsx',"Download the filtered raw data as .xlsx file"),
                 br(),
                 
                 #Render the selection table
                 dataTableOutput("exporttable_raw")
                 
        ),
        
        tabPanel("Export data (summary)",
                 
                 h5("This tab allows the export of parts of the summarized beetle data, based on specific criteria."),
                 h5("One can select start year and end year"),
                 h5("When no choice is provided, all possible values are exported (all years"),
                 
                 #Provide a starting date
                 dateInput(
                   inputId = "startdate_export_sum",
                   label = "Provide a start date",
                   value = Sys.Date()-(200*365),
                   format = "dd-mm-yyyy"
                 ),
                 
                 #Provide an end date
                 dateInput(
                   inputId = "enddate_export_sum",
                   label = "Provide an end date",
                   value = Sys.Date(),
                   format = "dd-mm-yyyy"
                 ),
                 
                 #Make a reset button
                 actionButton(
                   inputId="reset_choices_export_sum", label = "Reset"
                 ),
                 br(),
                 
                 downloadButton('download_sum',"Download the filtered summary data as .csv file"),
                 br(),
                 
                 downloadButton('download_sum_xlsx',"Download the filtered summary data as .xlsx file"),
                 br(),
                 
                 #Render the selection table
                 dataTableOutput("exporttable_sum")
                 
        ),tabPanel("Import observations in redcap",
                   
                   h5("This tab allows the entry of new data, and import of the new data to REDCap."),
                   h5("Data entry is only possible for users in possesion of an API token."),
                   h5("One can first enter all data, and check the correctness in the table at the bottom,"),
                   h5("before uploading the data to REDCap."),
                   h5("To remove faulty data, just enter the ID in and press the 'Remove' button"),
                   
                   h5("Input data: "),
                   
                   #Provide a date of observation
                   dateInput(
                     label = "Date of observation",
                     inputId = "observation_date",
                     value = Sys.Date(),
                     format = "dd-mm-yyyy"
                   ),
                   
                   #Get input for family
                   selectizeInput(
                     inputId = "family_choice_import",
                     label = h5("Provide the family"), 
                     choices=NULL, selected=NULL
                   ),
                   
                   #Get input for genus
                   selectizeInput(
                     inputId = "genus_choice_import",
                     label = h5("Provide the genus"),
                     choices=NULL, selected=NULL
                   ),
                   
                   #Provide an input for the choice of species
                   selectizeInput(
                     inputId = "species_choice_import",
                     label = h5("Provide the species name"),
                     choices=NULL, selected=NULL
                   ),
                   
                   #Provide an input for the roof_ID
                   selectizeInput(
                     inputId = "roof_choice_import",
                     label = h5("Provide the roof id"),
                     choices=NULL, selected=NULL
                   ),
                   
                   #Provide a count for the observation
                   textInput(label="Number of beetles", 
                             inputId="input_count",
                             value =""),
                   
                   #Provide a comment for the observation
                   textInput(label="Comment", 
                             inputId="input_comment",
                             value =""),
                   
                   #Provide a trap failure for the observation
                   textInput(label="Trap failure", 
                             inputId="input_trap_fail",
                             value =""),
                              
                   
                   #Make a button to add the current record to the data
                   actionButton(
                     inputId="add_to_import", label = "Add record to data"
                   ),
                   
                   #Make a reset button
                   actionButton(
                     inputId="reset_choices_import", label = "Reset choices"
                   ),
                   
                   h5("Remove data: "),
                   #Provide an ID for the observation
                   textInput(label="ID of faulty record", 
                             inputId="input_ID"),
                   
                   #Make a button to remove the faulty record
                   actionButton(
                     inputId="remove_from_import", label = "Remove faulty data"
                   ),
                   
                   h5("Data to import:"),
                   
                   #Render the table with the data to import
                   dataTableOutput("importtable_obs"),
                   
                   h5("Import the data to REDCap"),
                   
                   #Provide an ID for the observation
                   textInput(label="Provide API token", 
                             inputId="input_API"),
                   
                   #Make a button to import the data to the REDCap
                   actionButton(
                     inputId="import_data_to_redcap", label = "Import to REDCap (requires API token)"
                   ),
                   
                   #Write the output message for writing data to REDCap
                   textOutput("att_obs_import"),
                   
                   #Make a button to import the data to the REDCap
                   actionButton(
                     inputId="clear_import_data", label = "Clear the import dataset"
                   )
        
      )
    )
    
  )
)
)