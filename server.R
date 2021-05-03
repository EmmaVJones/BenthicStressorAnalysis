source('global.R')

ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1') %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN))) %>%
  mutate(ProbBasin = case_when(SUBBASIN == 'Big Sandy River' ~ 'Big Sandy',
                               SUBBASIN == 'Chowan River' ~ 'Chowan',
                               SUBBASIN %in% c('James River - Lower', "James River - Middle", "James River - Upper") ~ 'James',
                               SUBBASIN == 'New River' ~ 'New',
                               SUBBASIN == 'Potomac River' ~ 'Potomac',
                               SUBBASIN == 'Shenandoah River' ~ 'Shenandoah',
                               SUBBASIN == 'Rappahannock River' ~ 'Rappahannock',
                               SUBBASIN == 'Roanoke River' ~ 'Roanoke',
                               SUBBASIN == 'Clinch and Powell Rivers' ~ 'Clinch',
                               SUBBASIN == 'Holston River' ~ 'Holston',
                               SUBBASIN == 'York River' ~ 'York',
                               TRUE ~ as.character(NA)),
         ProbSuperBasin = case_when(SUBBASIN %in% c('Big Sandy River','Holston River','Clinch and Powell Rivers') ~ 'Tennessee',
                                    SUBBASIN %in% c('Potomac River', 'Shenandoah River') ~ 'Potomac-Shenandoah',
                                    SUBBASIN %in% c('Rappahannock River', 'York River') ~ 'Rappahannock-York',
                                    TRUE ~ as.character(NA)))



shinyServer(function(input, output, session) {
  # Reactive Value to store all user data
  userData <- reactiveValues()
  
  
  ##### How To Tab ----------------------------------------------------------------------------------------------------------------
  # Bring in pre-rendered HTML instruction page. This is built from the TMDLBenthicStressorToolHowTo.Rmd run outside the app itself
  #   to save time and to ensure all HTML formatting comes in correctly. *NOTE* includeHTML() in the ui broke the app
  #   bc of the inherent <body></body> within the ui <body></body> so this server workaround is necessary.
  #output$TMDLBenthicStressorToolHowTo<-renderUI({includeHTML("TMDLBenthicStressorToolHowTo.html")})
  
 
  ##### Data Upload Tab ------------------------------------------------------------------------------------------------------------
  
  # User Upload- Bring in user manipulated chemistry/field data
  # Download data template
  output$downloadTemplate <- downloadHandler(filename=function(){'template.csv'},
                                             content=function(file){write.csv(template,file,row.names=FALSE)})
  
  # Upload user manipulated site data
  inputFile <- reactive({inFile <- input$siteData
  if(is.null(inFile))
    return(NULL)
  removeUnits_envDataDF(read.csv(inFile$datapath)) %>% # Remove Units for all analysis purposes
    mutate(CollectionDateTime = as.POSIXct(as.character(CollectionDateTime), format =  '%Y-%m-%dT%H:%M:%S'))}) 
  
  # Suggest spatial information based on REST service and spatial joins
  observe({ req(inputFile())
    userData$initialSpatialSuggestions <- spatialSuggestions(inputFile(),  pool, subbasinVAHU6crosswalk, subbasins, ecoregion)
    userData$allStats <- statsFunction(inputFile())})
  
  # User chooses station to work through
  output$stationSelection_ <- renderUI({req(inputFile())
    selectInput('stationSelection', label = h4('Station Selection'), choices = unique(inputFile()$StationID))})
  
  stationInitialSpatialSuggestions <- reactive({req(inputFile(), userData$initialSpatialSuggestions, input$stationSelection)
    filter(userData$initialSpatialSuggestions, WQM_STA_ID %in% input$stationSelection)})
  
  # Save data for later use
  observeEvent(input$begin, {
    userData$siteData <- filter(inputFile(), StationID %in% input$stationSelection) 
    userData$siteStats <- filter(userData$allStats, StationID %in% input$stationSelection)})
  
  
  output$StreamOrder_ <- renderUI({req(inputFile(), userData$initialSpatialSuggestions, input$stationSelection)
    list(selectInput("StreamOrder", label = h4("1:100k Stream Order"),
                c(" "="NA","First Order", "Second Order", "Third Order", "Fourth Order", "Fifth Order"),
                selected=1),
         helpText("Please use the LAYER NAME on the GIS Staff App (EMBED LINK) to ensure you are referencing the correct
                  1:100k Strahler Order in order to accurately compare your site to the freshwater probabilistic estimates.")) })
  
  # Suggest Ecoregion for selected station
  output$Ecoregion_ <- renderUI({req(inputFile(), userData$initialSpatialSuggestions)
    list(selectInput("Ecoregion", label=h4("Ecoregion"), 
                choices = c("Piedmont", "Middle Atlantic Coastal Plain", "Northern Piedmont", "Southeastern Plains",          
                            "Blue Ridge", "Ridge and Valley", "Central Appalachians", NA),                
                selected= stationInitialSpatialSuggestions()$EPA_ECO_US_L3NAME),
         helpText("The Middle Atlantic Coastal Plain Level 3 Ecoregion does not have enough freshwater probabilistic sites
                  to calculate estimates, so it cannot be used for further analyses.")) })
  
  # Suggest Basin, SuperBasin for selected station
  output$BasinSuperBasin_ <- renderUI({req(inputFile(), userData$initialSpatialSuggestions)
    list(selectInput("Basin", label=h4("Basin"), 
                     choices = unique( subbasins$ProbBasin),                
                     selected= stationInitialSpatialSuggestions()$ProbBasin),
         selectInput("Superbasin (not required)", label=h4("Superbasin"), 
                     choices = unique( subbasins$ProbSuperBasin),                
                     selected= stationInitialSpatialSuggestions()$ProbSuperBasin)) })
  
  # Require drop downs to be filled in before moving on
  observe({
    shinyjs::toggleState("begin", !is.null(input$stationSelection) && input$stationSelection != "" && input$StreamOrder != "NA" && 
                           !is.na(input$Ecoregion) && input$Ecoregion != "Middle Atlantic Coastal Plain" && !is.na(input$Basin ))  })
      
  # Display user input data
  output$inputTable <- DT::renderDataTable({req(userData$siteData, input$begin)
    DT::datatable(userData$siteData %>% 
                    mutate(CollectionDateTime = as.character(CollectionDateTime)),
                  escape=F, rownames = F, selection = 'none',
                  options=list(dom = 'it', scrollX = TRUE, scrollY = "300px",pageLength=nrow(userData$siteData)))})

  # Display summary statistics
  output$summaryStats <- DT::renderDataTable({req(userData$siteStats, input$begin)
    DT::datatable(userData$siteStats,escape=F, rownames = F, selection = 'none',
                  options=list(dom = 'it', scrollX = TRUE, scrollY = "100px",pageLength=nrow(userData$siteStats))) %>% 
      formatRound(columns=c("pH","DO","TN", "TP", "TotalHabitat",  "LRBS", "MetalsCCU", 
                            "SpCond", "TDS", "DSulfate", "DChloride", "DPotassium", "DSodium"), digits=3)})
  
  
  # output$test <- renderPrint({#req(initialSpatialSuggestions(), input$stationSelection)
  #   userData$siteData})#stationInitialSpatialSuggestions()$EPA_ECO_US_L3NAME })#filter(initialSpatialSuggestions(), WQM_STA_ID %in% input$stationSelection)})
  # 
  
  
  
})