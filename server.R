# source('global.R')
# 
# ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
# subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
#   rename('SUBBASIN' = 'SUBBASIN_1') %>%
#   mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN))) %>%
#   mutate(ProbBasin = case_when(SUBBASIN == 'Big Sandy River' ~ 'Big Sandy',
#                                SUBBASIN == 'Chowan River' ~ 'Chowan',
#                                SUBBASIN %in% c('James River - Lower', "James River - Middle", "James River - Upper") ~ 'James',
#                                SUBBASIN == 'New River' ~ 'New',
#                                SUBBASIN == 'Potomac River' ~ 'Potomac',
#                                SUBBASIN == 'Shenandoah River' ~ 'Shenandoah',
#                                SUBBASIN == 'Rappahannock River' ~ 'Rappahannock',
#                                SUBBASIN == 'Roanoke River' ~ 'Roanoke',
#                                SUBBASIN == 'Clinch and Powell Rivers' ~ 'Clinch',
#                                SUBBASIN == 'Holston River' ~ 'Holston',
#                                SUBBASIN == 'York River' ~ 'York',
#                                TRUE ~ as.character(NA)),
#          ProbSuperBasin = case_when(SUBBASIN %in% c('Big Sandy River','Holston River','Clinch and Powell Rivers') ~ 'Tennessee',
#                                     SUBBASIN %in% c('Potomac River', 'Shenandoah River') ~ 'Potomac-Shenandoah',
#                                     SUBBASIN %in% c('Rappahannock River', 'York River') ~ 'Rappahannock-York',
#                                     TRUE ~ as.character(NA)))
# subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
#   filter(!is.na(SubbasinVAHU6code)) %>%
#   mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN))
# geospatialTemplate <- readRDS('geospatialTemplate.RDS')


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
  removeUnits_envDataDF(read.csv(inFile$datapath)) }) # Remove Units for all analysis purposes
  
  # Suggest spatial information based on REST service and spatial joins
  initialSpatialSuggestions <- reactive({req(inputFile())
    spatialSuggestions(inputFile()) })
  
  # User chooses station to work through
  output$stationSelection_ <- renderUI({req(inputFile())
    selectInput('stationSelection', label = h4('Station Selection'), choices = unique(inputFile()$StationID))})
  
  stationInitialSpatialSuggestions <- reactive({req(inputFile(), initialSpatialSuggestions(), input$stationSelection)
    filter(initialSpatialSuggestions(), WQM_STA_ID %in% input$stationSelection)})
  
  output$StreamOrder_ <- renderUI({req(inputFile(), initialSpatialSuggestions(), input$stationSelection)
    selectInput("StreamOrder", label = h4("1:100k Stream Order"),
                c(" "="NA","First Order", "Second Order", "Third Order", "Fourth Order", "Fifth Order"),
                selected=1)})
  
  # Suggest Ecoregion for selected station
  output$Ecoregion_ <- renderUI({req(inputFile(), initialSpatialSuggestions())
    list(selectInput("Ecoregion", label=h4("Ecoregion"), 
                choices = c("Piedmont", "Middle Atlantic Coastal Plain", "Northern Piedmont", "Southeastern Plains",          
                            "Blue Ridge", "Ridge and Valley", "Central Appalachians", NA),                
                selected= stationInitialSpatialSuggestions()$EPA_ECO_US_L3NAME),
         helpText("The Middle Atlantic Coastal Plain Level 3 Ecoregion does not have enough freshwater probabilistic sites
                  to calculate estimates, so it cannot be used for further analyses.")) })
  
  # Suggest Basin, SuperBasin for selected station
  output$BasinSuperBasin_ <- renderUI({req(inputFile(), initialSpatialSuggestions())
    list(selectInput("Basin", label=h4("Basin"), 
                     choices = unique( subbasins$ProbBasin),                
                     selected= stationInitialSpatialSuggestions()$ProbBasin),
         selectInput("Superbasin", label=h4("Superbasin"), 
                     choices = unique( subbasins$ProbSuperBasin),                
                     selected= stationInitialSpatialSuggestions()$ProbSuperBasin)) })
  
  # Require drop downs to be filled in before moving on
  observe({
    shinyjs::toggleState("begin", !is.null(input$stationSelection) && input$stationSelection != "" && input$StreamOrder != "NA" && 
                           !is.na(input$Ecoregion) && input$Ecoregion != "Middle Atlantic Coastal Plain" && !is.na(input$Basin ))  })
      
  
  
  
  
  
  output$test <- renderPrint({req(initialSpatialSuggestions(), input$stationSelection)
    input$StreamOrder})#stationInitialSpatialSuggestions()$EPA_ECO_US_L3NAME })#filter(initialSpatialSuggestions(), WQM_STA_ID %in% input$stationSelection)})
  
  
  
  
  # # Display user input data
  # output$inputTable <- DT::renderDataTable({
  #   DT::datatable(inputFile(),escape=F, rownames = F,
  #                 options=list(scrollX = TRUE, scrollY = "300px",pageLength=nrow(inputFile())))})
  # 
  # # Calculate statistics on input table
  # stats <- reactive({
  #   req(input$siteData)
  #   
  #   # Deal with columns of only NA coming in as logical
  #   dat <- inputFile() %>% select(-(Temp))
  #   dat <- japply( dat, which(sapply(dat, class)=="logical"), as.numeric )
  #   
  #   datamean <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
  #     summarise_all(funs(format(mean(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Average")
  #   datamean[datamean %in% c("NaN","NA")] <- NA
  #   datamedian <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
  #     summarise_all(funs(format(median(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Median")
  #   datamedian[datamedian %in% c("NaN","NA")] <- NA
  #   data_all <- rbind(datamean,datamedian)%>%select(Statistic,everything())
  #   return(data_all)
  # })
  # # Save objects to reactive values for better organization, keep stats() separate to enable easy req() calls 
  # #   for later functions
  # observe(userData$stats <- stats())
  # 
  # observe(userData$stats_wTemp <- inputFile())
  
  
})