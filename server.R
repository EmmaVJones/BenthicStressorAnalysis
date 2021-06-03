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
  
  
  ##### How To Tab ----------------------------------------------------------------------------------------------------------------------------------------
  # Bring in pre-rendered HTML instruction page. This is built from the TMDLBenthicStressorToolHowTo.Rmd run outside the app itself
  #   to save time and to ensure all HTML formatting comes in correctly. *NOTE* includeHTML() in the ui broke the app
  #   bc of the inherent <body></body> within the ui <body></body> so this server workaround is necessary.
  #output$TMDLBenthicStressorToolHowTo<-renderUI({includeHTML("TMDLBenthicStressorToolHowTo.html")})
  
 
  ##### Data Upload Tab ----------------------------------------------------------------------------------------------------------------------------------
  
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
    userData$stationData <- filter(inputFile(), StationID %in% input$stationSelection) 
    userData$stationStats <- filter(userData$allStats, StationID %in% input$stationSelection)})
  
  
  output$streamOrder_ <- renderUI({req(inputFile(), userData$initialSpatialSuggestions, input$stationSelection)
    list(selectInput("streamOrder", label = h4("1:100k Stream Order"),
                c(" "="NA","First Order", "Second Order", "Third Order", "Fourth Order", "Fifth Order"),
                selected=1),
         helpText("Please use the LAYER NAME on the GIS Staff App (EMBED LINK) to ensure you are referencing the correct
                  1:100k Strahler Order in order to accurately compare your site to the freshwater probabilistic estimates.")) })
  
  # Suggest Ecoregion for selected station
  output$ecoregion_ <- renderUI({req(inputFile(), userData$initialSpatialSuggestions)
    list(selectInput("ecoregion", label=h4("Ecoregion"), 
                choices = c("Piedmont", "Middle Atlantic Coastal Plain", "Northern Piedmont", "Southeastern Plains",          
                            "Blue Ridge", "Ridge and Valley", "Central Appalachians", NA),                
                selected= stationInitialSpatialSuggestions()$EPA_ECO_US_L3NAME),
         helpText("The Middle Atlantic Coastal Plain Level 3 Ecoregion does not have enough freshwater probabilistic sites
                  to calculate estimates, so it cannot be used for further analyses.")) })
  
  # Suggest Basin, SuperBasin for selected station
  output$basinSuperBasin_ <- renderUI({req(inputFile(), userData$initialSpatialSuggestions)
    list(selectInput("basin", label=h4("Basin"), 
                     choices = unique( subbasins$ProbBasin),                
                     selected= stationInitialSpatialSuggestions()$ProbBasin),
         selectInput("superbasin", label=h4("Superbasin (not required)"), 
                     choices = unique( subbasins$ProbSuperBasin),                
                     selected= stationInitialSpatialSuggestions()$ProbSuperBasin)) })
  
  # Require drop downs to be filled in before moving on
  observe({
    shinyjs::toggleState("begin", !is.null(input$stationSelection) && input$stationSelection != "" && input$streamOrder != "NA" && 
                           !is.na(input$ecoregion) && input$ecoregion != "Middle Atlantic Coastal Plain" && !is.na(input$basin ))
    # Save user inputs for other modules
    userData$stationMetadata <- list(StationID = input$stationSelection, 
                                  `Stream Order` = input$streamOrder, 
                                  Ecoregion = input$ecoregion, 
                                  Basin = input$basin, 
                                  SuperBasin = input$superbasin)    })
      
  # Display user input data
  output$inputTable <- DT::renderDataTable({req(userData$stationData, input$begin)
    DT::datatable(userData$stationData %>% 
                    mutate(CollectionDateTime = as.character(CollectionDateTime)),
                  escape=F, rownames = F, selection = 'none',
                  options=list(dom = 'it', scrollX = TRUE, scrollY = "300px",pageLength=nrow(userData$stationData)))})

  # Display summary statistics
  output$summaryStats <- DT::renderDataTable({req(userData$stationStats, input$begin)
    DT::datatable(userData$stationStats,escape=F, rownames = F, selection = 'none',
                  options=list(dom = 'it', scrollX = TRUE, scrollY = "100px",pageLength=nrow(userData$stationStats))) %>% 
      formatRound(columns=c("pH", "Dissolved Oxygen","Total Nitrogen", "Total Phosphorus", "Total Habitat",  "LRBS", "MetalsCCU", 
                            "Specific Conductance", "Total Dissolved Solids", "Sulfate", "Chloride", "Potassium", "Sodium"), digits=3)})
  
  
  ##### Data Summary Tab ------------------------------------------------------------------------------------------------------------
  
  # List to store all percentile tables
  observe({req(userData$stationStats)
    userData$percentiles <- list(pH = percentileTable(userData$stationStats, "pH", userData$stationMetadata),
                                 `Dissolved Oxygen` = percentileTable(userData$stationStats, "Dissolved Oxygen", userData$stationMetadata),
                                 `Total Nitrogen` = percentileTable(userData$stationStats, "Total Nitrogen", userData$stationMetadata),
                                 `Total Phosphorus` = percentileTable(userData$stationStats, "Total Phosphorus", userData$stationMetadata),
                                 `Total Habitat` = percentileTable(userData$stationStats, "Total Habitat", userData$stationMetadata),
                                 LRBS = percentileTable(userData$stationStats, "LRBS", userData$stationMetadata),
                                 MetalsCCU = percentileTable(userData$stationStats, "MetalsCCU", userData$stationMetadata),
                                 `Specific Conductance` = percentileTable(userData$stationStats, "Specific Conductance", userData$stationMetadata),
                                 `Total Dissolved Solids` = percentileTable(userData$stationStats, "Total Dissolved Solids", userData$stationMetadata),
                                 Sulfate = percentileTable(userData$stationStats, "Sulfate", userData$stationMetadata),
                                 Chloride = percentileTable(userData$stationStats, "Chloride", userData$stationMetadata),
                                 Potassium = percentileTable(userData$stationStats, "Potassium", userData$stationMetadata),
                                 Sodium = percentileTable(userData$stationStats, "Sodium", userData$stationMetadata))  })
 
  
  ### Composite Table SubTab -----------------------------------------------------------------------------------------------------------------------------------
  
  # Output Colored datatable
  output$compositeTable <- DT::renderDataTable({req(userData$stationStats)
    datatable(userData$stationStats, extensions = 'Buttons', escape=F, rownames = F,
              options=list(dom='Bt',buttons=list('copy',
                                                 list(extend='csv',filename=paste('CompositeTable_',Sys.Date(),sep='')),
                                                 list(extend='excel',filename=paste('CompositeTable_',Sys.Date(),sep='')),
                                                 list(extend='pdf',orientation='landscape',filename=paste('CompositeTable_',Sys.Date(),sep='')))))%>%
      formatStyle("pH", backgroundColor = styleInterval(pHRiskTable$brks, pHRiskTable$clrs))%>%
      formatStyle("Dissolved Oxygen", backgroundColor = styleInterval(DORiskTable$brks, DORiskTable$clrs)) %>%
      formatStyle("Total Nitrogen", backgroundColor = styleInterval(TNRiskTable$brks, TNRiskTable$clrs))%>%
      formatStyle("Total Phosphorus", backgroundColor = styleInterval(TPRiskTable$brks, TPRiskTable$clrs))%>%
      formatStyle("Total Habitat", backgroundColor = styleInterval(TotHabRiskTable$brks, TotHabRiskTable$clrs))%>%
      formatStyle("LRBS", backgroundColor = styleInterval(LRBSRiskTable$brks, LRBSRiskTable$clrs))%>%
      formatStyle("MetalsCCU", backgroundColor = styleInterval(MetalsCCURiskTable$brks, MetalsCCURiskTable$clrs))%>%
      formatStyle("Specific Conductance", backgroundColor = styleInterval(SpCondRiskTable$brks, SpCondRiskTable$clrs))%>%
      formatStyle("Total Dissolved Solids", backgroundColor = styleInterval(TDSRiskTable$brks, TDSRiskTable$clrs))%>%
      formatStyle("Sulfate", backgroundColor = styleInterval(DSulfateRiskTable$brks, DSulfateRiskTable$clrs))%>%
      formatStyle("Chloride", backgroundColor = styleInterval(DChlorideRiskTable$brks, DChlorideRiskTable$clrs))%>%
      formatStyle("Potassium", backgroundColor = styleInterval(DPotassiumRiskTable$brks, DPotassiumRiskTable$clrs))%>%
      formatStyle("Sodium", backgroundColor = styleInterval(DSodiumRiskTable$brks, DSodiumRiskTable$clrs))
  }, digits=4,border=TRUE)
  
  # Output table of risk categories
  output$bethicStressorColors <- DT::renderDataTable({
    datatable(risk,colnames=c('Risk Category'),rownames=FALSE, options = list(dom="t")) %>% 
      formatStyle('Risk_Category',backgroundColor=styleEqual(brksrisk,clrsrisk))  })
  
  
  # Output Detailed Parameter Population Estimates
  callModule(cdfSubpopSummary, 'parameterSummary', userData$stationData, userData$percentiles, listOfListsOfRiskTables, listOfListsOfCDFsettings)
  
  
  
  
   output$test <- renderPrint({userData$stationMetadata})#userData$stationData})#req(initialSpatialSuggestions(), input$stationSelection)
  #   userData$stationData})#stationInitialSpatialSuggestions()$EPA_ECO_US_L3NAME })#filter(initialSpatialSuggestions(), WQM_STA_ID %in% input$stationSelection)})
  # 
  
  
  
  
  
})