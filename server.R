source('global.R')

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
  removeUnits_envDataDF(read.csv(inFile$datapath)) # Remove Units for all analysis purposes
  })
  
  # Display user input data
  output$inputTable <- DT::renderDataTable({
    DT::datatable(inputFile(),escape=F, rownames = F,
                  options=list(scrollX = TRUE, scrollY = "300px",pageLength=nrow(inputFile())))})
  
  # Calculate statistics on input table
  stats <- reactive({
    req(input$siteData)
    
    # Deal with columns of only NA coming in as logical
    dat <- inputFile() %>% select(-(Temp))
    dat <- japply( dat, which(sapply(dat, class)=="logical"), as.numeric )
    
    datamean <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
      summarise_all(funs(format(mean(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Average")
    datamean[datamean %in% c("NaN","NA")] <- NA
    datamedian <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
      summarise_all(funs(format(median(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Median")
    datamedian[datamedian %in% c("NaN","NA")] <- NA
    data_all <- rbind(datamean,datamedian)%>%select(Statistic,everything())
    return(data_all)
  })
  # Save objects to reactive values for better organization, keep stats() separate to enable easy req() calls 
  #   for later functions
  observe(userData$stats <- stats())
  
  observe(userData$stats_wTemp <- inputFile())
  
  
})