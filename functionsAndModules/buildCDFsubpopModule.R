
parameterSummaryTabUI <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('table_Site')),
    DT::dataTableOutput(ns('table')),br()
  )
}

parameterSummaryTab <- function(input,output,session, percentileData, unit, colorList){
  output$table_Site <- DT::renderDataTable({
    datatable(percentileData[1,],colnames = c('StationID',paste('Average (',unit,')',sep=""),paste('Median (',unit,')',sep="")),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(colorList$brks, colorList$clrs))})
  output$table <- DT::renderDataTable({datatable(percentileData[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
}
# output$riskTable <- DT::renderDataTable({
#   datatable(parameterRiskTable$Data,
#             colnames=parameterRiskTable$ColNames,rownames = F) %>%
#     formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(parameterRiskTable$StyleEqual1,parameterRiskTable$StyleEqual2))})
# 
# output$dataset <- renderUI({
#   selectInput(session$ns("dataset_"),"Select Dataset to Plot",percentileData$Statistic[2:5])
# })
# output$plot <- renderPlot({
#   if(is.null(input$dataset_))
#     return(NULL)
#   cdfplot(prettyParameterName, parameter,input$dataset_,percentileData, CDFsettings)
# })


cdfSubpopSummaryUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('parameterChoiceUI')),
    #verbatimTextOutput(ns('testtest')),
    DT::dataTableOutput(ns('table_Site')),
    br(),
    fluidRow(
      column(4,DT::dataTableOutput(ns('riskTable'))),
      column(8,uiOutput(ns("dataset_")),
             plotOutput(ns('plot')))),
    br(), br(),
    
  )
}


cdfSubpopSummary <- function(input,output,session, stationData, percentiles, riskTables, cdfsettings){#prettyParameterName, parameter, percentileData, parameterRiskTable, CDFsettings){
  ns <- session$ns
  
  output$parameterChoiceUI <- renderUI({
    # just use parameters with data
    choices <- pivot_longer(stationData, cols= pH:Sodium, names_to = 'variable', values_to = 'result') %>% 
      filter(!is.na(result)) %>% distinct(variable) %>% pull()
    selectInput(ns("parameterChoice"), "Choose a parameter to investigate further.", choices = choices)  })
  
  parameterRiskTable <- reactive({req(input$parameterChoice)
    riskTables[[paste0(input$parameterChoice,'RiskTable')]] })
  
  parameterCDFsettings <- reactive({req(input$parameterChoice)
    cdfsettings[[paste0(input$parameterChoice,'settingsCDF')]] })
  
  output$table_Site <- DT::renderDataTable({
    z <- percentiles[[input$parameterChoice]][1,] %>%
      rename('StationID' = 'Statistic', 'Station Average' = 'Average', 'Station Median' = 'Median') %>%
      bind_cols(percentiles[[input$parameterChoice]][2:nrow(percentiles[[input$parameterChoice]]),] %>%
                  rename('Subpopulation' = 'Statistic', 'CDF Estimate Based on Average' = 'Average',
                         'CDF Estimate Based on Median' = 'Median'))
    datatable(z, rownames = F, options = list(dom = 't'),selection = 'none') %>%
      formatRound(c("Station Average","Station Median", 'CDF Estimate Based on Average', 'CDF Estimate Based on Median'), digits = 2) %>% 
      formatStyle(c("Station Average","Station Median"), backgroundColor = styleInterval(parameterRiskTable()$brks, parameterRiskTable()$clrs))
    })

  output$riskTable <- DT::renderDataTable({
      datatable(parameterRiskTable()$Data, colnames=parameterRiskTable()$ColNames, rownames = F, options = list(dom = 't'),selection = 'none') %>%
        formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(parameterRiskTable()$StyleEqual1,parameterRiskTable()$StyleEqual2))})
  
  # Choose a subpopulation to plot
  output$dataset_ <- renderUI({
    selectInput(ns("dataset"),"Select Dataset to Plot", percentiles[[input$parameterChoice]]$Statistic[2:nrow(percentiles[[input$parameterChoice]])])  })
  
  output$plot <- renderPlot({ req(input$parameterChoice, input$dataset, nrow(percentiles[[input$parameterChoice]]) > 0)
      cdfplot(prettyParameterName = input$parameterChoice, parameter = input$parameterChoice, indicator = input$dataset,
              dataset = percentiles[[input$parameterChoice]],  CDFsettings = parameterCDFsettings())    })
    
  # output$testtest <- renderPrint({cdfplot(prettyParameterName = input$parameterChoice, parameter = input$parameterChoice, indicator = input$dataset,
  #                                         dataset = percentiles[[input$parameterChoice]],  CDFsettings = parameterCDFsettings())   })#parameterCDFsettings()})#cdfsettings[[paste0(input$parameterChoice,'settingsCDF')]]})##riskTables[[paste0(input$parameterChoice,'RiskTable')]] })#percentiles[[parameter]]})
  # 
}

ui <- fluidPage(
  verbatimTextOutput('test'),
  cdfSubpopSummaryUI('parameterSummary')
)

server <- function(input,output,session){
  # Reactive Value to store all user data
  userData <- reactiveValues()
  
  observe({
    userData$stationStats <- stationStats1
    userData$percentiles <- percentiles1
    userData$stationData <- stationData1
  })
  
  #output$test <- renderPrint({parameterChosen()})#userData$percentiles})
  
  callModule(cdfSubpopSummary, 'parameterSummary',  userData$stationData, userData$percentiles, listOfListsOfRiskTables, listOfListsOfCDFsettings)#userData[["percentiles"]][["pH"]], 'unitless',pHRiskTable)
}

shinyApp(ui, server)  

parameter <- 'pH'

percentiles1[[parameter]][1,] %>% 
  rename('StationID' = 'Statistic', 'Station Average' = 'Average', 'Station Median' = 'Median') %>% 
  bind_cols(percentiles1[[parameter]][2:nrow(percentiles1[[parameter]]),] %>% 
              rename('Subpopulation' = 'Statistic', 'CDF Estimate Based on Average' = 'Average',
                     'CDF Estimate Based on Median' = 'Median'))

# Compare median of selected indicator to prob estimates
subFunction <- function(cdftable,parameter,userInput){
  return(filter(cdftable,Subpopulation%in%userInput & Indicator%in%parameter))
}
#View(subFunction(probEst, parameter = parameter, 'Virginia'))

subFunction2 <- function(cdftable,userValue){
  return(filter(cdftable,Estimate.P %in% userValue))
}
#subFunction2(subFunction(probEst, parameter = parameter, 'Virginia'), 48.14737)



# CDF plot function
prettyParameterName <- 'ph'
indicator <- 'Virginia'
dataset <- percentiles1[[parameter]]
CDFsettings <- listOfListsOfCDFsettings[['pHsettingsCDF']]

cdfplot <- function(prettyParameterName,parameter,indicator,dataset,CDFsettings){
  cdfsubset <- subFunction(cdfdata,parameter,indicator)
  avg1 <- as.numeric(filter(dataset,Statistic==indicator)[,2])
  avg <- subFunction2(cdfsubset,avg1)
  med1 <- as.numeric(filter(dataset,Statistic==indicator)[,3]) 
  med <- subFunction2(cdfsubset,med1)
  m <- max(cdfsubset$NResp)
  p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + 
    labs(x=paste0(prettyParameterName, ' (', unique(cdfsubset$Units), ')',sep=" "),y="Percentile") +
    ggtitle(paste(indicator,prettyParameterName,"Percentile Graph ( n=",m,")",sep=" ")) + 
    theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
    theme(axis.title = element_text(face='bold',size=12))+
    
     CDFsettings  +
    
    geom_point() +
    geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
    geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  return(p1)
  #return(CDFsettings)
}



cdfplot('pH', 'pH','Virginia',percentiles1[[parameter]],listOfListsOfCDFsettings[['pHsettingsCDF']])
