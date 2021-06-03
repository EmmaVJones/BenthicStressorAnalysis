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
