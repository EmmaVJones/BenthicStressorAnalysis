
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


ui <- fluidPage(
  selectInput("parameterChoice", "Choose a parameter to investigate further.", choices = names(dplyr::select(stationData1, pH:DSodium)))
)
