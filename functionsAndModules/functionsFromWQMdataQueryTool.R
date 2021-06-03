
# Actual scatterplot function
parameterScatterPlotly <- function(dat, parameter){
  plot_ly(data=dat) %>%
    {if(parameter %in% c('Temperature', 'Dissolved Oxygen'))
      add_lines(., data = dat, x=~`Collection Date`,y=~Standard, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
      else . } %>%
    {if(parameter %in% c('pH'))
      add_lines(., data = dat, x=~`Collection Date`,y=~Standard1, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) %>%
        add_lines(data = dat, x=~`Collection Date`,y=~Standard2, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
      else . } %>%
    {if(length(unique(dat$StationID)) > 1)
      add_markers(., data=dat, x= ~`Collection Date`, y= ~Measure,mode = 'scatter', name= ~StationID, 
                  color=~StationID,  symbol = ~StationID, #symbols = c('circle','x','o'), #marker = list(color= '#535559'), 
                  hoverinfo="text",
                  text=~paste(sep="<br>",
                              paste("StationID: ",StationID),
                              paste("Sample Date: ",`Collection Date`),
                              paste("Depth: ",Depth, "m"),
                              paste(parameter, ": ",Measure," (mg/L)")))
      else add_markers(., data=dat, x= ~`Collection Date`, y= ~Measure,mode = 'scatter', name= ~StationID, 
                       marker = list(color= '#535559'), hoverinfo="text",
                       text=~paste(sep="<br>",
                                   paste("StationID: ",StationID),
                                   paste("Sample Date: ",`Collection Date`),
                                   paste("Depth: ",Depth, "m"),
                                   paste(parameter, ": ",Measure," (mg/L)"))) } %>%
    layout(showlegend=TRUE,
           yaxis=list(title=paste(parameter, unique(dat$units))),
           xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
}


# Scatterplot with BSA colors
parameterScatterPlotlyBSA <- function(dat, parameter){
  if(nrow(dat) == 1){
    dat <- bind_rows(dat,
                     tibble(`Collection Date` = c(dat$`Collection Date`- days(5), dat$`Collection Date` + days(5))))
  }
  if(parameter == 'Chloride'){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 50, 55, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(50, maxheight, maxheight, 50))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(25, 50, 50, 25))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(10, 25, 25, 10))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 10, 10, 0))  }
  if(parameter == "Dissolved Oxygen"){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 10, 12, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(10, maxheight, maxheight, 10))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(8, 10, 10, 8))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(7, 8, 8, 7))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 7, 7, 0))}
  if(parameter == 'Total Nitrate Nitrogen'){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 50, 55, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(50, maxheight, maxheight, 50))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(25, 50, 50, 25))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(10, 25, 25, 10))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 10, 10, 0))}
  if(parameter == 'pH'){
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(9, 14, 14, 9))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(6, 9, 9, 6))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 6, 6, 0))}
  if(parameter == 'Specific Conductance'){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 500, 600, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(500, maxheight, maxheight, 500))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(350, 500, 500, 350))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(250, 350, 350, 250))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 250, 250, 0)) }
  if(parameter == 'Sulfate'){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 75, 100, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(75, maxheight, maxheight, 75))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(25, 75, 75, 25))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(10, 25, 25, 10))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 10, 10, 0)) }
  if(parameter == "Total Nitrogen"){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 2, 2.5, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(2, maxheight, maxheight, 2))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(1, 2, 2, 1))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0.5, 1, 1, 0.5))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 0.5, 0.5, 0)) }
  if(parameter == "Total Phosphorus"){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 0.1, 0.12, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0.1, maxheight, maxheight, 0.1))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0.05, 0.1, 0.1, 0.05))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0.02, 0.05, 0.05, 0.02))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 0.02, 0.02, 0)) }
  
  
  plot_ly(data=dat) %>%
    # pH special case
    {if(parameter == 'pH')
      add_polygons(., data = box1, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life'))
      else . } %>%
    #boxes go low to high stress
    {if(parameter %in% c("Dissolved Oxygen"))
      add_polygons(., data = box1, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) 
      else . } %>% 
    # boxes go high to low stress
    {if(parameter %in% c('Chloride', 'Total Nitrate Nitrogen', 'Specific Conductance', 'Sulfate', "Total Nitrogen",
                         "Total Phosphorus"))
      add_polygons(., data = box1, x = ~x, y = ~y,  fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life'))
      else . } %>% 
    
    {if(parameter %in% c('Temperature', 'Dissolved Oxygen'))
      add_lines(., data = dat, x=~`Collection Date`,y=~Standard, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
      else . } %>%
    {if(parameter %in% c('pH'))
      add_lines(., data = dat, x=~`Collection Date`,y=~Standard1, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) %>%
        add_lines(data = dat, x=~`Collection Date`,y=~Standard2, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
      else . } %>%
    {if(length(unique(filter(dat, !is.na(StationID))$StationID)) > 1)
      add_markers(., data=dat, x= ~`Collection Date`, y= ~Measure,mode = 'scatter', name= ~StationID, 
                  color=~StationID,  symbol = ~StationID,  #marker = list(color= '#535559'), 
                  hoverinfo="text",
                  text=~paste(sep="<br>",
                              paste("StationID: ",StationID),
                              paste("Sample Date: ",`Collection Date`),
                              paste("Depth: ",Depth, "m"),
                              paste(parameter, ": ",Measure," (mg/L)")))
      else add_markers(., data=dat, x= ~`Collection Date`, y= ~Measure,mode = 'scatter', name= ~StationID, 
                       marker = list(color= '#535559'), hoverinfo="text",
                       text=~paste(sep="<br>",
                                   paste("StationID: ",StationID),
                                   paste("Sample Date: ",`Collection Date`),
                                   paste("Depth: ",Depth, "m"),
                                   paste(parameter, ": ",Measure," (mg/L)"))) } %>%
    layout(showlegend=TRUE,
           yaxis=list(title=paste(parameter, unique(dat$units))),
           xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
}

# basicData <- inputFile1 %>%  mutate(`Collection Date` = as.Date(CollectionDateTime))
# parameterPlotly(basicData, parameter, unitData, WQSlookup, addBSAcolors= T)

parameterPlotly <- function(basicData,
                            parameter,
                            unitData,
                            WQSlookup,
                            addBSAcolors){
  z <- dplyr::select(basicData, parameterPlot = !! parameter) %>% # rename clutch for nse
    filter(!is.na(parameterPlot)) 
  if(nrow(z) != 0){
    parameterUnits <- filter(unitData, AltName %in% !!parameter)$Units
    if(parameter %in% c('Temperature', 'Dissolved Oxygen', "pH")){
      if(parameter == 'Temperature'){parameterLimit <- 'Max Temperature (C)'; specialStandards <- NULL}
      if(parameter == 'Dissolved Oxygen'){parameterLimit <- 'Dissolved Oxygen Min (mg/L)'; specialStandards <- NULL}
      if(parameter == 'pH'){
        parameterLimit <- c('pH Min', 'pH Max')
        if(nrow(filter(WQSlookup, StationID %in% unique(basicData$StationID)) %>% 
                filter(str_detect(as.character(SPSTDS), '6.5-9.5'))) > 0){specialStandards <- c(6.5, 9.5)
        } else {specialStandards <- NULL}}
    } else { parameterLimit <- NULL 
    specialStandards <- NULL  }
    
    dat <- dplyr::select(basicData, StationID, `Collection Date`, Depth, Measure = !! parameter) %>%
      filter( !is.na(Measure))
    
    if(nrow(dat) > 0){
      dat <- left_join(dat, WQSlookup, by = c('StationID')) %>%
        mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
        mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
        # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
        left_join(WQSvalues, by = 'CLASS_BASIN') %>%
        dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
        rename('CLASS' = 'CLASS.x') %>%
        # add standards info if available
        {if(!is.null(parameterLimit))
          dplyr::select(., StationID, `Collection Date`, Depth, Measure, Standard = !!parameterLimit) 
          else dplyr::select(., StationID, `Collection Date`, Depth, Measure) } %>%
        # pH special standards correction
        {if(!is.null(specialStandards))
          mutate(., Standard1 = specialStandards[1], Standard2 = specialStandards[2])
          else . } %>%
        mutate(units = parameterUnits) 
      #return(dat)
      
      if(addBSAcolors == FALSE){
        parameterScatterPlotly(dat, parameter)
      } else {
        parameterScatterPlotlyBSA(dat, parameter)     }
    } } 
}

##  Parameter graph with loess smoother
basicLoessPlotFunction <- function(basicData, parameter){
  ggplotly(
    basicData %>% 
      dplyr::select(StationID, `Collection Date`, Depth, Measure = !! parameter) %>%
      filter( !is.na(Measure)) %>% 
      ggplot()+
      geom_point(aes(x=`Collection Date`,y= Measure,  colour=StationID, shape = StationID) ) +
      geom_smooth(aes(x=`Collection Date`,y= Measure ),method='loess') +
      labs(y = parameter) +
      theme_minimal()) 
}
#basicLoessPlotFunction(basicData, 'pH')

## Individual parameter boxplot
parameterBoxplotFunction <- function(basicData, parameter, unitData, WQSlookup, addJitter){
  z <- dplyr::select(basicData, parameterPlot = !! parameter) %>% # rename clutch for nse
    filter(!is.na(parameterPlot)) 
  if(nrow(z) != 0){
    parameterUnits <- filter(unitData, AltName %in% !!parameter)$Units
    if(parameter %in% c('Temperature', 'Dissolved Oxygen', "pH")){
      if(parameter == 'Temperature'){parameterLimit <- 'Max Temperature (C)'; specialStandards <- NULL}
      if(parameter == 'Dissolved Oxygen'){parameterLimit <- 'Dissolved Oxygen Min (mg/L)'; specialStandards <- NULL}
      if(parameter == 'pH'){
        parameterLimit <- c('pH Min', 'pH Max')
        if(nrow(filter(WQSlookup, StationID %in% unique(basicData$StationID)) %>% 
                filter(str_detect(as.character(SPSTDS), '6.5-9.5'))) > 0){specialStandards <- c(6.5, 9.5)
        } else {specialStandards <- NULL}}
    } else { parameterLimit <- NULL 
    specialStandards <- NULL  }
    
    dat <- dplyr::select(basicData, StationID, `Collection Date`, Depth, Measure = !! parameter) %>%
      filter( !is.na(Measure))
    
    if(nrow(dat) > 0){
      dat <- left_join(dat, WQSlookup, by = c('StationID')) %>%
        mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
        mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
        # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
        left_join(WQSvalues, by = 'CLASS_BASIN') %>%
        dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
        rename('CLASS' = 'CLASS.x') %>%
        # add standards info if available
        {if(!is.null(parameterLimit))
          dplyr::select(., StationID, `Collection Date`, Depth, Measure, Standard = !!parameterLimit) 
          else dplyr::select(., StationID, `Collection Date`, Depth, Measure) } %>%
        # pH special standards correction
        {if(!is.null(specialStandards))
          mutate(., Standard1 = specialStandards[1], Standard2 = specialStandards[2])
          else . } %>%
        mutate(units = parameterUnits) 
      #return(dat)
      if(addJitter == TRUE){
        plot_ly(data=dat) %>% 
          add_boxplot( y = ~Measure, color = ~StationID, type = "box", boxpoints = "all", 
                       jitter = 0.3, pointpos = -1.8) %>% 
          {if(parameter %in% c('Temperature', 'Dissolved Oxygen'))
            add_markers(., x = ~StationID, y=~Standard, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                        hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard"))
            else .} %>% 
          {if(parameter %in% c('pH'))
            add_markers(., x = ~StationID, y=~Standard1, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                        hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) %>% 
              add_markers(., x = ~StationID, y=~Standard2, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                          hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard"))
            else .} 
        
      } else {
        plot_ly(data=dat) %>% 
          add_boxplot(y = ~Measure, color = ~StationID, type = "box") %>% 
          {if(parameter %in% c('Temperature', 'Dissolved Oxygen'))
            add_markers(., x = ~StationID, y=~Standard, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                        hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard"))
            else .} %>% 
          {if(parameter %in% c('pH'))
            add_markers(., x = ~StationID, y=~Standard1, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                        hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) %>% 
              add_markers(., x = ~StationID, y=~Standard2, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                          hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard"))
            else .}  }
    }
  }
}
#parameterBoxplotFunction(basicData, 'Dissolved Oxygen', unitData, WQSlookup, addJitter = F)

