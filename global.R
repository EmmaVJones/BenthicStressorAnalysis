httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))


library(shiny)
library(shinyjs)
#library(leaflet)
#library(inlmisc)
library(tidyverse)
library(sf)
library(DT)
library(config)
library(pins)
library(pool)
library(geojsonsf)

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings


# board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
#                          server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))
# 
# 
# ## For testing: connect to ODS production
# pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
#  Server= "DEQ-SQLODS-PROD,50000",
#  dbname = "ODS",
#  trusted_connection = "yes"
# )

# For deployment on the R server: Set up pool connection to production environment
# pool <- dbPool(
#   drv = odbc::odbc(),
#   Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
#   # Production Environment
#   Server= "DEQ-SQLODS-PROD,50000",
#   dbname = "ODS",
#   UID = conn$UID_prod,
#   PWD = conn$PWD_prod,
#   #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
#   # Test environment
#   #Server= "WSQ04151,50000",
#   #dbname = "ODS_test",
#   #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
#   trusted_connection = "yes"
# )

onStop(function() {
  poolClose(pool)
})

# Source functions from elsewhere
source('functionsAndModules/riskColors.R')
source('functionsAndModules/vlookup.R')
source('functionsAndModules/CDFsubpopModule.R')


# Local data for upload
template <- read_csv('data/template.csv')
geospatialTemplate <- readRDS('data/geospatialTemplate.RDS')
subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN))


# one day these won't need to be converted
cdfdata <- readRDS('data/IR2020probMonCDFestimates.RDS') %>%  ### UPDATED CDF DATA AFTER 2020IR REPORT
  mutate(Subpopulation = case_when(Subpopulation == 'Roanoke Basin' ~ 'Roanoke', 
                                   Subpopulation == 'James Basin' ~ 'James',
                                   Subpopulation == 'Central Appalachian Ridges and Valleys' ~'Ridge and Valley',
                                   Subpopulation == "Blue Ridge Mountains" ~ "Blue Ridge", 
                                   TRUE ~ as.character(Subpopulation)),
         Indicator = case_when(Indicator == 'DO' ~ 'Dissolved Oxygen',
                               Indicator == 'TN' ~ 'Total Nitrogen',
                               Indicator == 'TP' ~ 'Total Phosphorus',
                               Indicator == 'TotHab' ~ 'Total Habitat',
                               Indicator == 'MetalCCU' ~ 'MetalsCCU',
                               Indicator == 'SpCond' ~ 'Specific Conductance',
                               Indicator == 'TDS' ~ 'Total Dissolved Solids',
                               Indicator == 'Sf' ~ 'Sulfate',
                               Indicator == 'Cl' ~ 'Chloride',
                               Indicator == 'K' ~ 'Potassium',
                               Indicator == 'Na' ~ 'Sodium',
                               TRUE ~ as.character(Indicator)))

unitData <- read_csv('data/probParameterUnits.csv')
# Temporary list that we can compare data to. Maybe we increase to benthic metrics, MCCU, + more in time
probIndicators <- filter(unitData, AltName %in% #names(basicData))$AltName
                           c("Dissolved Oxygen", "pH", "Specific Conductance", "Total Habitat", "Total Nitrogen", "Total Phosphorus", "Total Dissolved Solids",
                             "Ammonia", "Total Nitrate Nitrogen", "Ortho Phosphorus", "Turbidity", "Total Suspended Solids", "Sodium", 
                             "Potassium", "Chloride", "Sulfate", "Suspended Sediment Concentration Coarse", "Suspended Sediment Concentration Fine",
                             "Arsenic", "Barium", "Beryllium", "Cadmium", "Chromium", "Copper", "Iron", "Lead", "Manganese", "Thallium", "Nickel",                                 
                             "Silver", "Zinc", "Antimony", "Aluminum", "Selenium", "Hardness"))


addUnits_envDataDF <- function(envData){
  return(rename(envData, `pH (unitless)`= pH, `DO (mg/L)` = DO, `TN (mg/L)` = TN, 
                `TP (mg/L)` = TP, `Total Habitat (unitless)`= TotalHabitat,
                `LRBS (unitless)`= LRBS, `MetalsCCU (unitless)`= MetalsCCU,
                `SpCond (uS/cm)` = SpCond,  `TDS (mg/L)` = TDS,  
                `DSulfate (mg/L)` = DSulfate, `DChloride (mg/L)` = DChloride, 
                `DPotassium (mg/L)` = DPotassium, `DSodium (mg/L)` = DSodium,
                `Temperature (C)` = Temp) %>% as_tibble())
}

removeUnits_envDataDF <- function(envData){
  return(rename(envData,  pH = "pH..unitless.", 'Dissolved Oxygen' = "DO..mg.L." , 'Total Nitrogen' =  "TN..mg.L.", 
                'Total Phosphorus' = "TP..mg.L.", 'Total Habitat' = "Total.Habitat..unitless.",
                LRBS =  "LRBS..unitless." , MetalsCCU = "MetalsCCU..unitless.",
                'Specific Conductance' = "SpCond..uS.cm.",  'Total Dissolved Solids' = "TDS..mg.L." ,  
                Sulfate = "DSulfate..mg.L.", Chloride= "DChloride..mg.L.", 
                Potassium = "DPotassium..mg.L.", Sodium = "DSodium..mg.L.",
                Temperature = "Temperature..C.") %>% as_tibble())
}


# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST_request <- function(pool, station, subbasinVAHU6crosswalk, subbasins, ecoregion){
  WQM_Station_Full_REST <- suppressWarnings(
    geojson_sf(
      paste0("http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
             toupper(station),"%27&outFields=*&f=geojson"))) 
  
  if(nrow(WQM_Station_Full_REST ) > 0){
    WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA)) %>% 
      left_join(dplyr::select(subbasinVAHU6crosswalk, SubbasinVAHU6code, BASIN_NAME), by = c('BASINS_VAHUSB' = 'SubbasinVAHU6code'))
    WQM_Station_Full_REST <- bind_cols(WQM_Station_Full_REST, st_coordinates(WQM_Station_Full_REST) %>% as_tibble()) %>%
      mutate(Latitude = Y, Longitude = X) %>% # add lat/lng in DD
      # have to strip geometry and add it back in to get st_intersection to work for some reason
      st_drop_geometry() %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)
    stationBasin <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, subbasins)$ProbBasin))
    stationSuperBasin <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, subbasins)$ProbSuperBasin))
    
    WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, ProbBasin = stationBasin, ProbSuperBasin = stationSuperBasin)
    
  } else { # station doesn't yet exist in WQM full dataset
    # get what we can from CEDS
    stationGISInfo <- pool %>% tbl( "WQM_Sta_GIS_View") %>%
      filter(Station_Id %in% !! toupper(station)) %>%
      as_tibble() 
    # pull a known station to steal data structure
    WQM_Station_Full_REST <- suppressWarnings(
      geojson_sf(
        paste0("http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%272-JKS023.61%27&outFields=*&f=geojson")))[1,] %>%
      mutate(WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA)) %>% 
      left_join(dplyr::select(subbasinVAHU6crosswalk, SubbasinVAHU6code, BASIN_NAME), by = c('BASINS_VAHUSB' = 'SubbasinVAHU6code')) %>%
      st_drop_geometry()
    WQM_Station_Full_REST <- bind_rows(WQM_Station_Full_REST[0,],
                                       tibble(STATION_ID = stationGISInfo$Station_Id, 
                                              Latitude = stationGISInfo$Latitude,
                                              Longitude = stationGISInfo$Longitude,
                                              BASINS_HUC_8_NAME = stationGISInfo$Huc6_Huc_8_Name, 
                                              BASINS_VAHU6 = stationGISInfo$Huc6_Vahu6) ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)   
    # add in basin and ecoregion information
    stationEcoregion <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, ecoregion)$US_L3NAME))
    stationBasin <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, subbasins)$ProbBasin))
    stationSuperBasin <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, subbasins)$ProbSuperBasin))
    
    WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, EPA_ECO_US_L3NAME = stationEcoregion, ProbBasin = stationBasin, ProbSuperBasin = stationSuperBasin)
  }
  return(WQM_Station_Full_REST) }


# Suggest Ecoregion, Basin, Superbasin, and Order information 
spatialSuggestions <- function(inFile, pool, subbasinVAHU6crosswalk, subbasins, ecoregion){
  if(nrow(inFile) > 0){
    zOut <- geospatialTemplate
    for(i in unique(inFile$StationID)){
      z <- WQM_Station_Full_REST_request(pool, i, subbasinVAHU6crosswalk, subbasins, ecoregion) %>% 
        distinct(STATION_ID, .keep_all = T) %>% dplyr::select(-c(WQM_VERIFYDATE, WQM_CHANGED_DATE)) %>% 
        st_drop_geometry()
      zOut <- bind_rows(zOut, z)
    }
    return(zOut)
  }
}


# Site Stats Function
statsFunction <- function(siteData){
  siteDataPrep <- siteData %>% 
    dplyr::select(-c(Temperature, CollectionDateTime, Longitude, Latitude)) %>% 
    mutate(across(where(is.logical), as.numeric)) %>% 
    mutate(across(where(is.integer), as.numeric)) %>% 
    group_by(StationID) 
  bind_rows(siteDataPrep %>% 
              summarise(across(pH:Sodium, ~ mean(.x, na.rm = TRUE))) %>% 
              mutate(Statistic = 'Average'),
            siteDataPrep %>% 
              summarise(across(pH:Sodium, ~ median(.x, na.rm = TRUE))) %>% 
              mutate(Statistic = 'Median')) %>% 
    dplyr::select(StationID, Statistic, everything()) %>% 
    arrange(StationID, Statistic)
}
#statsFunction(inputFile1)

# Return percentile 
#statsTable <- stationStats1; parameter <- "pH"; stationMetadata <- stationMetadata1
percentileTable <- function(statsTable, parameter, stationMetadata){
  out <- dplyr::select(statsTable, Statistic, !! parameter) %>% 
    pivot_wider(names_from = 'Statistic', values_from = !! parameter) %>% 
    mutate(Statistic = stationMetadata$StationID) %>% 
    select(Statistic,everything())
  # make dataset of just indicator and parameter for vlookup function to operate on
  va <- filter(cdfdata, Subpopulation == 'Virginia', Indicator == !! parameter) %>% select(Value, Estimate.P)
  basin <- filter(cdfdata, Subpopulation == stationMetadata$Basin, Indicator == !! parameter) %>% select(Value, Estimate.P)
  superbasin <- filter(cdfdata, Subpopulation == stationMetadata$SuperBasin, Indicator == !! parameter) %>% select(Value, Estimate.P)
  eco <- filter(cdfdata, Subpopulation == stationMetadata$Ecoregion, Indicator == !! parameter) %>% select(Value, Estimate.P)
  order <- filter(cdfdata, Subpopulation == stationMetadata$`Stream Order`, Indicator == !! parameter) %>% select(Value, Estimate.P)
  
  return(
    bind_rows(out, 
              tibble(Statistic='Virginia', Average = vlookup(out$Average, data.frame(va), 2, range=TRUE), Median = vlookup(out$Median, data.frame(va), 2, range=TRUE))) %>% 
      {if(nrow(superbasin) > 0)
        bind_rows(  tibble(Statistic = stationMetadata$SuperBasin, Average = vlookup(out$Average, superbasin, 2, TRUE), Median = vlookup(out$Median, superbasin, 2, TRUE)) )
        else . } %>% #bind_rows( .,  tibble(Statistic = NA, Average = NA, Median = NA)) } %>% 
      bind_rows(tibble(Statistic = stationMetadata$Basin, Average=vlookup(out$Average, data.frame(basin), 2, TRUE), Median = vlookup(out$Median, data.frame(basin), 2, TRUE)),
                tibble(Statistic = stationMetadata$Ecoregion, Average = vlookup(out$Average, data.frame(eco), 2, TRUE), Median = vlookup(out$Median, data.frame(eco), 2, TRUE)),
                tibble(Statistic = stationMetadata$`Stream Order`, Average = vlookup(out$Average, data.frame(order), 2, TRUE), Median = vlookup(out$Median, data.frame(order), 2, TRUE))
      ) ) }
#percentileTable(stationStats1, 'SpCond', stationMetadata1)




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
}
#cdfplot('pH', 'pH','Virginia',percentiles1[[parameter]],listOfListsOfCDFsettings[['pHsettingsCDF']])
