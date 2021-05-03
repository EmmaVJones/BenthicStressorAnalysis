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


board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


## For testing: connect to ODS production
# pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
#  Server= "DEQ-SQLODS-PROD,50000",
#  dbname = "ODS",
#  trusted_connection = "yes"
# )

# For deployment on the R server: Set up pool connection to production environment
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
  # Production Environment
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  UID = conn$UID_prod,
  PWD = conn$PWD_prod,
  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
  # Test environment
  #Server= "WSQ04151,50000",
  #dbname = "ODS_test",
  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
  trusted_connection = "yes"
)

onStop(function() {
  poolClose(pool)
})

geospatialTemplate <- readRDS('data/geospatialTemplate.RDS')
subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN))


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
  return(rename(envData,  pH = "pH..unitless.", DO = "DO..mg.L." , TN =  "TN..mg.L.", 
                TP = "TP..mg.L.", TotalHabitat = "Total.Habitat..unitless.",
                LRBS =  "LRBS..unitless." , MetalsCCU = "MetalsCCU..unitless.",
                SpCond = "SpCond..uS.cm.",  TDS = "TDS..mg.L." ,  
                DSulfate = "DSulfate..mg.L.", DChloride= "DChloride..mg.L.", 
                DPotassium = "DPotassium..mg.L.", DSodium = "DSodium..mg.L.",
                Temp = "Temperature..C.") %>% as_tibble())
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
    dplyr::select(-c(Temp, CollectionDateTime, Longitude, Latitude)) %>% 
    mutate(across(where(is.logical), as.numeric)) %>% 
    mutate(across(where(is.integer), as.numeric)) %>% 
    group_by(StationID) 
  bind_rows(siteDataPrep %>% 
              summarise(across(pH:DSodium, ~ mean(.x, na.rm = TRUE))) %>% 
              mutate(Statistic = 'Average'),
            siteDataPrep %>% 
              summarise(across(pH:DSodium, ~ median(.x, na.rm = TRUE))) %>% 
              mutate(Statistic = 'Median')) %>% 
    dplyr::select(StationID, Statistic, everything()) %>% 
    arrange(StationID, Statistic)
}
#statsFunction(inputFile1)
