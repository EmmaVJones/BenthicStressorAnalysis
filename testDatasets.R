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
subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN))
# geospatialTemplate <- z[0,] %>% st_drop_geometry()
# saveRDS(geospatialTemplate, 'geospatialTemplate.RDS')
geospatialTemplate <- readRDS('data/geospatialTemplate.RDS')

pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# Reactive Value to store all user data
userData1 <- list()

# Bring in user uploaded (mulistation) data
inputFile1 <- removeUnits_envDataDF(read.csv("testingData/TinkerMultistation/BSAtemplateData4ATKR.csv"))

# Suggest Ecoregion, Basin, Superbasin, and Order information 

## First pull this info from REST service and spatial joins
initialSpatialSuggestions1 <- spatialSuggestions(inputFile1)

## user selects station
stationSelection1 <- "4ATKR000.69"

# Subset site data
siteData <- filter(inputFile1, StationID %in% stationSelection1) %>% 
  mutate(CollectionDateTime = as.POSIXct(as.character(CollectionDateTime), format =  '%Y-%m-%dT%H:%M:%S'))
as.POSIXct(as.character(siteData$CollectionDateTime[1:5]), format =  '%Y-%m-%dT%H:%M:%S')

  
  
  #summarise_all(list(min, max))
  
  summarise_at(vars(pH:DSodium), median, na.rm = TRUE)
glimpse(stats)

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


