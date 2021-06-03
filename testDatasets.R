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

WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")


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
initialSpatialSuggestions1 <- spatialSuggestions(inputFile1,  pool, subbasinVAHU6crosswalk, subbasins, ecoregion)
allStats1 <- statsFunction(inputFile1)

## user selects station
stationSelection1 <- "4ATKR000.69"

# Subset site data
stationInitialSpatialSuggestions1 <- filter(initialSpatialSuggestions1, WQM_STA_ID %in% stationSelection1)
stationData1 <- filter(inputFile1, StationID %in% stationSelection1) %>% 
  mutate(CollectionDateTime = as.POSIXct(as.character(CollectionDateTime), format =  '%Y-%m-%dT%H:%M:%S'))
#as.POSIXct(as.character(stationData1$CollectionDateTime[1:5]), format =  '%Y-%m-%dT%H:%M:%S')

# user (potentially modified) metadata for site
streamOrder1 <- "Third Order"
ecoregion1 <- stationInitialSpatialSuggestions1$EPA_ECO_US_L3NAME
basin1 <- stationInitialSpatialSuggestions1$ProbBasin
superbasin1 <- stationInitialSpatialSuggestions1$ProbSuperBasin
  
# Central tendency stats
stationStats1 <- filter(allStats1, StationID %in% stationSelection1)

# Site metadata info, suggested by app but captures what user changes it to
stationMetadata1 <- list(StationID = stationSelection1, 
                     `Stream Order` = streamOrder1, 
                     Ecoregion = ecoregion1, 
                     Basin = basin1, 
                     SuperBasin = superbasin1) 

# List to store all percentile results for later use
percentiles1 <- list(pH = percentileTable(stationStats1, "pH", stationMetadata1),
                     `Dissolved Oxygen` = percentileTable(stationStats1, "Dissolved Oxygen", stationMetadata1),
                     `Total Nitrogen` = percentileTable(stationStats1, "Total Nitrogen", stationMetadata1),
                     `Total Phosphorus` = percentileTable(stationStats1, "Total Phosphorus", stationMetadata1),
                     `Total Habitat` = percentileTable(stationStats1, "Total Habitat", stationMetadata1),
                     LRBS = percentileTable(stationStats1, "LRBS", stationMetadata1),
                     MetalsCCU = percentileTable(stationStats1, "MetalsCCU", stationMetadata1),
                     `Specific Conductance` = percentileTable(stationStats1, "Specific Conductance", stationMetadata1),
                     `Total Dissolved Solids` = percentileTable(stationStats1, "Total Dissolved Solids", stationMetadata1),
                     Sulfate = percentileTable(stationStats1, "Sulfate", stationMetadata1),
                     Chloride = percentileTable(stationStats1, "Chloride", stationMetadata1),
                     Potassium = percentileTable(stationStats1, "Potassium", stationMetadata1),
                     Sodium = percentileTable(stationStats1, "Sodium", stationMetadata1))
                             



