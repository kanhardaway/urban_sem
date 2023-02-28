######### Libraries ############
# shpfile manipulation ####
library("raster")
library("sf")
library("sp")
library(rgdal)
library("mapview")
library("dplyr")
library("exactextractr")
library(profvis)
# grabbing data from the census ####
library(tidyverse)
library(tidylog)
library(magrittr)
library(tidycensus)
library(s2)
library(stringr)
library("units")
library("lwgeom")
# visualizations ####
library(ggplot2)
library("corrplot")
library("DescTools")
library("ppcor")


# Set working directory
setwd("D:/Purdue PhD/Projects/sem_spring_2023/")

######### WORKFLOW #############

# 1. downloaded data from respective sources: https://docs.google.com/document/d/13ATSkfk4dIkf5gJY8CMbghCD0GQ7bKTq375e69_-HTw/edit#
# 2. resampled building data to bring to near 60 m resolution (to have more centroids of pixels)
# 3a. Import municipalities - most from Top 500 Most Populous Cities Shapefile 
#               (four cities not included: Harrisburg, PA; Poughkeepsie, NY; North Port, FL; Greenville, SC)
city.ref <- read.csv("./city/cities.csv") # cities gathered with ST_FIPS, self-created abbreviations, CSA abbreviations
        city.ref$merge <- paste(city.ref$city,city.ref$ST_FIPS)
city <- st_read("./city/CityBoundaries.shp") # large dataset (503 cities)
        city$merge <- paste(city$NAME,as.numeric(city$STFIPS))
        city.sel <-subset(city, merge %in% city.ref$merge) # narrowing to selected cities
        city.sel <- city.sel[c('NAME','STFIPS')]
        city.sel <- st_transform(city.sel, crs = 5070)

city_harrisburg <-st_read("./city/City_Boundary_PennDOT.shp") # Harrisburg, PA
        city_harrisburg$NAME <- 'Harrisburg'
        city_harrisburg$STFIPS <- 42
        city_harrisburg <- city_harrisburg[c('NAME','STFIPS')]
        city_harrisburg <- st_transform(city_harrisburg, crs = 5070)
        
city_pough <- st_read("./city/poughkeepsie/Municipalities.shp"); city_pough<-city_pough[city_pough$NAME == 'POUGHKEEPSIE CITY',] # Poughkeepsie City, NY (consciously excluded the Poughkeepsie polygon since it did not represent the city boundaries)
        city_pough$NAME <- 'Poughkeepsie'
        city_pough$STFIPS <- 36
        city_pough <- city_pough[c('NAME','STFIPS')]
        city_pough <- st_transform(city_pough, crs = 5070)
        
city_northport <- st_read("./city/NP_CityBoundary.shp") # North Port, FL
        city_northport$NAME <- 'North Port'
        city_northport$STFIPS <- 12
        city_northport <- city_northport[c('NAME','STFIPS')]
        city_northport <- st_transform(city_northport, crs = 5070)
        
city_greenville <- st_read("./city/Data_Shapefiles/CityLimits.shp") # Greenville, SC
        city_greenville$NAME <- 'Greenville'
        city_greenville$STFIPS <- 45
        city_greenville <- city_greenville[c('NAME','STFIPS')]
        city_greenville <- st_transform(city_greenville, crs = 5070)
        
# 3b. Merge five data.frames (rbind)
        
city_bdry <- rbind(city.sel,city_harrisburg,city_pough,city_northport,city_greenville)

# 4a. Import census data, which contain census tract shapefiles
census_api_key(key = "45ec9b779952a4eebca2031286b625d5d177d0cb", install=T, overwrite = TRUE) 
geom <- 'tract'
YR <- 2019 # 2019 because 2015-2019 ACS 5-year captures 2016 which when canopy data taken
ST <- sort(unique(city$STFIPS))
var <- c('B01003_001','B06011_001') # population, median income, and ________________
# B01003_001 - TOTAL POP 
# B06011_001 - MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS) BY PLACE OF BIRTH IN THE UNITED STATES

census<- get_acs(geography = geom,
                 variables = var, 
                 state = ST,
                 year = YR,
                 output = 'wide',
                 geometry = TRUE) #That pulls in tigerlines
# tigris_use_cache = TRUE)%>% #Cache the data
census <- st_transform(census, crs = 5070) # - likely error if error

census$Area <- st_area(census) %>%
  units::set_units(value = mi^2)

census <- census %>%
  rename(
    FIPS = GEOID,
    pop_total = B01003_001E,
    med_income = B06011_001E
  )
census <- census[-c(4,6)] # make sure to update if anything is changed

# 4b. Winnow census tracts down to selected cities

  # Select by Location (referenced: https://rpubs.com/michaeldorman/247070)

profvis({
  aoi = city_bdry
  grid = census
  tracts = grid[aoi, ]
})
ct <- tracts
  # Remove census tracts larger than 100 sq mi and larger than 24 sq mi when pop. density is less than 100 persons per sq mi
ct$pop_dens <- ct$pop_total/ct$Area

ct <- ct[which(as.numeric(ct$Area) < 100),] # removing excessively large census tracts
ct <- ct[which(as.numeric(ct$Area) < 24 | as.numeric(ct$pop_dens) >= 100),] # ensure census tracts are urban

# 5. Export census tract file to GEE

st_write(ct,"D:/Purdue PhD/Projects/sem_spring_2023/censustracts/coded_ct.shp")

# 6. Import additional data
crs.s <- crs(ct) # desired projection
  # tree canopy
tree <- raster("./veg/nlcd_2016_treecanopy_2019_08_31.img") # already in NAD 1983 Albers
  # ISA
isa <- raster("./isa/nlcd_2016_impervious_l48_20210604/nlcd_2016_impervious_l48_20210604.img"); projectRaster(isa,tree)
  # resampled building height
bldg <- raster("./building/global-built-up-heights.tif"); projectRaster(bldg,tree)
  # NWALT (land use - categorical)
nwalt.ras <- raster("./nwalt/lu2012_050815.tif"); projectRaster(nwalt.ras,tree)
  # health
health <- read.csv("./health/PLACES_2021_release.csv")
  # ecoregion
ecoregion <- st_read("./ecoregions/NA_CEC_Eco_Level2.shp")
  # temperature from GEE
temp <- read.csv("./temp/temp_ALL.csv")

# 7. Zonal Statistics using shapefile from 3b. Retrieve all statistics

s <- stack(tree,isa,bldg,nwalt.ras) # creating a stack of rasters

ex <- extract(s,ct, fun = 'All')

  # tree canopy
zonal_tables <- data.frame() ############
for (i in 1:length(unique(city_bdry$STFIPS))) {
  clipped <- crop(tree, city_bdry[i,])
  clipped <- clipped * (clipped <= 100)  ## since it is percentage of canopy cover
  Census <- ct[which(ct$STFIPS == city_boundary$STFIPS[i]), ] ## shpfile = Census Tract shpfile name
  A <-  exact_extract(clipped, ct, c('sum','mean', 'max', 'min', 'median', "stdev")) # check help there are many functions for statistics # much faster than extract function in "raster" or "terra"
  A$census_tractID  <- Census$column_name # Generate CensusID column # the number of rows in A and Census must be same
  zonal_tables <- rbind(zonal_tables, A)
}
  # ISA

  # bldg

  # NWALT (land use - categorical)

# 8. Reduce each dataset to desired variables
  # tree canopy

  # ISA

  # bldg

  # NWALT (land use - categorical)

  # health
health <- health[c(seq(5,65,by = 2))] # narrowing to FIPS tract code and prevalence of conditions

  # ecoregion

  # temp



# 9. Rename Variables to be more descriptive (necessarily rename FIPS code to "FIPS")
hlth <- hlth %>%
  rename(access2 = ACCESS2_CrudePrev,
         arthritis = ARTHRITIS_CrudePrev,
         binge = BINGE_CrudePrev,
         bphi = BPHIGH_CrudePrev,
         bpmed = BPMED_CrudePrev,
         cancer = CANCER_CrudePrev,
         asthma = CASTHMA_CrudePrev,
         cervical = CERVICAL_CrudePrev,
         chd = CHD_CrudePrev,
         checkup = CHECKUP_CrudePrev,
         cholscreen = CHOLSCREEN_CrudePrev,
         colonscreen = COLON_SCREEN_CrudePrev,
         copd = COPD_CrudePrev,
         corem = COREM_CrudePrev,
         corew = COREW_CrudePrev,
         smoking = CSMOKING_CrudePrev,
         dental = DENTAL_CrudePrev,
         depression = DEPRESSION_CrudePrev,
         diabetes = DIABETES_CrudePrev,
         ghlth = GHLTH_CrudePrev,
         hichol = HIGHCHOL_CrudePrev,
         kidney = KIDNEY_CrudePrev,
         lpa = LPA_CrudePrev,
         mammouse = MAMMOUSE_CrudePrev,
         mhlth = MHLTH_CrudePrev,
         obesity = OBESITY_CrudePrev,
         phlth = PHLTH_CrudePrev,
         sleep = SLEEP_CrudePrev,
         stroke = STROKE_CrudePrev,
         teethlost = TEETHLOST_CrudePrev,
         eco_level_ii = NA_L2NAME
  )
# 10. Spatially join Ecoregion shapefile to NWALT file

# 11. Merge all datasets by FIPS

# 12. Ensure the final dataset has no problems

# 13. Write final shapefile to file folder for other uses

############ DATA DIVISIONS ##########

# 1. Drop geometry from master dataset

# 2. Divide master dataset by ecoregion

# 3. Divide master dataset by land use (NWALT)

# 4a. Use exported final shapefile from Workflow Step 12 to add city codes to each city in ArcGIS
# 4b. Import city-demarcated shapefile
# 4c. Divide master dataset by city

############## Numbers ################

# 1. Ecoregion
  # Gini Index
    # Temperature
    # Canopy
    # ISA
    # Median Income
    # Population Density
  # Coefficient of Variation

  # 
# 2. Land Use

# 3. City




############ Visualizations ###########
