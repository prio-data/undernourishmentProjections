rm(list = ls(all = TRUE))

# Load necessary libraries
library(cshapes)
library(ncdf4)
library(terra)
library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(CFtime)
library(RColorBrewer)
library(lattice)
library(raster)
library(crayon)
library(arrow)
library(ggplot2)
library(rasterVis)

# Clean up temporary files
temp_files <- list.files(tempdir(), full.names = TRUE)
unlink(temp_files, recursive = TRUE)

# Set the working directory and get the list of NetCDF files
#setwd("")
source("code/replication_climate_data/functions.R")
# Process the PRODUCTION data to aggregate to 1 degree grids and convert to dataframe 
directory <- "data/crop/mapspam/spam2020V1r0_global_production/"




###############################

process_crop_tiff(directory)

head(crop_data)
names(crop_data) <- c('lon', 'lat', 'crop', 'crop_production')


write.csv(crop_data, file = 'data/crop/mapspam/processed/crop_production.csv', row.names = F)

# Process the HARVESTED AREA data to aggregate to 1 degree grids and convert to dataframe 
rm(crop_data) #to make sure nothing goes wrong

directory <- "data/crop/mapspam/spam2020V1r0_global_harvested_area/"


process_crop_tiff(directory)

names(crop_data) <- c('lon', 'lat', 'crop', 'crop_harvarea')

write.csv(crop_data, file = 'crop_harvarea.csv', row.names = F)



###Restart from here
#####Load and merge#####


# Load the two datasets
crop_prod <- read.csv('data/crop/mapspam/processed/crop_production.csv')
crop_ha <- read.csv('data/crop/mapspam/processed/crop_harvarea.csv', )

crop_all <- merge(crop_prod, crop_ha, 
                           by = c("lon", "lat", "crop"), all =T)

crop_all <- crop_all %>%
  mutate(crop_harvarea = ifelse(is.na(crop_harvarea), 0, crop_harvarea))

write.csv(crop_all, file = 'data/crop/mapspam/processed/crop_harvarea_production.csv', row.names = F)


#Merge data on crop production from Mapspam with data on growing season from GGCMI

#Data on crops growing season from GGCMI
crops_calendar <- read.csv("data/crop/growingseason/processed/all_crops_growing_season.csv")
names(crops_calendar)

crops_gs <- merge(crop_all, crops_calendar, by = c('lon', 'lat', 'crop'), all = T)

#'rest' (all other crops) in mapspam is not matched, assign to the dates of the closest crop by lon lat
# Convert to sf object, keeping lon/lat
crops_gs_sf <- crops_gs %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

#Fill missing values using the average for the same lon, lat
crops_gs_sf <- crops_gs_sf %>%
  group_by(lon, lat) %>%
  mutate(
    planting_day = ifelse(is.na(planting_day), round(mean(planting_day, na.rm = TRUE)), planting_day),
    maturity_day = ifelse(is.na(maturity_day), round(mean(maturity_day, na.rm = TRUE)), maturity_day)
  ) %>%
  ungroup()

#Find the nearest non-NA value only if still missing
crops_non_na <- crops_gs_sf %>%
  filter(!is.na(planting_day) & !is.na(maturity_day))

crops_gs_sf <- crops_gs_sf %>%
  mutate(
    planting_day = coalesce(planting_day, crops_non_na$planting_day[st_nearest_feature(., crops_non_na)]),
    maturity_day = coalesce(maturity_day, crops_non_na$maturity_day[st_nearest_feature(., crops_non_na)])
  )

# Convert back to dataframe without dropping lon/lat
crops_gs_filled <- as.data.frame(crops_gs_sf)

crops_gs <- crops_gs_filled %>% filter(!is.na(crop_harvarea)) %>% filter(crop_harvarea > 0)

# Summarize the data by lon and lat, selecting the crop with the maximum harvest area
# and if there's a tie, keep only the one with the highest crop_production 

crop_dominant <- crops_gs %>%
  group_by(lon, lat) %>%
  slice_max(crop_harvarea, with_ties = TRUE) %>%  # Step 1: Keep only max harvested area
  slice_max(crop_production, with_ties = FALSE) %>%  # Step 2: If ties, keep highest production
  ungroup()


duplicates <- crop_dominant %>%
  group_by(lon, lat) %>%
  filter(n() > 1)

duplicates

head(crop_dominant) 

spg <- crop_dominant %>% dplyr::select(lon, lat, crop_harvarea)
coordinates(spg) <- ~ lon + lat

# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE

# coerce to raster
rasterDF <- raster(spg)
plot(rasterDF)

crop_dominant <- crop_dominant %>% mutate(lon = round(as.numeric(lon), 2), lat = round(as.numeric(lat), 2))
crop_dominant <- crop_dominant %>% dplyr::select(-geometry, -crop_production)

write.csv(crop_dominant, 'data/crop/mapspam/processed/crops_gs.csv')
