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


# ===============================================================
# 1. Aggregate MapSPAM production and harvested area to 1 degree
# ===============================================================

# Process the PRODUCTION data to aggregate to 1 degree grids and convert to dataframe
directory <- "data/crop/mapspam/spam2020V1r0_global_production/"

process_crop_tiff(directory)
head(crop_data)
names(crop_data) <- c('lon', 'lat', 'crop', 'crop_production')
write.csv(crop_data, file = 'data/crop/mapspam/processed/crop_production.csv', row.names = F)

# Process the HARVESTED AREA data to aggregate to 1 degree grids and convert to dataframe
rm(crop_data) # to make sure nothing goes wrong

directory <- "data/crop/mapspam/spam2020V1r0_global_harvested_area/"
process_crop_tiff(directory)
names(crop_data) <- c('lon', 'lat', 'crop', 'crop_harvarea')
write.csv(crop_data, file = 'data/crop/mapspam/processed/crop_harvarea.csv', row.names = F)


# ===============================================================
# 2. Merge production and harvested area
# ===============================================================

crop_prod <- read.csv('data/crop/mapspam/processed/crop_production.csv')
crop_ha   <- read.csv('data/crop/mapspam/processed/crop_harvarea.csv')

crop_all <- merge(crop_prod, crop_ha, by = c("lon", "lat", "crop"), all = T)

crop_all <- crop_all %>%
  mutate(crop_harvarea = ifelse(is.na(crop_harvarea), 0, crop_harvarea))

write.csv(crop_all, file = 'data/crop/mapspam/processed/crop_harvarea_production.csv', row.names = F)


# ===============================================================
# 3. Merge with the GGCMI growing season calendar
# ===============================================================

crops_calendar <- read.csv("data/crop/growingseason/processed/all_crops_growing_season.csv")
names(crops_calendar)

# Check: every GGCMI crop should match a MapSPAM crop
setdiff(unique(crops_calendar$crop), unique(crop_all$crop))   # should be empty

crops_gs <- merge(crop_all, crops_calendar, by = c('lon', 'lat', 'crop'), all = T)
crops_gs$imputed <- is.na(crops_gs$planting_day)   # no GGCMI calendar for this crop and cell; filled below


# ===============================================================
# 4. Fill calendars for crops not covered by GGCMI
# ===============================================================
# Crops without a GGCMI calendar (e.g. vegetables, fruits, 'rest') get the calendar
# of the crop with the largest harvested area in the same cell that has one.
# (Averaging day-of-year values across crops is not meaningful when seasons cross
# the new year.)

# Convert to sf object, keeping lon/lat
crops_gs_sf <- crops_gs %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

crops_gs_sf <- crops_gs_sf %>%
  mutate(area0 = coalesce(crop_harvarea, 0)) %>%
  group_by(lon, lat) %>%
  mutate(
    has_cal   = !is.na(planting_day) & !is.na(maturity_day),
    ref_plant = if (any(has_cal)) planting_day[has_cal][which.max(area0[has_cal])] else NA_real_,
    ref_mat   = if (any(has_cal)) maturity_day[has_cal][which.max(area0[has_cal])] else NA_real_,
    planting_day = coalesce(planting_day, ref_plant),
    maturity_day = coalesce(maturity_day, ref_mat)
  ) %>%
  ungroup() %>%
  dplyr::select(-area0, -has_cal, -ref_plant, -ref_mat)

# If no crop in the cell has a calendar, take the nearest cell that has one
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


# ===============================================================
# 5. Select the dominant crop per cell
# ===============================================================
# Crop with the maximum harvested area; if tied, the one with the highest production

crop_dominant <- crops_gs %>%
  group_by(lon, lat) %>%
  slice_max(crop_harvarea, with_ties = TRUE) %>%       # Step 1: keep only max harvested area
  slice_max(crop_production, with_ties = FALSE) %>%    # Step 2: if ties, keep highest production
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


# ===============================================================
# 6. Checks and save
# ===============================================================

# Dominant crops whose calendar was imputed
table(crop_dominant$imputed)
crop_dominant %>% filter(imputed) %>% count(crop, sort = TRUE)

# Dominant crop seasons: share crossing the new year and year-round seasons
crop_dominant %>%
  summarise(share_crossing = mean(maturity_day < planting_day),
            year_round     = sum(planting_day == 1 & maturity_day == 365))

write.csv(crop_dominant, 'data/crop/mapspam/processed/crops_gs.csv', row.names = FALSE)
