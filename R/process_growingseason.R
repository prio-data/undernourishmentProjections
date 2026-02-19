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
library(arrow)
# Clean up temporary files
temp_files <- list.files(tempdir(), full.names = TRUE)
unlink(temp_files, recursive = TRUE)


# Set the working directory and get the list of NetCDF files

source("code/replication_climate_data/functions.R")

setwd("data/crop/growingseason/")




##################

nc_files <- list.files(pattern = "\\.nc4", full.names = TRUE)
nc_files <- gsub(".*/|", "", nc_files)


# Apply the function to all nc_files and combine the results
all_crops_df <- purrr::map_df(nc_files,process_gs)

write_parquet(all_crops_df, file.path("processed/all_crops_growing_season_extended.parquet"))


all_crops_df <- read_parquet("processed/all_crops_growing_season_extended.parquet")
all_crops_df <- all_crops_df %>% dplyr::select(x,y,crop, planting_day, 
                                              maturity_day, crop)

names(all_crops_df) <- c('lon', 'lat', 'crop', 'planting_day', 'maturity_day')

duplicates <- all_crops_df %>%
  group_by(lon, lat, crop) %>%
  filter(n() > 1)

# Modify the crop column to combine irrigated and rainfed crops
all_crops_df <- all_crops_df %>%
  mutate(crop =  gsub("_ir|_rf|1|2", "", crop))

# Define the mapping of original filenames to MAPSPAM names
rename_map <- c(
  "bar" = "barl",
  "bea" = "bean",
  "cas" = "cass",
  "cot" = "cott",
  "mai" = "maiz",
  "mil" = "mill",
  "nut" = "grou",
  "pea" = "pige",
  "pot" = "pota",
  "rap" = "rape",
  "ri" = "rice",
  "rye" = "ocer",
  "sgb" = "sugb",
  "sgc" = "sugc",
  "sor" = "sorg",
  "soy" = "soyb",
  "sun" = "sunf",
  "swh" = "whea",
  "wwh" = "whea"
)

# Apply the renaming using match()
crops_calendar <- all_crops_df %>%
  mutate(crop = ifelse(crop %in% names(rename_map), rename_map[match(crop, names(rename_map))], crop))

crops_calendar <- crops_calendar %>%
  dplyr::select(lon, lat, crop, planting_day, maturity_day) %>%
  group_by(lon, lat, crop) %>%
  summarize(
    planting_day = min(planting_day), 
    maturity_day = max(maturity_day),
    .groups = 'drop'  # Ensures no additional grouping is retained
  )

crops_calendar <- subset(crops_calendar, !is.na(crops_calendar$planting_day))

duplicates <- crops_calendar %>%
  group_by(lon, lat, crop) %>%
  filter(n() > 1)

# Save the combined dataframe as a Parquet file
write_parquet(crops_calendar, file.path("processed/all_crops_growing_season.parquet"))

# Save the combined dataframe as a CSV file if needed
write.csv(crops_calendar, file.path("processed/all_crops_growing_season.csv"), row.names = FALSE)


