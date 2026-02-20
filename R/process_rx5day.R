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
#setwd("")
temp_files <- list.files(tempdir(), full.names = TRUE)
unlink(temp_files, recursive = TRUE)

source("code/replication_climate_data/functions.R")

###HISTORICAL 

future = F
hist = T
scenario <- "hist"

nc_file = 'data/extremes/rx5day/hist/rx5day_CMIP6_historical_mon_185001-201412_v02.nc'

experiment_id <- scenario

if (scenario != 'hist'){
  # Define the start and end year
  start <- 2015 #fut 
  end <- 2100 #fut
} else{
  start <- 1950 #hist 
  end <- 2014 #hist
}


# Open the NetCDF file
nc_data <- nc_open(nc_file)


# Define the output directory
output_dir <- paste0("data/extremes/processed/rx5day_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

find_date(nc_file, start, end)

# Process the NetCDF file

#process_rx5day_data(nc_file, output_dir)

output_csv_grid <- file.path(output_dir, paste0("rx5day_CMIP6_", scenario, ".csv"))

# Check on a map


#Combine rx5day parquet files to obtain complete timeseries for historic and projected data

# Read and combine grid-level Parquet files
output_dir
grid_data <- read_combine_parquet("rx5day_grid_timestep")

grid_data <- grid_data %>% filter(!is.na(rx5day_median))

# Open the NetCDF file
nc_data <- nc_open(nc_file)

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
time <- ncvar_get(nc_data, "time")
tunits  <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)




grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)
names(grid_data)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates
summary(grid_data)

output_csv_grid <- file.path(output_dir, paste0("rx5day_", scenario, ".csv"))

write.csv(grid_data, output_csv_grid, row.names = FALSE)


#######################126######################

hist = F
future = T
scenario <- 'ssp126'

nc_file = 'data/extremes/rx5day/ssp126/rx5day_CMIP6_ssp126_mon_201501-210012_v02.nc'

experiment_id <- scenario

if (scenario != 'hist'){
  # Define the start and end year
  start <- 2015 #fut 
  end <- 2100 #fut
} else{
  start <- 1980 #hist 
  end <- 2014 #hist
}


# Open the NetCDF file
nc_data <- nc_open(nc_file)


# Define the output directory
output_dir <- paste0("data/extremes/processed/rx5day_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

find_date(nc_file, start, end)

# Process the NetCDF file
#process_rx5day_data(nc_file, output_dir)

output_csv_grid <- file.path(output_dir, paste0("rx5day_CMIP6_", scenario, ".csv"))

# Check on a map


#Combine rx5day parquet files to obtain complete timeseries for historic and projected data

# Read and combine grid-level Parquet files
output_dir
grid_data <- read_combine_parquet("rx5day_grid_timestep")

grid_data <- grid_data %>% filter(!is.na(rx5day_median))
# Open the NetCDF file
nc_data <- nc_open(nc_file)

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
time <- ncvar_get(nc_data, "time")
tunits  <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)




grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)
names(grid_data)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates
summary(grid_data)

output_csv_grid <- file.path(output_dir, paste0("rx5day_", scenario, ".csv"))
write.csv(grid_data, output_csv_grid, row.names = FALSE)

output_dta_grid <- file.path(output_dir, paste0("rx5day_", scenario, ".dta"))



############################245#######################

future = T
hist = F
#scenario <- "hist"
scenario <- 'ssp245'

nc_file = 'data/extremes/rx5day/ssp245/rx5day_CMIP6_ssp245_mon_201501-210012_v02.nc'

experiment_id <- scenario

if (scenario != 'hist'){
  # Define the start and end year
  start <- 2015 #fut 
  end <- 2100 #fut
} else{
  start <- 1980 #hist 
  end <- 2014 #hist
}


# Open the NetCDF file
nc_data <- nc_open(nc_file)


# Define the output directory
output_dir <- paste0("data/extremes/processed/rx5day_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

find_date(nc_file, start, end)

# Process the NetCDF file
#process_rx5day_data(nc_file, output_dir)

output_csv_grid <- file.path(output_dir, paste0("rx5day_CMIP6_", scenario, ".csv"))

# Check on a map


#Combine rx5day parquet files to obtain complete timeseries for historic and projected data

# Read and combine grid-level Parquet files
output_dir
grid_data <- read_combine_parquet("rx5day_grid_timestep")

grid_data <- grid_data %>% filter(!is.na(rx5day_median))
# Open the NetCDF file
nc_data <- nc_open(nc_file)

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
time <- ncvar_get(nc_data, "time")
tunits  <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)




grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)
names(grid_data)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates
summary(grid_data)

output_csv_grid <- file.path(output_dir, paste0("rx5day_", scenario, ".csv"))
write.csv(grid_data, output_csv_grid, row.names = FALSE)

############################370#######################

scenario <- 'ssp370'

nc_file = 'data/extremes/rx5day/ssp370/rx5day_CMIP6_ssp370_mon_201501-210012_v02.nc'

experiment_id <- scenario

if (scenario != 'hist'){
  # Define the start and end year
  start <- 2015 #fut 
  end <- 2100 #fut
} else{
  start <- 1980 #hist 
  end <- 2014 #hist
}


# Open the NetCDF file
nc_data <- nc_open(nc_file)


# Define the output directory
output_dir <- paste0("data/extremes/processed/rx5day_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

find_date(nc_file, start, end)

# Process the NetCDF file
#process_rx5day_data(nc_file, output_dir)

output_csv_grid <- file.path(output_dir, paste0("rx5day_CMIP6_", scenario, ".csv"))

#Combine rx5day parquet files to obtain complete timeseries for historic and projected data

# Read and combine grid-level Parquet files

output_dir
grid_data <- read_combine_parquet("rx5day_grid_timestep")

grid_data <- grid_data %>% filter(!is.na(rx5day_median))


# Open the NetCDF file
nc_data <- nc_open(nc_file)

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
time <- ncvar_get(nc_data, "time")
tunits  <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)




grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)
names(grid_data)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates
summary(grid_data)

output_csv_grid <- file.path(output_dir, paste0("rx5day_", scenario, ".csv"))
write.csv(grid_data, output_csv_grid, row.names = FALSE)



###########################585#######################

scenario <- 'ssp585'

nc_file = 'data/extremes/rx5day/ssp585/rx5day_CMIP6_ssp585_mon_201501-210012_v02.nc'

experiment_id <- scenario

if (scenario != 'hist'){
  # Define the start and end year
  start <- 2015 #fut 
  end <- 2100 #fut
} else{
  start <- 1980 #hist 
  end <- 2014 #hist
}



# Open the NetCDF file
nc_data <- nc_open(nc_file)

# Define the output directory
output_dir <- paste0("data/extremes/processed/rx5day_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

find_date(nc_file, start, end)

# Process the NetCDF file
#process_rx5day_data(nc_file, output_dir)

output_csv_grid <- file.path(output_dir, paste0("rx5day_CMIP6_", scenario, ".csv"))


# Read and combine grid-level Parquet files

output_dir
grid_data <- read_combine_parquet("rx5day_grid_timestep")

grid_data <- grid_data %>% filter(!is.na(rx5day_median))


# Open the NetCDF file
nc_data <- nc_open(nc_file)

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
time <- ncvar_get(nc_data, "time")
tunits  <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)




grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)
names(grid_data)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates
summary(grid_data)

output_csv_grid <- file.path(output_dir, paste0("rx5day_", scenario, ".csv"))
write.csv(grid_data, output_csv_grid, row.names = FALSE)

output_dta_grid <- file.path(output_dir, paste0("rx5day_", scenario, ".dta"))



#################################################################
#######################COMBINE###################################
#################################################################

##### Read hist and fut and merge###


##### Read hist and fut and merge###
scenario <- 'ssp126'
rx5day_fut <- read.csv(paste0('data/extremes/processed/rx5day_', scenario, '/rx5day_', scenario, '.csv')) #change
rx5day_hist <- read.csv('data/extremes/processed/rx5day_hist/rx5day_hist.csv')

rx5day <- bind_rows(rx5day_hist, rx5day_fut)

rx5day <- rx5day %>%
  arrange(year, month, lon, lat)

#Reorder columns
rx5day <- rx5day %>%
  dplyr::select(lon, lat, time_index, year, month, everything())

write.csv(rx5day, paste0('data/extremes/processed/rx5day_', scenario, '/rx5day_CMIP6_hist_', scenario, '.csv'), row.names = FALSE)

scenario <- 'ssp370'
rx5day_fut <- read.csv(paste0('data/extremes/processed/rx5day_', scenario, '/rx5day_', scenario, '.csv')) #change

rx5day <- bind_rows(rx5day_hist, rx5day_fut)

rx5day <- rx5day %>%
  arrange(year, month, lon, lat)

#Reorder columns
rx5day <- rx5day %>%
  dplyr::select(lon, lat, time_index, year, month, everything())

write.csv(rx5day, paste0('data/extremes/processed/rx5day_', scenario, '/rx5day_CMIP6_hist_', scenario, '.csv'), row.names = FALSE)

scenario <- 'ssp585'
rx5day_fut <- read.csv(paste0('data/extremes/processed/rx5day_', scenario,'/rx5day_', scenario, '.csv')) #change

rx5day <- bind_rows(rx5day_hist, rx5day_fut)

rx5day <- rx5day %>%
  arrange(year, month, lon, lat)

#Reorder columns
rx5day <- rx5day %>%
  dplyr::select(lon, lat, time_index, year, month, everything())

write.csv(rx5day, paste0('data/extremes/processed/rx5day_', scenario, '/rx5day_CMIP6_hist_', scenario, '.csv'), row.names = FALSE)

##### Read hist and fut and merge###
scenario <- 'ssp245'
rx5day_fut <- read.csv(paste0('data/extremes/processed/rx5day_', scenario, '/rx5day_', scenario, '.csv')) #change

rx5day <- bind_rows(rx5day_hist, rx5day_fut)

rx5day <- rx5day %>%
  arrange(year, month, lon, lat)

#Reorder columns
rx5day <- rx5day %>%
  dplyr::select(lon, lat, time_index, year, month, everything())

write.csv(rx5day, paste0('data/extremes/processed/rx5day_', scenario, '/rx5day_CMIP6_hist_', scenario, '.csv'), row.names = FALSE)

