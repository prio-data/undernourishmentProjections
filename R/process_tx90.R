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
library(data.table)

gc()
# Clean up temporary files
temp_files <- list.files(tempdir(), full.names = TRUE)
unlink(temp_files, recursive = TRUE)


# Set the working directory and get the list of NetCDF files
#setwd("")
source("code/replication_climate_data/functions.R")

#Process across all scenarios. Historical timeseries always the same!

#############################################
##############HIST###########################
#############################################

scenario  <- 'hist'

if (scenario =='hist'){
  start_year <- 1950
  end_year <- 2014
} else{start_year <- 2015
end_year <- 2100}



# Define the directory based on the scenario
directory <- paste0("data/extremes/tx90/tx90p_", scenario, "/")

# Use list.files to find the files with the ".nc" extension
nc_files <- list.files(directory, pattern = "\\.nc$", full.names = TRUE, recursive = T)


# Define the output directory for the processed Parquet files
output_dir <- paste0("data/extremes/processed/tx90p_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

  
# Open the NetCDF file and extract the necessary variables (the time index is the same across all models)

nc_data <- nc_open(nc_files[1])
time <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class and parse it
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)
range(time_cf$year)


# Find the start and end time indices from the first file
time_indices <- find_date(nc_files[1], start_year, end_year)
start_time_index <- time_indices$startdate
end_time_index <- time_indices$enddate

process_tx90_quantiles(nc_files, start_time_index, end_time_index, output_dir)


# Read and combine grid-level Parquet files
grid_data <- read_combine_parquet("tx90p_quantiles_time_index")

grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)

names(grid_data)
grid_data <- grid_data %>%  dplyr::select(lon, lat, year, month, tx90p_10th,tx90p_median, tx90p_90th)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates

output_csv_grid <- file.path(output_dir, paste0("tx90p_all.csv"))

write.csv(grid_data, output_csv_grid, row.names = FALSE)

output_dta_grid <- file.path(output_dir, paste0("tx90p_all.dta"))

library(haven)
write_dta(grid_data, output_dta_grid)


####################################
#############SSP 245################
####################################


scenario <- 'ssp245'

if (scenario =='hist'){
  start_year <- 1950
  end_year <- 2014
} else{start_year <- 2015
end_year <- 2100}



# Define the directory based on the scenario
directory <- paste0("data/extremes/tx90/tx90p_", scenario, "/")

# Use list.files to find the files with the ".nc" extension
nc_files <- list.files(directory, pattern = "\\.nc$", full.names = TRUE, recursive = T)


# Define the output directory for the processed Parquet files
output_dir <- paste0("data/extremes/processed/tx90p_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# Open the NetCDF file and extract the necessary variables (the time index is the same across all models)

nc_data <- nc_open(nc_files[1])
time <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class and parse it
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)
range(time_cf$year)


# Find the start and end time indices from the first file
time_indices <- find_date(nc_files[1], start_year, end_year)
start_time_index <- time_indices$startdate
end_time_index <- time_indices$enddate

process_tx90_quantiles(nc_files, start_time_index, end_time_index, output_dir)


# Read and combine grid-level Parquet files
grid_data <- read_combine_parquet("tx90p_quantiles_time_index")

grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)

names(grid_data)
grid_data <- grid_data %>%  dplyr::select(lon, lat, year, month, tx90p_10th,tx90p_median, tx90p_90th)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates

output_csv_grid <- file.path(output_dir, paste0("tx90p_all.csv"))

write.csv(grid_data, output_csv_grid, row.names = FALSE)

output_dta_grid <- file.path(output_dir, paste0("tx90p_all.dta"))

library(haven)
write_dta(grid_data, output_dta_grid)



####################################
#############SSP 126################
####################################


scenario <- 'ssp126'

if (scenario =='hist'){
  start_year <- 1950
  end_year <- 2014
} else{start_year <- 2015
end_year <- 2100}



# Define the directory based on the scenario
directory <- paste0("data/extremes/tx90/tx90p_", scenario, "/")

# Use list.files to find the files with the ".nc" extension
nc_files <- list.files(directory, pattern = "\\.nc$", full.names = TRUE, recursive = T)


# Define the output directory for the processed Parquet files
output_dir <- paste0("data/extremes/processed/tx90p_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# Open the NetCDF file and extract the necessary variables (the time index is the same across all models)

nc_data <- nc_open(nc_files[1])
time <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class and parse it
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)
range(time_cf$year)


# Find the start and end time indices from the first file
time_indices <- find_date(nc_files[1], start_year, end_year)
start_time_index <- time_indices$startdate
end_time_index <- time_indices$enddate

process_tx90_quantiles(nc_files, start_time_index, end_time_index, output_dir)


# Read and combine grid-level Parquet files
grid_data <- read_combine_parquet("tx90p_quantiles_time_index")

grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)

names(grid_data)
grid_data <- grid_data %>%  dplyr::select(lon, lat, year, month, tx90p_10th,tx90p_median, tx90p_90th)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates

output_csv_grid <- file.path(output_dir, paste0("tx90p_all.csv"))

write.csv(grid_data, output_csv_grid, row.names = FALSE)

output_dta_grid <- file.path(output_dir, paste0("tx90p_all.dta"))

library(haven)
write_dta(grid_data, output_dta_grid)


####################################
#############SSP 370################
####################################


scenario <- 'ssp370'

if (scenario =='hist'){
  start_year <- 1950
  end_year <- 2014
} else{start_year <- 2015
end_year <- 2100}



# Define the directory based on the scenario
directory <- paste0("data/extremes/tx90/tx90p_", scenario, "/")

# Use list.files to find the files with the ".nc" extension
nc_files <- list.files(directory, pattern = "\\.nc$", full.names = TRUE, recursive = T)


# Define the output directory for the processed Parquet files
output_dir <- paste0("data/extremes/processed/tx90p_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# Open the NetCDF file and extract the necessary variables (the time index is the same across all models)

nc_data <- nc_open(nc_files[1])
time <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class and parse it
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)
range(time_cf$year)


# Find the start and end time indices from the first file
time_indices <- find_date(nc_files[1], start_year, end_year)
start_time_index <- time_indices$startdate
end_time_index <- time_indices$enddate

process_tx90_quantiles(nc_files, start_time_index, end_time_index, output_dir)


# Read and combine grid-level Parquet files
grid_data <- read_combine_parquet("tx90p_quantiles_time_index")

grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)

names(grid_data)
grid_data <- grid_data %>%  dplyr::select(lon, lat, year, month, tx90p_10th,tx90p_median, tx90p_90th)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates

output_csv_grid <- file.path(output_dir, paste0("tx90p_all.csv"))

write.csv(grid_data, output_csv_grid, row.names = FALSE)

output_dta_grid <- file.path(output_dir, paste0("tx90p_all.dta"))

library(haven)
write_dta(grid_data, output_dta_grid)


####################################
#############SSP 585################
####################################

scenario <- 'ssp585'

if (scenario =='hist'){
  start_year <- 1950
  end_year <- 2014
} else{start_year <- 2015
end_year <- 2100}



# Define the directory based on the scenario
directory <- paste0("data/extremes/tx90/tx90p_", scenario, "/")

# Use list.files to find the files with the ".nc" extension
nc_files <- list.files(directory, pattern = "\\.nc$", full.names = TRUE, recursive = T)


# Define the output directory for the processed Parquet files
output_dir <- paste0("data/extremes/processed/tx90p_", scenario)

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# Open the NetCDF file and extract the necessary variables (the time index is the same across all models)

nc_data <- nc_open(nc_files[1])
time <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data, "time", "units")

# Convert time to CFtime class and parse it
cf <- CFtime(tunits$value, calendar = "standard", time)
timestamps <- as_timestamp(cf)
time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
time_cf$time_index <- row_number(time_cf)
range(time_cf$year)


# Find the start and end time indices from the first file
time_indices <- find_date(nc_files[1], start_year, end_year)
start_time_index <- time_indices$startdate
end_time_index <- time_indices$enddate

process_tx90_quantiles(nc_files, start_time_index, end_time_index, output_dir)


# Read and combine grid-level Parquet files
grid_data <- read_combine_parquet("tx90p_quantiles_time_index")

grid_data <- merge(grid_data, time_cf, by = 'time_index', all.x = TRUE)

names(grid_data)
grid_data <- grid_data %>%  dplyr::select(lon, lat, year, month, tx90p_10th,tx90p_median, tx90p_90th)

duplicates <- grid_data[duplicated(grid_data[, c("lon", "lat", "month", 'year')]), ]
duplicates

output_csv_grid <- file.path(output_dir, paste0("tx90p_all.csv"))

write.csv(grid_data, output_csv_grid, row.names = FALSE)

output_dta_grid <- file.path(output_dir, paste0("tx90p_all.dta"))

library(haven)
write_dta(grid_data, output_dta_grid)


#################################################################
#######################COMBINE###################################
#################################################################
library(haven)

##### Read hist and fut and merge###


####################################
#############SSP245#################
####################################


scenario <- 'ssp245'

tx90p_fut <- read.csv(paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_all.csv')) #change
tx90p_hist <- read.csv('data/extremes/processed/tx90p_hist/tx90p_all.csv')
tx90p_fut <- tx90p_fut %>%
  arrange(year, month, lon, lat)
tx90p_hist <- tx90p_hist %>%
  arrange(year, month, lon, lat)

tx90p <- bind_rows(tx90p_hist, tx90p_fut)

summary(tx90p)

write.csv(tx90p, paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_CMIP6_hist_', scenario, '.csv'), row.names = FALSE)
write_dta(tx90p, paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_CMIP6_hist_', scenario, '.dta'))



####################################
#############SSP126#################
####################################
scenario <- 'ssp126'

tx90p_fut <- read.csv(paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_all.csv')) #change
tx90p_hist <- read.csv('data/extremes/processed/tx90p_hist/tx90p_all.csv')
tx90p_fut <- tx90p_fut %>%
  arrange(year, month, lon, lat)
tx90p_hist <- tx90p_hist %>%
  arrange(year, month, lon, lat)

tx90p <- bind_rows(tx90p_hist, tx90p_fut)

summary(tx90p)

write.csv(tx90p, paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_CMIP6_hist_', scenario, '.csv'), row.names = FALSE)
write_dta(tx90p, paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_CMIP6_hist_', scenario, '.dta'))



####################################
#############SSP370#################
####################################

scenario <- 'ssp370'

tx90p_fut <- read.csv(paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_all.csv')) #change
tx90p_hist <- read.csv('data/extremes/processed/tx90p_hist/tx90p_all.csv')
tx90p_fut <- tx90p_fut %>%
  arrange(year, month, lon, lat)
tx90p_hist <- tx90p_hist %>%
  arrange(year, month, lon, lat)

tx90p <- bind_rows(tx90p_hist, tx90p_fut)

summary(tx90p)

write.csv(tx90p, paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_CMIP6_hist_', scenario, '.csv'), row.names = FALSE)
write_dta(tx90p, paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_CMIP6_hist_', scenario, '.dta'))



####################################
#############SSP585#################
####################################

scenario <- 'ssp585'

tx90p_fut <- read.csv(paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_all.csv')) #change
tx90p_hist <- read.csv('data/extremes/processed/tx90p_hist/tx90p_all.csv')
tx90p_fut <- tx90p_fut %>%
  arrange(year, month, lon, lat)
tx90p_hist <- tx90p_hist %>%
  arrange(year, month, lon, lat)

tx90p <- bind_rows(tx90p_hist, tx90p_fut)

summary(tx90p)

write.csv(tx90p, paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_CMIP6_hist_', scenario, '.csv'), row.names = FALSE)
write_dta(tx90p, paste0('data/extremes/processed/tx90p_', scenario, '/tx90p_CMIP6_hist_', scenario, '.dta'))


