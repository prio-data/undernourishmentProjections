rm(list = ls(all = TRUE))
gc()

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
library(Rssa)
library(zoo)
library(lubridate)

# Clean up temporary files
temp_files <- list.files(tempdir(), full.names = TRUE)
unlink(temp_files, recursive = TRUE)

# Set the working directory and get the list of NetCDF files
#setwd("")
source("code/replication_climate_data/functions.R")


# ===============================================================
# Load climate data
# ===============================================================

# ---------- SSP245 ----------
spi6_ssp245  <- read.csv('data/extremes/processed/spi6_ssp245/spi6_CMIP6_hist_ssp245.csv')
tx90p_ssp245 <- read.csv('data/extremes/processed/tx90p_ssp245/tx90p_CMIP6_hist_ssp245.csv')
spei_ssp245  <- read.csv('data/extremes/processed/spei6_ssp245/spei6_CMIP6_hist_ssp245.csv')
rx5day_ssp245 <- read.csv('data/extremes/processed/rx5day_ssp245/rx5day_CMIP6_hist_ssp245.csv')

spi6_ssp245 <- spi6_ssp245 %>%
  dplyr::select(lon, lat, year, month, country_id, country_name, spi6_median) %>%
  rename(spi6_median_ssp245 = spi6_median)

tx90p_ssp245 <- tx90p_ssp245 %>%
  dplyr::select(lon, lat, year, month, tx90p_median) %>%
  rename(tx90p_median_ssp245 = tx90p_median)

spei_ssp245 <- spei_ssp245 %>%
  dplyr::select(lon, lat, year, month, spei6_median) %>%
  rename(spei6_median_ssp245 = spei6_median)

rx5day_ssp245 <- rx5day_ssp245 %>%
  dplyr::select(lon, lat, year, month, rx5day_median) %>%
  rename(rx5day_median_ssp245 = rx5day_median)

# ---------- SSP126 ----------
spi6_ssp126  <- read.csv('data/extremes/processed/spi6_ssp126/spi6_CMIP6_hist_ssp126.csv')
tx90p_ssp126 <- read.csv('data/extremes/processed/tx90p_ssp126/tx90p_CMIP6_hist_ssp126.csv')
spei_ssp126  <- read.csv('data/extremes/processed/spei6_ssp126/spei6_CMIP6_hist_ssp126.csv')
rx5day_ssp126 <- read.csv('data/extremes/processed/rx5day_ssp126/rx5day_CMIP6_hist_ssp126.csv')

spi6_ssp126 <- spi6_ssp126 %>%
  dplyr::select(lon, lat, year, month, spi6_median) %>%
  rename(spi6_median_ssp126 = spi6_median)

tx90p_ssp126 <- tx90p_ssp126 %>%
  dplyr::select(lon, lat, year, month, tx90p_median) %>%
  rename(tx90p_median_ssp126 = tx90p_median)

spei_ssp126 <- spei_ssp126 %>%
  dplyr::select(lon, lat, year, month, spei6_median) %>%
  rename(spei6_median_ssp126 = spei6_median)

rx5day_ssp126 <- rx5day_ssp126 %>%
  dplyr::select(lon, lat, year, month, rx5day_median) %>%
  rename(rx5day_median_ssp126 = rx5day_median)

# ---------- SSP370 ----------
spi6_ssp370  <- read.csv('data/extremes/processed/spi6_ssp370/spi6_CMIP6_hist_ssp370.csv')
tx90p_ssp370 <- read.csv('data/extremes/processed/tx90p_ssp370/tx90p_CMIP6_hist_ssp370.csv')
spei_ssp370  <- read.csv('data/extremes/processed/spei6_ssp370/spei6_CMIP6_hist_ssp370.csv')
rx5day_ssp370 <- read.csv('data/extremes/processed/rx5day_ssp370/rx5day_CMIP6_hist_ssp370.csv')

spi6_ssp370$time_index <- NULL
spei_ssp370$time_index <- NULL
rx5day_ssp370$time_index <- NULL

spi6_ssp370 <- spi6_ssp370 %>%
  dplyr::select(lon, lat, year, month, spi6_median) %>%
  rename(spi6_median_ssp370 = spi6_median)

tx90p_ssp370 <- tx90p_ssp370 %>%
  dplyr::select(lon, lat, year, month, tx90p_median) %>%
  rename(tx90p_median_ssp370 = tx90p_median)

spei_ssp370 <- spei_ssp370 %>%
  dplyr::select(lon, lat, year, month, spei6_median) %>%
  rename(spei6_median_ssp370 = spei6_median)

rx5day_ssp370 <- rx5day_ssp370 %>%
  dplyr::select(lon, lat, year, month, rx5day_median) %>%
  rename(rx5day_median_ssp370 = rx5day_median)

# ---------- SSP585 ----------
spi6_ssp585  <- read.csv('data/extremes/processed/spi6_ssp585/spi6_CMIP6_hist_ssp585.csv')
tx90p_ssp585 <- read.csv('data/extremes/processed/tx90p_ssp585/tx90p_CMIP6_hist_ssp585.csv')
spei_ssp585  <- read.csv('data/extremes/processed/spei6_ssp585/spei6_CMIP6_hist_ssp585.csv')
rx5day_ssp585 <- read.csv('data/extremes/processed/rx5day_ssp585/rx5day_CMIP6_hist_ssp585.csv')

spi6_ssp585 <- spi6_ssp585 %>%
  dplyr::select(lon, lat, year, month, spi6_median) %>%
  rename(spi6_median_ssp585 = spi6_median)

tx90p_ssp585 <- tx90p_ssp585 %>%
  dplyr::select(lon, lat, year, month, tx90p_median) %>%
  rename(tx90p_median_ssp585 = tx90p_median)

spei_ssp585 <- spei_ssp585 %>%
  dplyr::select(lon, lat, year, month, spei6_median) %>%
  rename(spei6_median_ssp585 = spei6_median)

rx5day_ssp585 <- rx5day_ssp585 %>%
  dplyr::select(lon, lat, year, month, rx5day_median) %>%
  rename(rx5day_median_ssp585 = rx5day_median)

# ===============================================================
# Merge climate variables by scenario
# ===============================================================

clim_ssp245 <- left_join(spi6_ssp245, tx90p_ssp245, by = c("lon", "lat", "month", "year"))
clim_ssp245 <- left_join(clim_ssp245, spei_ssp245, by = c("lon", "lat", "month", "year"))
clim_ssp245 <- left_join(clim_ssp245, rx5day_ssp245, by = c("lon", "lat", "month", "year"))

clim_ssp126 <- left_join(spi6_ssp126, tx90p_ssp126, by = c("lon", "lat", "month", "year"))
clim_ssp126 <- left_join(clim_ssp126, spei_ssp126, by = c("lon", "lat", "month", "year"))
clim_ssp126 <- left_join(clim_ssp126, rx5day_ssp126, by = c("lon", "lat", "month", "year"))

clim_ssp370 <- left_join(spi6_ssp370, tx90p_ssp370, by = c("lon", "lat", "month", "year"))
clim_ssp370 <- left_join(clim_ssp370, spei_ssp370, by = c("lon", "lat", "month", "year"))
clim_ssp370 <- left_join(clim_ssp370, rx5day_ssp370, by = c("lon", "lat", "month", "year"))

clim_ssp585 <- left_join(spi6_ssp585, tx90p_ssp585, by = c("lon", "lat", "month", "year"))
clim_ssp585 <- left_join(clim_ssp585, spei_ssp585, by = c("lon", "lat", "month", "year"))
clim_ssp585 <- left_join(clim_ssp585, rx5day_ssp585, by = c("lon", "lat", "month", "year"))

clim_12 <- left_join(clim_ssp245, clim_ssp126, by = c("lon", "lat", "month", "year"))
clim_35 <- left_join(clim_ssp370, clim_ssp585, by = c("lon", "lat", "month", "year"))

clim_all <- left_join(clim_12, clim_35, by = c("lon", "lat", "month", "year"))

write.csv(clim_all, "data/final/climate_all_scenarios.csv")

# ===============================================================
# Merge with crop growing season data
# ===============================================================

crops_gs <- read.csv("data/crop/mapspam/processed/crops_gs.csv")

clim_all <- clim_all %>%
  mutate(lon = round(as.numeric(lon), 2),
         lat = round(as.numeric(lat), 2))

data <- inner_join(clim_all, crops_gs, by = c("lon", "lat"))

data <- data %>%
  mutate(
    planting_date = as.Date(planting_day - 1, origin = paste0(year, "-01-01")),
    maturity_date = as.Date(
      maturity_day - 1,
      origin = ifelse(maturity_day < planting_day,
                      paste0(year + 1, "-01-01"),
                      paste0(year, "-01-01"))
    ),
    month_start = as.Date(paste(year, month, "01", sep = "-")),
    month_end   = ceiling_date(month_start, "month") - days(1),
    overlap_start = pmax(planting_date, month_start),
    overlap_end   = pmin(maturity_date, month_end),
    days_in_month   = as.numeric(month_end - month_start + 1),
    days_in_overlap = as.numeric(pmax(0, overlap_end - overlap_start + 1)),
    adjusted_gs_days = days_in_overlap,
    
    adjusted_tx90_126 = (tx90p_median_ssp126 / 100) * days_in_overlap,
    adjusted_tx90_245 = (tx90p_median_ssp245 / 100) * days_in_overlap,
    adjusted_tx90_370 = (tx90p_median_ssp370 / 100) * days_in_overlap,
    adjusted_tx90_585 = (tx90p_median_ssp585 / 100) * days_in_overlap,
    
    weighted_spi6_126  = spi6_median_ssp126  * days_in_overlap,
    weighted_spi6_245  = spi6_median_ssp245  * days_in_overlap,
    weighted_spi6_370  = spi6_median_ssp370  * days_in_overlap,
    weighted_spi6_585  = spi6_median_ssp585  * days_in_overlap,
    
    weighted_spei6_126 = spei6_median_ssp126 * days_in_overlap,
    weighted_spei6_245 = spei6_median_ssp245 * days_in_overlap,
    weighted_spei6_370 = spei6_median_ssp370 * days_in_overlap,
    weighted_spei6_585 = spei6_median_ssp585 * days_in_overlap,
    
    weighted_rx5day_126 = rx5day_median_ssp126 * days_in_overlap,
    weighted_rx5day_245 = rx5day_median_ssp245 * days_in_overlap,
    weighted_rx5day_370 = rx5day_median_ssp370 * days_in_overlap,
    weighted_rx5day_585 = rx5day_median_ssp585 * days_in_overlap
  )

data_yearly <- data %>%
  group_by(lon, lat, year) %>%
  summarise(
    country_id   = first(country_id),
    country_name = first(country_name),
    crop_harvarea = first(crop_harvarea),
    
    gs_days = sum(adjusted_gs_days, na.rm = TRUE),
    
    tx90p_126_gs_total = sum(adjusted_tx90_126, na.rm = TRUE),
    tx90p_245_gs_total = sum(adjusted_tx90_245, na.rm = TRUE),
    tx90p_370_gs_total = sum(adjusted_tx90_370, na.rm = TRUE),
    tx90p_585_gs_total = sum(adjusted_tx90_585, na.rm = TRUE),
    
    tx90p_126_gs_share = ifelse(gs_days > 0, tx90p_126_gs_total / gs_days, NA),
    tx90p_245_gs_share = ifelse(gs_days > 0, tx90p_245_gs_total / gs_days, NA),
    tx90p_370_gs_share = ifelse(gs_days > 0, tx90p_370_gs_total / gs_days, NA),
    tx90p_585_gs_share = ifelse(gs_days > 0, tx90p_585_gs_total / gs_days, NA),
    
    spi6_126_gs = ifelse(gs_days > 0, sum(weighted_spi6_126,  na.rm = TRUE) / gs_days, NA),
    spi6_245_gs = ifelse(gs_days > 0, sum(weighted_spi6_245,  na.rm = TRUE) / gs_days, NA),
    spi6_370_gs = ifelse(gs_days > 0, sum(weighted_spi6_370,  na.rm = TRUE) / gs_days, NA),
    spi6_585_gs = ifelse(gs_days > 0, sum(weighted_spi6_585,  na.rm = TRUE) / gs_days, NA),
    
    spei6_126_gs = ifelse(gs_days > 0, sum(weighted_spei6_126, na.rm = TRUE) / gs_days, NA),
    spei6_245_gs = ifelse(gs_days > 0, sum(weighted_spei6_245, na.rm = TRUE) / gs_days, NA),
    spei6_370_gs = ifelse(gs_days > 0, sum(weighted_spei6_370, na.rm = TRUE) / gs_days, NA),
    spei6_585_gs = ifelse(gs_days > 0, sum(weighted_spei6_585, na.rm = TRUE) / gs_days, NA),
    
    rx5day_126_gs = ifelse(gs_days > 0, sum(weighted_rx5day_126, na.rm = TRUE) / gs_days, NA),
    rx5day_245_gs = ifelse(gs_days > 0, sum(weighted_rx5day_245, na.rm = TRUE) / gs_days, NA),
    rx5day_370_gs = ifelse(gs_days > 0, sum(weighted_rx5day_370, na.rm = TRUE) / gs_days, NA),
    rx5day_585_gs = ifelse(gs_days > 0, sum(weighted_rx5day_585, na.rm = TRUE) / gs_days, NA),
    
    .groups = "drop"
  )

write_parquet(data, 'data/final/clim_all.parquet')

#data <- read_parquet('data/final/clim_all.parquet')


aggregated_data <- data_yearly %>%
  group_by(country_id, country_name, year) %>%
  summarise(
    
    # total harvested area in country-year
    total_harvarea = sum(crop_harvarea, na.rm = TRUE),
    
    # ---------- SSP126 ----------
    tx90p_126_gs = if (total_harvarea > 0) {
      weighted.mean(tx90p_126_gs_share, crop_harvarea, na.rm = TRUE)
    } else {
      mean(tx90p_126_gs_share, na.rm = TRUE)
    },
    
    spi6_126_gs = if (total_harvarea > 0) {
      weighted.mean(spi6_126_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(spi6_126_gs, na.rm = TRUE)
    },
    
    spei6_126_gs = if (total_harvarea > 0) {
      weighted.mean(spei6_126_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(spei6_126_gs, na.rm = TRUE)
    },
    
    rx5day_126_gs = if (total_harvarea > 0) {
      weighted.mean(rx5day_126_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(rx5day_126_gs, na.rm = TRUE)
    },
    
    # ---------- SSP245 ----------
    tx90p_245_gs = if (total_harvarea > 0) {
      weighted.mean(tx90p_245_gs_share, crop_harvarea, na.rm = TRUE)
    } else {
      mean(tx90p_245_gs_share, na.rm = TRUE)
    },
    
    spi6_245_gs = if (total_harvarea > 0) {
      weighted.mean(spi6_245_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(spi6_245_gs, na.rm = TRUE)
    },
    
    spei6_245_gs = if (total_harvarea > 0) {
      weighted.mean(spei6_245_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(spei6_245_gs, na.rm = TRUE)
    },
    
    rx5day_245_gs = if (total_harvarea > 0) {
      weighted.mean(rx5day_245_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(rx5day_245_gs, na.rm = TRUE)
    },
    
    # ---------- SSP370 ----------
    tx90p_370_gs = if (total_harvarea > 0) {
      weighted.mean(tx90p_370_gs_share, crop_harvarea, na.rm = TRUE)
    } else {
      mean(tx90p_370_gs_share, na.rm = TRUE)
    },
    
    spi6_370_gs = if (total_harvarea > 0) {
      weighted.mean(spi6_370_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(spi6_370_gs, na.rm = TRUE)
    },
    
    spei6_370_gs = if (total_harvarea > 0) {
      weighted.mean(spei6_370_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(spei6_370_gs, na.rm = TRUE)
    },
    
    rx5day_370_gs = if (total_harvarea > 0) {
      weighted.mean(rx5day_370_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(rx5day_370_gs, na.rm = TRUE)
    },
    
    # ---------- SSP585 ----------
    tx90p_585_gs = if (total_harvarea > 0) {
      weighted.mean(tx90p_585_gs_share, crop_harvarea, na.rm = TRUE)
    } else {
      mean(tx90p_585_gs_share, na.rm = TRUE)
    },
    
    spi6_585_gs = if (total_harvarea > 0) {
      weighted.mean(spi6_585_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(spi6_585_gs, na.rm = TRUE)
    },
    
    spei6_585_gs = if (total_harvarea > 0) {
      weighted.mean(spei6_585_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(spei6_585_gs, na.rm = TRUE)
    },
    
    rx5day_585_gs = if (total_harvarea > 0) {
      weighted.mean(rx5day_585_gs, crop_harvarea, na.rm = TRUE)
    } else {
      mean(rx5day_585_gs, na.rm = TRUE)
    },
    
    .groups = "drop"
  )

duplicates <- aggregated_data %>%
  group_by(country_id, year) %>%
  filter(n() > 1)

summary(aggregated_data)
write.csv(aggregated_data,
          'data/final/clim_growingseason_country_year_allssps.csv',
          row.names = FALSE)
haven::write_dta(aggregated_data,
                 'data/final/clim_growingseason_country_year_allssps.dta')

#aggregated_data <- read.csv('data/final/clim_growingseason_country_year_allssps.csv')

###### Simple aggregations without the weighting and growing season ######

aggregated_data_unweighted <- data %>%
  group_by(country_name, country_id, year) %>%
  summarize(
    # SSP126
    spi6_126 = mean(spi6_median_ssp126, na.rm = TRUE),
    spei6_126 = mean(spei6_median_ssp126, na.rm = TRUE),
    tx90p_126 = mean(tx90p_median_ssp126, na.rm = TRUE),
    rx5day_126 = mean(rx5day_median_ssp126, na.rm = TRUE),
    
    # SSP245
    spi6_245 = mean(spi6_median_ssp245, na.rm = TRUE),
    spei6_245 = mean(spei6_median_ssp245, na.rm = TRUE),
    tx90p_245 = mean(tx90p_median_ssp245, na.rm = TRUE),
    rx5day_245 = mean(rx5day_median_ssp245, na.rm = TRUE),
    
    # SSP370
    spi6_370 = mean(spi6_median_ssp370, na.rm = TRUE),
    spei6_370 = mean(spei6_median_ssp370, na.rm = TRUE),
    tx90p_370 = mean(tx90p_median_ssp370, na.rm = TRUE),
    rx5day_370 = mean(rx5day_median_ssp370, na.rm = TRUE),
    
    # SSP585
    spi6_585 = mean(spi6_median_ssp585, na.rm = TRUE),
    spei6_585 = mean(spei6_median_ssp585, na.rm = TRUE),
    tx90p_585 = mean(tx90p_median_ssp585, na.rm = TRUE),
    rx5day_585 = mean(rx5day_median_ssp585, na.rm = TRUE),
    
    .groups = "drop"
  )

summary(aggregated_data_unweighted)

write.csv(aggregated_data_unweighted,
          'data/final/clim_unweighted_country_year_allssps.csv',
          row.names = FALSE)
haven::write_dta(aggregated_data_unweighted,
                 'data/final/clim_unweighted_country_year_allssps.dta')

#############################
########DIAGNOSTICS##########
#############################


spg <- data %>% dplyr::select(lon, lat, tx90p_median_ssp126)
coordinates(spg) <- ~ lon + lat

gridded(spg) <- TRUE
rasterDF <- raster(spg)
plot(rasterDF)

data_yearly$continent <- countrycode::countrycode(data_yearly$country_name,
                                                  'country.name',
                                                  'continent')
data_yearly$continent[is.na(data_yearly$continent) &
                        data_yearly$country_name == "Kosovo"] <- "Europe"
data_yearly$continent[is.na(data_yearly$continent) &
                        data_yearly$country_name == "Czechoslovakia"] <- "Europe"
data_yearly$continent[is.na(data_yearly$continent) &
                        data_yearly$country_name == "German Democratic Republic"] <- "Europe"
data_yearly$continent[is.na(data_yearly$continent) &
                        data_yearly$country_name == "Yugoslavia"] <- "Europe"
data_yearly$continent[is.na(data_yearly$continent) &
                        data_yearly$country_name == "Yemen (Arab Republic of Yemen)"] <- "Asia"
data_yearly$continent[is.na(data_yearly$continent) &
                        data_yearly$country_name == "Yemen, People's Republic of"] <- "Asia"

temporal_trend <- data_yearly %>%
  group_by(year) %>%
  summarize(median_var = median(tx90p_585_gs_share, na.rm = TRUE))

ggplot(temporal_trend, aes(x = year, y = median_var)) +
  geom_line() +
  labs(title = "Temporal Trend", x = "Year", y = "Median") +
  theme_minimal()

ranges <- c(-Inf, -2, -1.5, -1, 0, 1, 1.5, 2, Inf)
range_labels <- c("<= -2", "> -2 & <= -1.5", "> -1.5 & <= -1",
                  "> -1 & <= 0", "> 0 & <= 1", "> 1 & <= 1.5",
                  "> 1.5 & <= 2", "> 2")

percentile_table <- data %>%
  mutate(range = cut(tx90p_median_ssp245,
                     breaks = ranges,
                     labels = range_labels,
                     right = TRUE)) %>%
  group_by(range) %>%
  summarize(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

aggregated_data$continent <- countrycode::countrycode(aggregated_data$country_name,
                                                      'country.name',
                                                      'continent')
aggregated_data$continent[is.na(aggregated_data$continent) &
                            aggregated_data$country_name == "Kosovo"] <- "Europe"
aggregated_data$continent[is.na(aggregated_data$continent) &
                            aggregated_data$country_name == "Czechoslovakia"] <- "Europe"
aggregated_data$continent[is.na(aggregated_data$continent) &
                            aggregated_data$country_name == "German Democratic Republic"] <- "Europe"
aggregated_data$continent[is.na(aggregated_data$continent) &
                            aggregated_data$country_name == "Yugoslavia"] <- "Europe"
aggregated_data$continent[is.na(aggregated_data$continent) &
                            aggregated_data$country_name == "Yemen (Arab Republic of Yemen)"] <- "Asia"
aggregated_data$continent[is.na(aggregated_data$continent) &
                            aggregated_data$country_name == "Yemen, People's Republic of"] <- "Asia"

temporal_trend <- aggregated_data %>%
  group_by(year, continent) %>%
  summarize(mean = mean(spei6_245_gs, na.rm = TRUE))

ggplot(temporal_trend, aes(x = year, y = mean, color = continent)) +
  geom_line() +
  labs(title = "Temporal Trend by Continent",
       x = "Year",
       y = "Median") +
  theme_minimal()

summary(aggregated_data)


trend_global <- aggregated_data %>%
  group_by(year) %>%
  summarise(
    tx90p_126 = mean(tx90p_126_gs, na.rm = TRUE),
    tx90p_245 = mean(tx90p_245_gs, na.rm = TRUE),
    tx90p_370 = mean(tx90p_370_gs, na.rm = TRUE),
    tx90p_585 = mean(tx90p_585_gs, na.rm = TRUE),
    
    spi6_126 = mean(spi6_126_gs, na.rm = TRUE),
    spi6_245 = mean(spi6_245_gs, na.rm = TRUE),
    spi6_370 = mean(spi6_370_gs, na.rm = TRUE),
    spi6_585 = mean(spi6_585_gs, na.rm = TRUE),
    
    spei6_126 = mean(spei6_126_gs, na.rm = TRUE),
    spei6_245 = mean(spei6_245_gs, na.rm = TRUE),
    spei6_370 = mean(spei6_370_gs, na.rm = TRUE),
    spei6_585 = mean(spei6_585_gs, na.rm = TRUE),
    
    rx5day_126 = mean(rx5day_126_gs, na.rm = TRUE),
    rx5day_245 = mean(rx5day_245_gs, na.rm = TRUE),
    rx5day_370 = mean(rx5day_370_gs, na.rm = TRUE),
    rx5day_585 = mean(rx5day_585_gs, na.rm = TRUE)
  )


ggplot(trend_global, aes(x = year)) +
  geom_line(aes(y = tx90p_126, color = "SSP126")) +
  geom_line(aes(y = tx90p_245, color = "SSP245")) +
  geom_line(aes(y = tx90p_370, color = "SSP370")) +
  geom_line(aes(y = tx90p_585, color = "SSP585")) +
  labs(title = "Global Mean TX90p (Growing Season)",
       y = "Share of GS days",
       color = "Scenario") +
  theme_minimal()


ggplot(trend_global, aes(x = year)) +
  geom_line(aes(y = spei6_126, color = "SSP126")) +
  geom_line(aes(y = spei6_245, color = "SSP245")) +
  geom_line(aes(y = spei6_370, color = "SSP370")) +
  geom_line(aes(y = spei6_585, color = "SSP585")) +
  labs(title = "Global Mean spei6 (Growing Season)",
       y = "Share of GS days",
       color = "Scenario") +
  theme_minimal()


ggplot(trend_global, aes(x = year)) +
  geom_line(aes(y = spi6_126, color = "SSP126")) +
  geom_line(aes(y = spi6_245, color = "SSP245")) +
  geom_line(aes(y = spi6_370, color = "SSP370")) +
  geom_line(aes(y = spi6_585, color = "SSP585")) +
  labs(title = "Global Mean spi6 (Growing Season)",
       y = "Share of GS days",
       color = "Scenario") +
  theme_minimal()


ggplot(trend_global, aes(x = year)) +
  geom_line(aes(y = rx5day_126, color = "SSP126")) +
  geom_line(aes(y = rx5day_245, color = "SSP245")) +
  geom_line(aes(y = rx5day_370, color = "SSP370")) +
  geom_line(aes(y = rx5day_585, color = "SSP585")) +
  labs(title = "Global Mean rx5day (Growing Season)",
       y = "Share of GS days",
       color = "Scenario") +
  theme_minimal()



scenario_correlations <- aggregated_data %>%
  summarise(
    cor_spi_126_245 = cor(spi6_126_gs, spi6_245_gs, use = "complete.obs"),
    cor_spi_245_370 = cor(spi6_245_gs, spi6_370_gs, use = "complete.obs"),
    cor_spi_370_585 = cor(spi6_370_gs, spi6_585_gs, use = "complete.obs"),
    
    cor_spei_126_245 = cor(spei6_126_gs, spei6_245_gs, use = "complete.obs"),
    cor_spei_245_370 = cor(spei6_245_gs, spei6_370_gs, use = "complete.obs"),
    cor_spei_370_585 = cor(spei6_370_gs, spei6_585_gs, use = "complete.obs"),
    
    cor_rx5_126_245 = cor(rx5day_126_gs, rx5day_245_gs, use = "complete.obs"),
    cor_rx5_245_370 = cor(rx5day_245_gs, rx5day_370_gs, use = "complete.obs"),
    cor_rx5_370_585 = cor(rx5day_370_gs, rx5day_585_gs, use = "complete.obs")
  )

print(scenario_correlations)


lat_diagnostics <- data_yearly %>%
  group_by(lat) %>%
  summarise(
    tx90p = mean(tx90p_585_gs_share, na.rm = TRUE),
    spei6 = mean(spei6_585_gs, na.rm = TRUE),
    rx5day = mean(rx5day_585_gs, na.rm = TRUE)
  )

ggplot(lat_diagnostics, aes(x = lat)) +
  geom_smooth(aes(y = tx90p, color = "TX90p"), method = "gam") +
  geom_smooth(aes(y = spei6, color = "SPEI6"), method = "gam") +
  geom_smooth(aes(y = rx5day, color = "RX5day"), method = "gam") +
  labs(title = "Latitude Gradients (SSP585)",
       y = "Indicator value",
       color = "Variable") +
  theme_minimal()



lat_diagnostics <- data_yearly %>%
  group_by(lat) %>%
  summarise(
    tx90p = mean(tx90p_245_gs_share, na.rm = TRUE),
    spei6 = mean(spei6_245_gs, na.rm = TRUE),
    rx5day = mean(rx5day_245_gs, na.rm = TRUE)
  )

ggplot(lat_diagnostics, aes(x = lat)) +
  geom_smooth(aes(y = tx90p, color = "TX90p"), method = "gam") +
  geom_smooth(aes(y = spei6, color = "SPEI6"), method = "gam") +
  geom_smooth(aes(y = rx5day, color = "RX5day"), method = "gam") +
  labs(title = "Latitude Gradients (SSP245)",
       y = "Indicator value",
       color = "Variable") +
  theme_minimal()

lat_diagnostics <- data_yearly %>%
  group_by(lat) %>%
  summarise(
    tx90p = mean(tx90p_370_gs_share, na.rm = TRUE),
    spei6 = mean(spei6_370_gs, na.rm = TRUE),
    rx5day = mean(rx5day_370_gs, na.rm = TRUE)
  )

ggplot(lat_diagnostics, aes(x = lat)) +
  geom_smooth(aes(y = tx90p, color = "TX90p"), method = "gam") +
  geom_smooth(aes(y = spei6, color = "SPEI6"), method = "gam") +
  geom_smooth(aes(y = rx5day, color = "RX5day"), method = "gam") +
  labs(title = "Latitude Gradients (SSP370)",
       y = "Indicator value",
       color = "Variable") +
  theme_minimal()

lat_diagnostics <- data_yearly %>%
  group_by(lat) %>%
  summarise(
    tx90p = mean(tx90p_126_gs_share, na.rm = TRUE),
    spei6 = mean(spei6_126_gs, na.rm = TRUE),
    rx5day = mean(rx5day_126_gs, na.rm = TRUE)
  )

ggplot(lat_diagnostics, aes(x = lat)) +
  geom_smooth(aes(y = tx90p, color = "TX90p"), method = "gam") +
  geom_smooth(aes(y = spei6, color = "SPEI6"), method = "gam") +
  geom_smooth(aes(y = rx5day, color = "RX5day"), method = "gam") +
  labs(title = "Latitude Gradients (SSP126)",
       y = "Indicator value",
       color = "Variable") +
  theme_minimal()
summary(data_yearly$gs_days)



rx5_long <- aggregated_data %>%
  dplyr::select(year,
         rx5day_126_gs,
         rx5day_245_gs,
         rx5day_370_gs,
         rx5day_585_gs) %>%
  pivot_longer(-year, names_to = "scenario", values_to = "rx5day")

ggplot(rx5_long, aes(x = rx5day, fill = scenario)) +
  geom_density(alpha = 0.3) +
  labs(title = "RX5day Distribution by Scenario",
       x = "RX5day (GS mean)",
       y = "Density") +
  theme_minimal()



