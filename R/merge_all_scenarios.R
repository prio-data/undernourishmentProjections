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
setwd("") #CHANGE
source("code/replication_climate_data/functions.R")


# ===============================================================
# Load climate data
# ===============================================================

# ---------- SSP245 ----------
spi6_ssp245  <- fread('data/extremes/processed/spi6_ssp245/spi6_CMIP6_hist_ssp245.csv')
tx90p_ssp245 <- fread('data/extremes/processed/tx90p_ssp245/tx90p_CMIP6_hist_ssp245.csv')
spei_ssp245  <- fread('data/extremes/processed/spei6_ssp245/spei6_CMIP6_hist_ssp245.csv')
rx5day_ssp245 <- fread('data/extremes/processed/rx5day_ssp245/rx5day_CMIP6_hist_ssp245.csv')

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
spi6_ssp126  <- fread('data/extremes/processed/spi6_ssp126/spi6_CMIP6_hist_ssp126.csv')
tx90p_ssp126 <- fread('data/extremes/processed/tx90p_ssp126/tx90p_CMIP6_hist_ssp126.csv')
spei_ssp126  <- fread('data/extremes/processed/spei6_ssp126/spei6_CMIP6_hist_ssp126.csv')
rx5day_ssp126 <- fread('data/extremes/processed/rx5day_ssp126/rx5day_CMIP6_hist_ssp126.csv')

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
spi6_ssp370  <- fread('data/extremes/processed/spi6_ssp370/spi6_CMIP6_hist_ssp370.csv')
tx90p_ssp370 <- fread('data/extremes/processed/tx90p_ssp370/tx90p_CMIP6_hist_ssp370.csv')
spei_ssp370  <- fread('data/extremes/processed/spei6_ssp370/spei6_CMIP6_hist_ssp370.csv')
rx5day_ssp370 <- fread('data/extremes/processed/rx5day_ssp370/rx5day_CMIP6_hist_ssp370.csv')

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
spi6_ssp585  <- fread('data/extremes/processed/spi6_ssp585/spi6_CMIP6_hist_ssp585.csv')
tx90p_ssp585 <- fread('data/extremes/processed/tx90p_ssp585/tx90p_CMIP6_hist_ssp585.csv')
spei_ssp585  <- fread('data/extremes/processed/spei6_ssp585/spei6_CMIP6_hist_ssp585.csv')
rx5day_ssp585 <- fread('data/extremes/processed/rx5day_ssp585/rx5day_CMIP6_hist_ssp585.csv')

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

# CHANGED: joins are no longer based on SPI6.
# Country assignment (from the SPI6 country extraction) is a property of the grid cell,
# with fixed borders, so it is taken once per cell and attached to TX90p, which is used
# as the base. SPI6, SPEI6 and RX5day are then joined on. This way, cell-months where
# SPI6 is missing (e.g. dry cells, or months where the 6-month index cannot be computed)
# keep their TX90p and RX5day values instead of being dropped.

# One country per cell
cell_country <- spi6_ssp245 %>%
  distinct(lon, lat, country_id, country_name)

cell_country %>% count(lon, lat) %>% filter(n > 1) %>% nrow()   # should be 0 (cells assigned to >1 country)
cell_country <- cell_country %>% distinct(lon, lat, .keep_all = TRUE)   # safeguard: one country per cell

spi6_ssp245 <- spi6_ssp245 %>% dplyr::select(-country_id, -country_name)

# Check: cells lost because they have no country assignment (no SPI6 values at all)
nrow(anti_join(distinct(tx90p_ssp245, lon, lat), cell_country, by = c("lon", "lat")))

clim_ssp245 <- inner_join(tx90p_ssp245, cell_country, by = c("lon", "lat"))
clim_ssp245 <- left_join(clim_ssp245, spi6_ssp245, by = c("lon", "lat", "month", "year"))
clim_ssp245 <- left_join(clim_ssp245, spei_ssp245, by = c("lon", "lat", "month", "year"))
clim_ssp245 <- left_join(clim_ssp245, rx5day_ssp245, by = c("lon", "lat", "month", "year"))

clim_ssp126 <- left_join(tx90p_ssp126, spi6_ssp126, by = c("lon", "lat", "month", "year"))
clim_ssp126 <- left_join(clim_ssp126, spei_ssp126, by = c("lon", "lat", "month", "year"))
clim_ssp126 <- left_join(clim_ssp126, rx5day_ssp126, by = c("lon", "lat", "month", "year"))

clim_ssp370 <- left_join(tx90p_ssp370, spi6_ssp370, by = c("lon", "lat", "month", "year"))
clim_ssp370 <- left_join(clim_ssp370, spei_ssp370, by = c("lon", "lat", "month", "year"))
clim_ssp370 <- left_join(clim_ssp370, rx5day_ssp370, by = c("lon", "lat", "month", "year"))

clim_ssp585 <- left_join(tx90p_ssp585, spi6_ssp585, by = c("lon", "lat", "month", "year"))
clim_ssp585 <- left_join(clim_ssp585, spei_ssp585, by = c("lon", "lat", "month", "year"))
clim_ssp585 <- left_join(clim_ssp585, rx5day_ssp585, by = c("lon", "lat", "month", "year"))

clim_12 <- left_join(clim_ssp245, clim_ssp126, by = c("lon", "lat", "month", "year"))
clim_35 <- left_join(clim_ssp370, clim_ssp585, by = c("lon", "lat", "month", "year"))

clim_all <- left_join(clim_12, clim_35, by = c("lon", "lat", "month", "year"))

# Check: share of missing values per indicator (SPI6/SPEI6 may be missing in some cell-months)
clim_all %>%
  summarise(across(ends_with("_ssp245"), ~ mean(is.na(.x))))

write.csv(clim_all, "data/final/climate_all_scenarios.csv")

lost <- anti_join(distinct(tx90p_ssp245, lon, lat), cell_country, by = c("lon", "lat"))
plot(lost$lon, lost$lat, pch = ".", col = "grey40")

# ===============================================================
# Unweighted country-year aggregation
# (all grid cells, calendar year; no crop-area or growing-season weighting)
# ===============================================================
rm(list = setdiff(ls(), c("safe_max"))); gc()

library(dplyr)
library(data.table)

clim_all <- fread("data/final/climate_all_scenarios.csv") %>%
  as_tibble() %>%
  dplyr::select(-any_of(c("V1", "X")))   # drop the row-number column


# Helper: max that returns NA (not -Inf) when all values are missing
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

aggregated_data_unweighted <- clim_all %>%
  filter(!is.na(country_id)) %>%
  
  # Step 1: grid cell-year
  group_by(lon, lat, country_id, country_name, year) %>%
  summarize(
    # SSP126
    spi6_126   = mean(spi6_median_ssp126,  na.rm = TRUE),
    spei6_126  = mean(spei6_median_ssp126, na.rm = TRUE),
    tx90p_126  = mean(tx90p_median_ssp126, na.rm = TRUE),
    rx5day_126 = safe_max(rx5day_median_ssp126),
    # SSP245
    spi6_245   = mean(spi6_median_ssp245,  na.rm = TRUE),
    spei6_245  = mean(spei6_median_ssp245, na.rm = TRUE),
    tx90p_245  = mean(tx90p_median_ssp245, na.rm = TRUE),
    rx5day_245 = safe_max(rx5day_median_ssp245),
    # SSP370
    spi6_370   = mean(spi6_median_ssp370,  na.rm = TRUE),
    spei6_370  = mean(spei6_median_ssp370, na.rm = TRUE),
    tx90p_370  = mean(tx90p_median_ssp370, na.rm = TRUE),
    rx5day_370 = safe_max(rx5day_median_ssp370),
    # SSP585
    spi6_585   = mean(spi6_median_ssp585,  na.rm = TRUE),
    spei6_585  = mean(spei6_median_ssp585, na.rm = TRUE),
    tx90p_585  = mean(tx90p_median_ssp585, na.rm = TRUE),
    rx5day_585 = safe_max(rx5day_median_ssp585),
    .groups = "drop"
  ) %>%
  
  # Step 2: country-year (simple mean across grid cells)
  group_by(country_id, country_name, year) %>%
  summarize(
    across(c(starts_with("spi6_"), starts_with("spei6_"),
             starts_with("tx90p_"), starts_with("rx5day_")),
           ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

summary(aggregated_data_unweighted)

write.csv(aggregated_data_unweighted,
          'data/final/clim_unweighted_country_year_allssps.csv',
          row.names = FALSE)

haven::write_dta(aggregated_data_unweighted,
                 'data/final/clim_unweighted_country_year_allssps.dta')


# ===============================================================
# Merge with crop growing season data
# ===============================================================

crops_gs <- read.csv("data/crop/mapspam/processed/crops_gs.csv")

crops_gs %>% count(lon, lat) %>% filter(n > 1) %>% nrow()
clim_all <- clim_all %>%
  mutate(lon = round(as.numeric(lon), 2),
         lat = round(as.numeric(lat), 2))

data <- inner_join(clim_all, crops_gs, by = c("lon", "lat")) %>%
  mutate(
    crosses     = maturity_day < planting_day,
    month_start = as.Date(paste(year, month, "01", sep = "-")),
    month_end   = ceiling_date(month_start, "month") - days(1),
    
    # Season planted in this calendar year
    plant_cur = as.Date(planting_day - 1, origin = paste0(year, "-01-01")),
    mat_cur   = as.Date(maturity_day - 1,
                        origin = ifelse(crosses, paste0(year + 1, "-01-01"),
                                        paste0(year, "-01-01"))),
    overlap_cur = as.numeric(pmax(0, pmin(mat_cur, month_end) -
                                    pmax(plant_cur, month_start) + 1)),
    
    # Season planted in the previous calendar year (only for crossing seasons)
    plant_prev = as.Date(planting_day - 1, origin = paste0(year - 1, "-01-01")),
    mat_prev   = as.Date(maturity_day - 1, origin = paste0(year, "-01-01")),
    overlap_prev = ifelse(crosses,
                          as.numeric(pmax(0, pmin(mat_prev, month_end) -
                                            pmax(plant_prev, month_start) + 1)),
                          0)
  )

# Assign each month's overlap to the harvest year of its season
data <- bind_rows(
  data %>% mutate(days_in_overlap = overlap_cur,
                  year = ifelse(crosses, year + 1, year)),
  data %>% filter(crosses) %>%
    mutate(days_in_overlap = overlap_prev)          # harvest year = this year
) %>%
  filter(days_in_overlap > 0) %>%
  mutate(
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

# CHANGED: each indicator is now divided by the growing-season days for which THAT indicator
# has a value (days_with()), instead of by all growing-season days. If an indicator is missing
# in some months of a season (e.g. SPI6/SPEI6 in early 2015), the old version summed only the
# available months but divided by all days, pulling the average towards zero. When no values
# are missing, days_with() equals gs_days and results are identical to before.
days_with <- function(days, x) sum(days[!is.na(x)])

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
    
    tx90p_126_gs_share = ifelse(days_with(adjusted_gs_days, tx90p_median_ssp126) > 0, tx90p_126_gs_total / days_with(adjusted_gs_days, tx90p_median_ssp126), NA),
    tx90p_245_gs_share = ifelse(days_with(adjusted_gs_days, tx90p_median_ssp245) > 0, tx90p_245_gs_total / days_with(adjusted_gs_days, tx90p_median_ssp245), NA),
    tx90p_370_gs_share = ifelse(days_with(adjusted_gs_days, tx90p_median_ssp370) > 0, tx90p_370_gs_total / days_with(adjusted_gs_days, tx90p_median_ssp370), NA),
    tx90p_585_gs_share = ifelse(days_with(adjusted_gs_days, tx90p_median_ssp585) > 0, tx90p_585_gs_total / days_with(adjusted_gs_days, tx90p_median_ssp585), NA),
    
    spi6_126_gs = ifelse(days_with(adjusted_gs_days, spi6_median_ssp126) > 0, sum(weighted_spi6_126,  na.rm = TRUE) / days_with(adjusted_gs_days, spi6_median_ssp126), NA),
    spi6_245_gs = ifelse(days_with(adjusted_gs_days, spi6_median_ssp245) > 0, sum(weighted_spi6_245,  na.rm = TRUE) / days_with(adjusted_gs_days, spi6_median_ssp245), NA),
    spi6_370_gs = ifelse(days_with(adjusted_gs_days, spi6_median_ssp370) > 0, sum(weighted_spi6_370,  na.rm = TRUE) / days_with(adjusted_gs_days, spi6_median_ssp370), NA),
    spi6_585_gs = ifelse(days_with(adjusted_gs_days, spi6_median_ssp585) > 0, sum(weighted_spi6_585,  na.rm = TRUE) / days_with(adjusted_gs_days, spi6_median_ssp585), NA),
    
    spei6_126_gs = ifelse(days_with(adjusted_gs_days, spei6_median_ssp126) > 0, sum(weighted_spei6_126, na.rm = TRUE) / days_with(adjusted_gs_days, spei6_median_ssp126), NA),
    spei6_245_gs = ifelse(days_with(adjusted_gs_days, spei6_median_ssp245) > 0, sum(weighted_spei6_245, na.rm = TRUE) / days_with(adjusted_gs_days, spei6_median_ssp245), NA),
    spei6_370_gs = ifelse(days_with(adjusted_gs_days, spei6_median_ssp370) > 0, sum(weighted_spei6_370, na.rm = TRUE) / days_with(adjusted_gs_days, spei6_median_ssp370), NA),
    spei6_585_gs = ifelse(days_with(adjusted_gs_days, spei6_median_ssp585) > 0, sum(weighted_spei6_585, na.rm = TRUE) / days_with(adjusted_gs_days, spei6_median_ssp585), NA),
    
    rx5day_126_gs = ifelse(days_with(adjusted_gs_days, rx5day_median_ssp126) > 0, sum(weighted_rx5day_126, na.rm = TRUE) / days_with(adjusted_gs_days, rx5day_median_ssp126), NA),
    rx5day_245_gs = ifelse(days_with(adjusted_gs_days, rx5day_median_ssp245) > 0, sum(weighted_rx5day_245, na.rm = TRUE) / days_with(adjusted_gs_days, rx5day_median_ssp245), NA),
    rx5day_370_gs = ifelse(days_with(adjusted_gs_days, rx5day_median_ssp370) > 0, sum(weighted_rx5day_370, na.rm = TRUE) / days_with(adjusted_gs_days, rx5day_median_ssp370), NA),
    rx5day_585_gs = ifelse(days_with(adjusted_gs_days, rx5day_median_ssp585) > 0, sum(weighted_rx5day_585, na.rm = TRUE) / days_with(adjusted_gs_days, rx5day_median_ssp585), NA),
    
    .groups = "drop"
  )

yrs <- range(clim_all$year)
data_yearly <- data_yearly %>%
  filter(year > yrs[1], year <= yrs[2])

write_parquet(data, 'data/final/clim_all.parquet')

n_distinct(paste(clim_all$lon, clim_all$lat))   # all climate cells
n_distinct(paste(data$lon, data$lat))           # cells kept after join

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
