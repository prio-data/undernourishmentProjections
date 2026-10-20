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

setwd("data/crop/growingseason/")


# ===============================================================
# 1. Read the GGCMI Phase 3 crop calendars and regrid to 1 degree
# ===============================================================

nc_files <- list.files(pattern = "\\.nc4", full.names = TRUE)
nc_files <- gsub(".*/|", "", nc_files)

# Apply the function to all nc_files and combine the results
all_crops_df <- purrr::map_df(nc_files, process_gs)

write_parquet(all_crops_df, file.path("processed/all_crops_growing_season_extended.parquet"))

all_crops_df <- read_parquet("processed/all_crops_growing_season_extended.parquet")
all_crops_df <- all_crops_df %>% dplyr::select(x, y, crop, planting_day, maturity_day)

names(all_crops_df) <- c('lon', 'lat', 'crop', 'planting_day', 'maturity_day')

duplicates <- all_crops_df %>%
  group_by(lon, lat, crop) %>%
  filter(n() > 1)


# ===============================================================
# 2. Harmonize crop names with MapSPAM
# ===============================================================

# Combine irrigated and rainfed variants, and the two rice seasons
all_crops_df <- all_crops_df %>%
  mutate(crop = gsub("_ir|_rf|1|2", "", crop))

# Mapping of GGCMI crop names to MapSPAM names
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
  "ri"  = "rice",
  "rye" = "ocer",
  "sgb" = "sugb",
  "sgc" = "sugc",
  "sor" = "sorg",
  "soy" = "soyb",
  "sun" = "sunf",
  "swh" = "whea",   # spring and winter wheat are both wheat in MapSPAM
  "wwh" = "whea"
)

crops_calendar <- all_crops_df %>%
  mutate(crop = ifelse(crop %in% names(rename_map), rename_map[match(crop, names(rename_map))], crop))

# The GGCMI millet calendar applies to both MapSPAM millet codes (mill and pmil, pearl millet)
crops_calendar <- bind_rows(crops_calendar,
                            crops_calendar %>% filter(crop == "mill") %>% mutate(crop = "pmil"))


# ===============================================================
# 3. Combine variants into one growing season per cell and crop
# ===============================================================
# Each MapSPAM crop can have several GGCMI variants per cell (irrigated/rainfed,
# rice season 1/2, spring/winter wheat). The growing season is defined as the
# period during which ANY variant is in the ground (the union of the variants).
# Day-of-year values wrap around at 31 December, so the union is computed on a
# circular calendar.

# Drop variants with missing dates; flag seasons crossing the new year
crops_calendar <- crops_calendar %>%
  dplyr::select(lon, lat, crop, planting_day, maturity_day) %>%
  filter(!is.na(planting_day), !is.na(maturity_day)) %>%
  mutate(crosses = maturity_day < planting_day)

# Diagnostic: cell-crops whose variants mix crossing and non-crossing seasons
mix_check <- crops_calendar %>%
  group_by(lon, lat, crop) %>%
  summarise(mixed = n_distinct(crosses) > 1, .groups = "drop")
mix_check %>% count(crop, mixed) %>% tidyr::pivot_wider(names_from = mixed, values_from = n)

# Shortest season (circular arc) covering all variants.
# Returns c(planting_day, maturity_day); the whole year (1, 365) if the
# variants together leave no gap in the year.
cover_arc <- function(p, m) {
  best_L <- Inf; best_s <- NA
  for (s in p) {
    fits <- all((p - s) %% 365 <= (m - s) %% 365)   # every variant lies within the arc starting at s
    if (!fits) next
    L <- max((m - s) %% 365)
    if (L < best_L) { best_L <- L; best_s <- s }
  }
  if (is.na(best_s)) return(c(1, 365))              # variants together cover the whole year
  c(best_s, (best_s - 1 + best_L) %% 365 + 1)
}

# Union of the variants:
# - variants all within one calendar year, or all crossing the new year:
#   earliest planting to latest maturity
# - variants mixing crossing and non-crossing seasons: circular union (cover_arc)
crops_calendar <- crops_calendar %>%
  group_by(lon, lat, crop) %>%
  summarize(
    mixed = n_distinct(crosses) > 1,
    arc   = if (mixed) list(cover_arc(planting_day, maturity_day)) else list(NULL),
    planting_day = if (mixed) arc[[1]][1] else min(planting_day),
    maturity_day = if (mixed) arc[[1]][2] else max(maturity_day),
    .groups = 'drop'
  ) %>%
  dplyr::select(-mixed, -arc)

duplicates <- crops_calendar %>%
  group_by(lon, lat, crop) %>%
  filter(n() > 1)


# ===============================================================
# 4. Save
# ===============================================================

write_parquet(crops_calendar, file.path("processed/all_crops_growing_season.parquet"))
write.csv(crops_calendar, file.path("processed/all_crops_growing_season.csv"), row.names = FALSE)


# ===============================================================
# 5. Checks
# ===============================================================

sum(is.na(crops_calendar$planting_day))                           # should be 0
nrow(duplicates)                                                  # should be 0
sort(unique(crops_calendar$crop))                                 # should all be MapSPAM codes

# Share of cells with a season crossing the new year, and year-round seasons, by crop
crops_calendar %>%
  group_by(crop) %>%
  summarise(share_crossing = mean(maturity_day < planting_day),
            year_round     = sum(planting_day == 1 & maturity_day == 365),
            .groups = "drop")
