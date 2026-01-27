library(tidyverse)

#renv::install("vdeminstitute/vdemdata")
#renv::install("kvelleby/poldat", rebuild = T)
#df <- poldat::static_world
#saveRDS(df, "data_raw/static_world.rds")
df <- readRDS("data_raw/static_world.rds")

clim <- readr::read_csv("data_raw/agro_climate_data/clim_growingseason_country_year_allssps.csv") |>
  dplyr::select(country_id, country_name, year, total_harvarea, tx90p_245_gs, spi6_245_gs, rx5day_245_gs, spei6_245_gs) |>
  dplyr::rename(tx90pgs = tx90p_245_gs,
  							spi6gs = spi6_245_gs,
  							spei6gs = spei6_245_gs,
  							rx5daygs = rx5day_245_gs) |>
  dplyr::filter(year <= 2023)

ged_crop <- readr::read_csv("data/crop_ged_area_weighted.csv")

# Observed temperature only until 2014, use SSP2 the last 10 years.
temp <- readr::read_csv("data_raw/country_avg_temperature.csv") |>
  dplyr::filter(scenario %in% c("historical", "SSP2"), year <= 2024) |>
  dplyr::select(gwn_code, year, tas)

source("R/custom_gwcode_matches.R")
# Our World In Data
owd_energy_supply <- read_csv("data_raw/per-capita-kilocalorie-supply-from-all-foods-per-day.csv") |> janitor::clean_names() |>
  filter(!grepl("\\(FAO", country)) |>
  mutate(gwcode = countrycode::countrycode(country, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)) |>
  na.omit() |>
  dplyr::select(gwcode, year, energy_supply_owd = food_supply_kcal_per_capita_per_day) |>
  group_by(gwcode, year) |>
  summarize(energy_supply_owd = mean(energy_supply_owd, na.rm = T)) |>
  tsibble::tsibble(key = "gwcode", index = "year")

df <- dplyr::left_join(df, owd_energy_supply, by = c("gwcode", "year"))
df <- dplyr::left_join(df, clim, by = c("gwcode" = "country_id", "year"))
df <- dplyr::left_join(df, temp, by = c("gwcode" = "gwn_code", "year"))

food_production <- read_csv("data_raw/index-of-cereal-production-yield-and-land-use/index-of-cereal-production-yield-and-land-use.csv") |> janitor::clean_names() |>
  rename(
    cereal_area = cereals_00001717_area_harvested_005312_hectares,
    cereal_production = cereals_00001717_production_005510_tonnes,
    cereal_yield = cereals_00001717_yield_005419_tonnes_per_hectare
  ) |>
  mutate(gwcode = countrycode::countrycode(entity, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)) |>
  na.omit() |>
  dplyr::filter(gwcode == 220, year == 1961) |>
  group_by(gwcode, year) |>
  summarize(cereal_yield = weighted.mean(cereal_yield, cereal_production),
            cereal_area = sum(cereal_area),
            cereal_production = sum(cereal_production)) |>
  dplyr::select(gwcode, year, cereal_area, cereal_production, cereal_yield) |>
  tsibble::tsibble(key = "gwcode", index = "year")

df <- dplyr::left_join(df, food_production, by = c("gwcode", "year"))
df <- dplyr::left_join(df, ged_crop, by = c("gwcode", "year"))


df <- df |> dplyr::mutate(calorie_var_adj = dplyr::if_else(calorie_var < 0.2, 0.2, dplyr::if_else(calorie_var > 0.35, 0.35, calorie_var))) # de Haaen et al 2011 p.6
pou <- poldat::estimate_undernourishment(df$energy_supply, df$calorie_var, df$min_energy_req)
pou_adj <- poldat::estimate_undernourishment(df$energy_supply, df$calorie_var_adj, df$min_energy_req)
df$pou <- pou$prevalence_of_undernourishment
df$pou_adj <- pou_adj$prevalence_of_undernourishment

# See R/foodbalancesheets.R
fbs <- readr::read_csv("data/food_balance_gwcode.csv")
df <- dplyr::left_join(df, fbs, by = c("gwcode", "year"))

# See R/historical_mder.R
mder <- readr::read_csv("data/mder_historical.csv") |> dplyr::select(-population) |>
  group_by(gwcode, year) |>
  summarize(mder = mean(mder))
df <- dplyr::left_join(df, mder, by = c("gwcode", "year"))

readr::write_csv(df, "data/main.csv")
