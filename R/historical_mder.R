library(tidyverse)
library(poldat)

mder <- read_csv("data_raw/minimum-requirement-calories/minimum-requirement-calories.csv") |> janitor::clean_names() |>
  rename(mder = minimum_dietary_energy_requirement_kcal_cap_day_00021056_value_006128_kilocalories_per_capita_per_day)

source("R/custom_gwcode_matches.R")
mder <- mder |> filter(!grepl("\\(FAO\\)", entity))
mder$gwcode <- countrycode::countrycode(mder$entity, origin = "country.name",  destination = "gwn", custom_match = custom_gwcode_matches)
mder <- na.omit(mder) |> select(gwcode, year, mder)

mder <- left_join(mder, static_world |> select(gwcode, year, population), by = c("gwcode", "year")) |>
  group_by(gwcode, year) |>
  mutate(mder = weighted.mean(mder, population))

write_csv(mder, "data/mder_historical.csv")

ssp <- read_csv("data/ssp_interpolated.csv") 
ssp_mder <- ssp |> filter(year == 2025, scenario == "SSP2") |> rename(ssp_mder = mder) |> arrange(gwcode) |> select(gwcode, ssp_mder)

comparison <- mder |> filter(year == max(year)) |> left_join(ssp_mder, by = "gwcode") |>
  mutate(diff = ssp_mder - mder) |>
  arrange(diff)


quantile_mapping <- function(projection, gold_standard, new_projections) {
  # Create quantiles for both distributions
  proj_quantiles <- quantile(projection, probs = seq(0, 1, 0.01), na.rm = T)
  gold_quantiles <- quantile(gold_standard, probs = seq(0, 1, 0.01), na.rm = T)
  
  # Build mapping function
  mapping_function <- approxfun(proj_quantiles, gold_quantiles, rule = 2)
  
  # Apply to new projections
  calibrated_values <- mapping_function(new_projections)
  
  return(calibrated_values)
}

ssp$mder_cal_quantile <- quantile_mapping(comparison$ssp_mder, comparison$mder, ssp$mder)


library(stats)

# loess calibration
loess_model <- loess(comparison$mder ~ comparison$ssp_mder, 
                     span = 0.75,  # Controls smoothing (0.5-0.8 typical)
                     degree = 1)   # Polynomial degree (1=linear, 2=quadratic)

ssp$ssp_mder <- ssp$mder
ssp$mder_cal_loess <- predict(loess_model, newdata = ssp$mder)

# linear calibration
calibration_model <- lm(mder ~ ssp_mder, data = comparison)
ssp$mder_cal <- predict(calibration_model, newdata = ssp)
                
#ssp <- ssp |> mutate(mder_cal = if_else(mder_cal > 2050, 2050, mder_cal))

future <- ssp |> filter(scenario == "SSP1") |> select(gwcode, year, mder = mder_cal_quantile)
past <- mder |> select(gwcode, year, mder)
all <- bind_rows(past, future)

ggplot(all, aes(x = year, y = mder, group = gwcode)) + geom_line()

#quantile mapping seems to be a good approach.
ssp |> mutate(quantile_diff = mder - mder_cal_quantile) |> View()


ggplot(ssp, aes(x = year, y = (mder - mder_cal), group = gwcode)) + geom_line() + facet_wrap(~scenario)

comparison <- comparison |>
  mutate(cal_diff = ssp_mder - ssp_mder_cal)
View(comparison)

ggplot(comparison, aes(x = mder, y = ssp_mder)) + geom_point() + coord_fixed(ratio = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")
ggplot(comparison, aes(x = mder, y = ssp_mder_cal)) + geom_point() + coord_fixed(ratio = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")