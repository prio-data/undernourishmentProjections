library(tidyverse)
library(tidyr)
library(tsibble)

# Simulation data ------------------------------------------------------------------------------
ssp <- read_csv("data/ssp_interpolated.csv") |>
	dplyr::select(scenario, gwcode, year, gdppc, secprop, mder, population = pop, tx90pgs, tas) |>
	mutate(v2x_polyarchy = NA_real_,
				 best = NA_real_,
				 des = NA_real_,
				 cv = NA_real_,
				 crop_country_share = NA_real_)

for(s in paste0("SSP", 1:5)){
	hist <- main_df |>
		as_tibble() |>
		dplyr::select(any_of(names(ssp))) |>
		dplyr::filter(year >= (1999)) |>
		mutate(scenario = s)
	ssp <- ssp |> bind_rows(hist)
}

clim <- read_csv("data_raw/agro_climate_data/clim_growingseason_country_year_allssps.csv") |>
	tidyr::pivot_longer(cols = starts_with("tx90p"), names_to = "scenario", values_to = "tx90pgs") |>
	dplyr::mutate(scenario =
									case_when(scenario == "tx90p_126_gs" ~ "SSP1",
														scenario == "tx90p_245_gs" ~ "SSP2",
														scenario == "tx90p_370_gs" ~ "SSP3",
														scenario == "tx90p_585_gs" ~ "SSP5"))
clim_ssp4 <- clim |> dplyr::filter(scenario == "SSP3") |>
	dplyr::mutate(scenario = "SSP4")
clim <- dplyr::bind_rows(clim, clim_ssp4) |>
	dplyr::rename(gwcode = country_id) |>
	dplyr::select(scenario, gwcode, year, tx90pgs)

temp <- readr::read_csv("data_raw/country_avg_temperature.csv") |>
	dplyr::select(scenario, gwn_code, year, tas)
temp_historical <- temp |> dplyr::filter(scenario == "historical")
for(s in paste0("SSP", c(1,2,3,5))){
	temp_historical$scenario <- s
	temp <- bind_rows(temp, temp_historical)
}
temp <- temp |> dplyr::filter(scenario != "historical")
temp_ssp4 <- temp |> dplyr::filter(scenario == "SSP3")
temp_ssp4$scenario <- "SSP4"
temp <- bind_rows(temp, temp_ssp4)


harmonized_ssp <- ssp |>
	tsibble(key = c("scenario", "gwcode"), index = "year") |>
	fill_gaps() |>
	group_by(scenario, gwcode) |>
	arrange(year) |>
	dplyr::select(gwcode, year, scenario, gdppc, population, secprop, mder, v2x_polyarchy, best, des, cv, crop_country_share) |>
	mutate(secprop = if_else(secprop > 0.98, 0.98, secprop)) |>
	mutate(secprop = if_else(secprop < 0.02, 0.02, secprop)) |>
	mutate(v2x_polyarchy = if_else(v2x_polyarchy < 0.02, 0.02, v2x_polyarchy)) |>
	mutate(
		lsecprop = qlogis(secprop) + 100 # to get everything linear and positive for growth calculation
	) |>
	mutate(
		gddpc_d = difference(gdppc)/dplyr::lag(gdppc),
		population_d = difference(population)/dplyr::lag(population),
		lsecprop_d = difference(lsecprop)/dplyr::lag(lsecprop)
	) |>
	fill(gddpc_d, population_d, lsecprop_d, .direction = "up") |>
	mutate(gdppc_mapped = purrr::accumulate(tail(gddpc_d,-1), ~ .x * (1+.y), .init = first(gdppc))) |>
	mutate(population_mapped = purrr::accumulate(tail(population_d,-1), ~ .x * (1+.y), .init = first(population))) |>
	mutate(lsecprop_mapped = purrr::accumulate(tail(lsecprop_d,-1), ~ .x * (1+.y), .init = first(lsecprop))) |>
	mutate(secprop_mapped = plogis(lsecprop_mapped - 100)) |>
	dplyr::select(gwcode, year, scenario, gdppc = gdppc_mapped, population = population_mapped, secprop = secprop_mapped,
								mder, v2x_polyarchy, best, des, cv, crop_country_share) |>
	fill(v2x_polyarchy, mder, best, des, cv, crop_country_share, .direction = "down") |>
	dplyr::left_join(clim, by = c("scenario", "gwcode", "year")) |>
	dplyr::left_join(temp, by = c("scenario", "gwcode" = "gwn_code", "year"))

write_csv(harmonized_ssp, "data/harmonized_ssp.csv")
