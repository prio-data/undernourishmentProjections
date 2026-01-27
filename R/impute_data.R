library(tidyverse)
library(sbgcop) # https://journals.sagepub.com/doi/10.1177/0049124118799381
library(tsibble)

df <- readr::read_csv("data/main.csv")

variables <- c("best", "gdppc_grwt", "gdppc", "population", "wdi_imr", "wdi_gini",
							 "v2x_polyarchy", "v2x_egal", "v2xeg_eqdr", "v2pepwrgen", "v2x_libdem",
							 "psecprop", "priprop", "secprop", "energy_supply", "energy_supply_owd", "calorie_var", "mder",
							 "import_share", "world_export_share",
							 "tx90pgs", "tas", "spei6gs", "spi6gs", "rx5daygs",
							 "world_production", "pou",
							 "crop_country_sum", "crop_country_share",
							 "crop_ged_distance_m", "crop_ged_distance_s", "crop_ged_brd_m", "crop_ged_brd_s",
							 "crop_ged_incidence_m", "crop_ged_incidence_s",
							 "cereal_area", "cereal_production", "cereal_yield")

assertthat::are_equal(nrow(tsibble::duplicates(df, key = "gwcode", index = "year")), 0)

df <- df |>
	dplyr::select(dplyr::all_of(c("gwcode", "year", variables))) |>
	dplyr::group_by(gwcode, year) |>
	dplyr::summarize(dplyr::across(.cols = dplyr::all_of(variables), .fns = mean)) |>
	dplyr::mutate(energy_supply_owd = dplyr::if_else(is.na(energy_supply_owd), energy_supply, energy_supply_owd)) |>
	dplyr::filter(year >= 1960) |>
	dplyr::mutate(lgdppc = log(gdppc))

# View(df |> select(gwcode, year, energy_supply, energy_supply_owd))

# Multiple imputation using Gaussian Copulas, see Hollenbach et al. (2021) for example

#### Impute energy supply
impute_vars <- c("energy_supply_owd", "wdi_imr", "import_share", "world_export_share", "lgdppc")
imputation_matrix <- df |> dplyr::ungroup() |>
	dplyr::select(dplyr::all_of(c("year", impute_vars))) |> as.matrix()
fit <- sbgcop.mcmc(imputation_matrix)
summary(fit)
plot(fit)
imp_es <- fit$Y.pmean |> dplyr::as_tibble() |>
	dplyr::mutate(gwcode = df$gwcode) |>
	dplyr::rename_with(~paste0("imp_", .x), dplyr::all_of(impute_vars)) |>
	dplyr::mutate(year = round(year, 0) |> as.integer(),
								gwcode = round(gwcode, 0) |> as.integer()) |>
	dplyr::select("gwcode", "year", "imp_energy_supply_owd")

res <- dplyr::left_join(df, imp_es, by = c("year", "gwcode")) |>
	dplyr::mutate(dplyr::across(dplyr::starts_with("imp_"), ~dplyr::if_else(is.na(.), get(str_replace(cur_column(), "imp_", "")), .)))

res <- tsibble(res, key = "gwcode", index = "year") |>
	mutate(imp_energy_supply_owd = zoo::na.approx(imp_energy_supply_owd))

res <- res |> filter(year <= 2023) # not enough data after 2023

grow_back <- function(x, growth) exp(log(dplyr::lead(x)) - dplyr::lead(growth))

res <- res |>
	group_by(gwcode) |>
	arrange(year) |>
	dplyr::mutate(des_diff = difference(imp_energy_supply_owd)/dplyr::lag(imp_energy_supply_owd))|>
	dplyr::mutate(des_diff = lag(if_else(!is.na(energy_supply_owd), NA, des_diff))) |>
	dplyr::mutate(des = energy_supply_owd)

for(i in 1:100){
	res <- res  |>
		dplyr::mutate(des = dplyr::if_else(is.na(des), grow_back(des, des_diff), des))
}


res <- res |>
	tsibble(key = "gwcode", index = "year") |>
	fill_gaps(.full = TRUE)
res$complete <- complete.cases(res)

to_fill <- res |> dplyr::filter(year >= 2018)
filled <- to_fill |> group_by(gwcode) |> arrange(year) |> fill(everything()) # Imputes a few observations for a few countries

main_df <- dplyr::bind_rows(
	res |> dplyr::filter(year < 2018),
	filled
)

write_csv(main_df, "data/imputed_main.csv")
