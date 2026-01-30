library(poldat)
library(tidyverse)

mder_estimates <- read_csv("data/mder_estimates.csv")

ssp <- readxl::read_xlsx("data_raw/1721734326790-ssp_basic_drivers_release_3.1_full.xlsx", sheet = "data")
ssp <- janitor::clean_names(ssp)
ssp <- ssp |> pivot_longer(cols = starts_with("x"), names_to = "year") |>
  mutate(year = stringr::str_remove(year, "x") |> as.numeric())
var_names <- unique(ssp$variable)
new_var_names <- janitor::make_clean_names(var_names)
var_names_match <- tibble(var_names, new_var_names)
ssp <- left_join(ssp, var_names_match, by = c("variable" = "var_names"))
ssp <- ssp |> filter(!grepl("\\(R|World", region)) # only keep countries

population <- ssp |> filter(model == "IIASA-WiC POP 2023")
pop_by_sex_age <- population |>
  filter(grepl("Population\\|Female\\||Population\\|Male\\|", variable)) |>
  filter(!grepl("Education", variable)) |>
  mutate(sex = stringr::str_split_i(variable, "\\|", 2),
         age = stringr::str_split_i(variable,"\\|", 3) |> stringr::str_remove("Age "))

ssp_mder <- pop_by_sex_age |> filter(scenario != "Historical Reference", year >= 2025) |>
  mutate(gwcode = countrycode::countrycode(region, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)) |>
  group_by(scenario, gwcode, year, sex, age) |>
  summarize(value = sum(value)) |>
  group_by(scenario, gwcode, year) |>
  mutate(tot_pop = sum(value)) |> # across cohorts
  mutate(share_pop = value / tot_pop,
         sex = tolower(sex)) |>
  left_join(mder_estimates, by = c("age" = "age_group", "sex")) |> #filter(scenario == "SSP1", region == "Croatia", year == 2025) |> View()
  summarize(mder = sum(share_pop * mer)) # does not include the pregnancy allowance (210*birth_rate)
write_csv(ssp_mder, file = "data/ssp_mder_projections.csv")


gwcodes <- countrycode::codelist_panel |> dplyr::select(year, iso3c, gwn)
source("R/custom_gwcode_matches.R")

clim <- read_csv("data_raw/agro_climate_data/clim_growingseason_country_year_allssps.csv") |>
  dplyr::filter(year >= 2025) |>
  tidyr::pivot_longer(cols = starts_with(c("tx90p", "rx5day", "spi6", "spei6")), names_to = "scenario_var", values_to = "value") |>
	dplyr::mutate(
		variable_name = stringr::str_extract(scenario_var, "^[^_]+"),
		scenario = case_when(
			stringr::str_detect(scenario_var, "_126_") ~ "SSP1",
			stringr::str_detect(scenario_var, "_245_") ~ "SSP2",
			stringr::str_detect(scenario_var, "_370_") ~ "SSP3",
			stringr::str_detect(scenario_var, "_585_") ~ "SSP5"
		)
	)
ssp4 <- clim |> dplyr::filter(scenario == "SSP3") |>
  dplyr::mutate(scenario = "SSP4")
clim <- dplyr::bind_rows(clim, ssp4) |>
  dplyr::rename(gwcode = country_id) |>
	dplyr::select(-scenario_var) %>%
	tidyr::pivot_wider(
		names_from = variable_name,
		values_from = value
	) |>
  dplyr::select(scenario, gwcode, year, tx90pgs = tx90p, rx5daygs = rx5day, spi6gs = spi6, spei6gs = spei6)

temp <- readr::read_csv("data_raw/country_avg_temperature.csv") |>
  dplyr::filter(year >= 2024) |>
  dplyr::select(scenario, gwn_code, year, tas) |>
  na.omit()


#ggplot(clim |> dplyr::filter(gwcode == 2), aes(x = year, y = tx90pgs, group = scenario, color = scenario)) + geom_line()

gdp <- ssp |>
  dplyr::filter(model %in% c("IIASA GDP 2023", "OECD ENV-Growth 2023"),
                grepl("SSP", scenario),
                year >= 2025) |>
  filter(!grepl("per capita", variable)) |>
  dplyr::select(model, scenario, region, year, "gdp" = value) |> na.omit() |>
  mutate(gwcode = countrycode::countrycode(region, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)) |>
  group_by(model, scenario, gwcode, year) |>
  summarize(gdp = sum(gdp)) |>
  mutate(model2 = if_else(model == "IIASA GDP 2023", "gdp_iiasa", "gdp_oecd")) |>
  pivot_wider(id_cols = all_of(c("scenario", "gwcode", "year")), names_from = "model2", values_from = "gdp")

edu_by_sex_age <- population |>
  filter(grepl("Population\\|Female\\||Population\\|Male\\|", variable)) |>
  filter(grepl("Education", variable)) |>
  filter(year >= 2025) |>
  mutate(sex = stringr::str_split_i(variable, "\\|", 2),
         age = stringr::str_split_i(variable,"\\|", 3) |> stringr::str_remove("Age "),
         education = stringr::str_split_i(variable,"\\|", 4))

edu <- edu_by_sex_age |> dplyr::select(c("model", "scenario", "region", "unit", "year", "sex", "age", "education", "value")) |>
  pivot_wider(id_cols = all_of(c("model", "scenario", "region", "unit", "year", "sex", "age")), names_from = "education", values_from = "value") |>
  janitor::clean_names()

edu <- edu |>
  mutate(gwcode = countrycode::countrycode(region, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)) |>
  group_by(model, scenario, gwcode, year) |>
  summarize(across(all_of(c("no_education", "incomplete_primary_education", "lower_secondary_education", "post_secondary_education", "primary_education",
                            "upper_secondary_education")),sum)) |>
  mutate(tot = no_education + incomplete_primary_education + lower_secondary_education + post_secondary_education + primary_education + upper_secondary_education) |>
  mutate(priprop = (lower_secondary_education + post_secondary_education + primary_education + upper_secondary_education)/tot,
         secprop = (post_secondary_education + upper_secondary_education)/tot,
         psecprop = post_secondary_education/tot) |>
  dplyr::select(model, scenario, gwcode, year, priprop, secprop, psecprop)

pop <- pop_by_sex_age |>
  filter(grepl("SSP", scenario), year >= 2025) |>
  mutate(gwcode = countrycode::countrycode(region, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)) |>
  group_by(model, scenario, gwcode, year) |>
  summarize(pop = sum(value))

all <- left_join(pop |> ungroup() |> dplyr::select(-model), gdp |> ungroup(), by = c("scenario", "gwcode", "year")) |>
  left_join(edu |> ungroup() |> dplyr::select(-model), by = c("scenario", "gwcode", "year")) |>
  left_join(ssp_mder |> ungroup(), by = c("scenario", "gwcode", "year")) |>
  mutate(gdppc_iiasa = gdp_iiasa / (pop/10^3),
         gdppc_oecd = gdp_oecd / (pop/10^3)) |>
  mutate(gdppc = if_else(is.na(gdppc_oecd), gdppc_iiasa, gdppc_oecd)) |>
  mutate(gdp = if_else(is.na(gdp_oecd), gdp_iiasa, gdp_oecd)) |>
  dplyr::select(-gdppc_iiasa, -gdppc_oecd, -gdp_iiasa, -gdp_oecd)

all[!complete.cases(all),] |> pull(gwcode) |> unique()

library(tsibble)

ssp_interpolated <- all[complete.cases(all),] |>
  tsibble(key = c("scenario", "gwcode"), index = "year") |>
  group_by_key() |>
  complete(year = seq(min(year), max(year))) |>
  mutate(pop = zoo::na.approx(pop),
         gdp = zoo::na.approx(gdp),
         priprop = zoo::na.approx(priprop),
         secprop = zoo::na.approx(secprop),
         psecprop = zoo::na.approx(psecprop),
         mder = zoo::na.approx(mder),
         gdppc = zoo::na.approx(gdppc)
         )

#### Calibration of mder

ssp_template <- ssp_interpolated |> dplyr::select(scenario, gwcode, year) |> filter(year == 2025)

mder <- read_csv("data/mder_historical.csv") |> dplyr::select(gwcode, year, mder_hist = mder) |>
  group_by(gwcode, year) |>
  summarize(mder_hist = mean(mder_hist)) |> # gwcode 200 were double...
  filter(year == max(year)) |>
  mutate(year = 2025) |> # assume number is for 2025
  right_join(ssp_template, by = c("gwcode", "year")) |>
  tsibble(key = c("scenario", "gwcode"), index = "year")


ssp_mder <- ssp_interpolated |> dplyr::select(scenario, gwcode, year, mder) |>
  tsibble(key = c("scenario", "gwcode"), index = "year") |>
  mutate(mder_growth = difference(log(mder))) |>
  left_join(mder) |>
  fill_gaps() |>
  as_tibble() |>
  group_by(scenario, gwcode) |>
  arrange(year) |>
  mutate(mder_mapped = purrr::accumulate(tail(mder_growth,-1), ~ .x * (1+.y), .init = first(mder_hist))) |>
  dplyr::select(scenario, gwcode, year, mder = mder_mapped)

ssp_interpolated <- ssp_interpolated |> dplyr::select(-mder) |>
  left_join(ssp_mder, by = c("scenario", "gwcode", "year")) |>
  left_join(clim, by = c("scenario", "gwcode", "year")) |>
  left_join(temp, by = c("scenario", "gwcode" = "gwn_code", "year"))

write_csv(ssp_interpolated, "data/ssp_interpolated.csv")
