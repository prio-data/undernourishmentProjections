library(tidyverse)


df_historic <- read_csv("data_raw/FoodBalanceSheetsHistoric_E_All_Data/FoodBalanceSheetsHistoric_E_All_Data.csv") |> janitor::clean_names()
df <- read_csv("data_raw/FoodBalanceSheets_E_All_Data/FoodBalanceSheets_E_All_Data.csv") |> janitor::clean_names()
names(df)
df$item |> unique()
df$element |> unique()
df_historic$element |> unique()

df |> filter(element == "Production") |> pull(unit) |> unique()
df |> filter(element == "Food supply (kcal/capita/day)") |> pull(unit) |> unique()
df |> filter(element == "Import quantity") |> pull(unit) |> unique()
df |> filter(element == "Export quantity") |> pull(unit) |> unique()
df |> filter(element == "Losses") |> pull(unit) |> unique()
df |> filter(element == "Feed") |> pull(unit) |> unique()
df |> filter(element == "Seed") |> pull(unit) |> unique()
df |> filter(element == "Food") |> pull(unit) |> unique()

regions <- df |> dplyr::filter(item == "Prevalence of undernourishment (percent) (annual value)") |> pull(area_code) |> unique()
years <- paste0("y", 2010:2022)
keyvars <- c("area_code", "area_code_m49", "area", "item_code", "item", "element_code", "element", "unit")

cdf <- df |> dplyr::select(all_of(c(keyvars, years))) |>
  dplyr::filter(!area_code %in% regions) |>
  dplyr::mutate(across(.cols = all_of(years), .fns = ~as.character(.))) |>
  tidyr::pivot_longer(cols = all_of(years), names_to = "year", values_to = "value") |>
  dplyr::mutate(value = stringr::str_replace_all(value, "<", "") |> as.numeric(value)) |> na.omit() |>
  dplyr::mutate(year = stringr::str_replace_all(year, "y", "") |> as.numeric(year))

pie <- cdf |> filter(element %in% c("Total Population - Both sexes", "Production", "Food", "Import quantity", "Export quantity", "Domestic supply quantity", "Food supply (kcal/capita/day)")) |>
  select(-unit, -element_code) |>
  tidyr::pivot_wider(id_cols = any_of(c(keyvars, "year")), names_from = "element", values_from = "value") |>
  janitor::clean_names() |>
  mutate(production = production * food_supply_kcal_capita_day,
         import = import_quantity * food_supply_kcal_capita_day,
         export = export_quantity * food_supply_kcal_capita_day,
         supply = domestic_supply_quantity * food_supply_kcal_capita_day,
         food = food * food_supply_kcal_capita_day) |>
  group_by(area_code, area, year) |>
  summarize(production = sum(production, na.rm = T),
            import = sum(import, na.rm = T),
            export = sum(export, na.rm = T),
            supply = sum(supply, na.rm = T),
            food = sum(food, na.rm = T),
            pop = sum(total_population_both_sexes, na.rm = T)) |>
  mutate(domestic_calories = production + import + supply - export) |>
  mutate(domestic_share = (production+supply-export) / domestic_calories,
         import_share = import / domestic_calories,
         export_share = export / domestic_calories,
         food_share = food / domestic_calories)

regions <- df_historic |> dplyr::filter(item == "Prevalence of undernourishment (percent) (annual value)") |> pull(area_code) |> unique()
years <- paste0("y", 1961:2013)
keyvars <- c("area_code", "area_code_m49", "area", "item_code", "item", "element_code", "element", "unit")

cdf_historic <- df_historic |> dplyr::select(all_of(c(keyvars, years))) |>
  dplyr::filter(!area_code %in% regions) |>
  dplyr::mutate(across(.cols = all_of(years), .fns = ~as.character(.))) |>
  tidyr::pivot_longer(cols = all_of(years), names_to = "year", values_to = "value") |>
  dplyr::mutate(value = stringr::str_replace_all(value, "<", "") |> as.numeric(value)) |> na.omit() |>
  dplyr::mutate(year = stringr::str_replace_all(year, "y", "") |> as.numeric(year))

pie_historic <- cdf_historic |> filter(element %in% c("Total Population - Both sexes", "Production", "Food", "Import Quantity", "Export Quantity", "Domestic supply quantity", "Food supply (kcal/capita/day)")) |>
  select(-unit, -element_code) |>
  tidyr::pivot_wider(id_cols = any_of(c(keyvars, "year")), names_from = "element", values_from = "value") |>
  janitor::clean_names() |>
  mutate(production = production * food_supply_kcal_capita_day,
         import = import_quantity * food_supply_kcal_capita_day,
         export = export_quantity * food_supply_kcal_capita_day,
         supply = domestic_supply_quantity * food_supply_kcal_capita_day,
         food = food * food_supply_kcal_capita_day) |>
  group_by(area_code, area, year) |>
  summarize(production = sum(production, na.rm = T),
            import = sum(import, na.rm = T),
            export = sum(export, na.rm = T),
            supply = sum(supply, na.rm = T),
            food = sum(food, na.rm = T),
            pop = sum(total_population_both_sexes, na.rm = T)) |>
  mutate(domestic_calories = production + import + supply - export) |>
  mutate(domestic_share = (production+supply-export) / domestic_calories,
         import_share = import / domestic_calories,
         export_share = export / domestic_calories,
         food_share = food / domestic_calories)


pie_historic |> filter(area == "World", year %in% c(2010:2013))
pie |> filter(area == "World", year %in% c(2010:2013))

pie_all <- bind_rows(pie_historic |> filter(year<2010), pie) |> arrange(area_code, year)

ccodes <- readr::read_csv("data_raw/FAOSTAT_data_2-3-2025.csv") |> janitor::clean_names()

pie_all <- pie_all |>
  dplyr::left_join(ccodes, by = c("area_code" = "country_code"))

gwcodes <- countrycode::codelist_panel |> dplyr::select(year, iso3c, gwn)
pie_all <- pie_all |> filter(!is.na(iso3_code)) |> dplyr::left_join(gwcodes, by = c("year", "iso3_code" = "iso3c"))

source("R/custom_gwcode_matches.R")
pie_all$country_name <- countrycode::countrycode(pie_all$iso3_code, origin = "iso3c", destination = "country.name")
pie_all$gwcode <- countrycode::countrycode(pie_all$country_name, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)    
pie_all <- pie_all |> filter(!is.na(gwcode))

# Consolidate countries with same gwcode
pie_all <- pie_all |>
  group_by(gwcode, year) |>
  summarize(production = sum(production, na.rm = T),
            import = sum(import, na.rm = T),
            export = sum(export, na.rm = T),
            supply = sum(supply, na.rm = T),
            food = sum(food, na.rm = T),
            pop = sum(pop, na.rm = T)) |>
  mutate(production = if_else(production <0, 0, production),
         import = if_else(import <0, 0, import),
         export = if_else(export <0, 0, export),
         supply = if_else(supply <0, 0, supply),
         food = if_else(food <0, 0, food)) |>
  mutate(domestic_calories = production + import + supply - export) |>
  mutate(domestic_share = (production+supply-export) / domestic_calories,
         import_share = import / domestic_calories,
         export_share = export / domestic_calories,
         food_share = food / domestic_calories
         )

pie_all <- pie_all |>
  group_by(year) |>
  mutate(world_export_share = export / sum(export),
         world_production = sum(production),
         world_domestic_food_availability = sum(food),
         world_pop = sum(pop))

write_csv(pie_all, "data/food_balance_gwcode.csv")

library(ggplot2)
ggplot(pie_all |> filter(gwcode %in% c(2, 101, 200, 365, 430, 530, 700, 710, 750)), aes(x = year, y = domestic_share, group = gwcode, color = gwcode)) + 
  geom_line()

ggplot(pie_all |> filter(gwcode %in% c(2, 101, 200, 365, 430, 530, 700, 710, 750)), aes(x = year, y = world_export_share, group = gwcode, color = gwcode)) + 
  geom_line()

ggplot(pie_all, aes(x = year, y = world_export_share, group = gwcode, color = gwcode)) + 
  geom_line()
ggplot(pie_all, aes(x = year, y = world_production, group = gwcode, color = gwcode)) + 
  geom_line()
ggplot(pie_all, aes(x = year, y = world_domestic_food_availability, group = gwcode, color = gwcode)) + 
  geom_line()
ggplot(pie_all, aes(x = year, y = world_domestic_food_availability / world_production, group = gwcode, color = gwcode)) + 
  geom_line() + theme(legend.position = "none")

pie_all$world_domestic_food_availability |> summary()

pie_all |> filter(year == 1961) |> ungroup() |> summarize(mean(world_production)/mean(world_pop))
pie_all |> filter(year == 2020) |> ungroup() |> summarize(mean(world_production)/mean(world_pop))
pie_all |> filter(year == 1961) |> ungroup() |> summarize(mean(world_domestic_food_availability)/mean(world_pop))
pie_all |> filter(year == 2020) |> ungroup() |> summarize(mean(world_domestic_food_availability)/mean(world_pop))
pie_all$world_domestic_food_availability
pie_all$world_domestic_food_availability |> max() / 8e9
