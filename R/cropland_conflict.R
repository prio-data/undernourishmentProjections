# renv::install("prio-data/priogrid", rebuild = TRUE)
renv::install("assertthat")
library(tidyverse)
library(poldat)
library(priogrid)

ged <- priogrid::read_ucdp_ged()
ged_counts <- priogrid::load_pgvariable("ucdp_ged") # PRIOGRID Version 3.0.1, UCDP GED 25.1
gwcodes <- priogrid::load_pgvariable("cshapes_gwcode")


cshp <- poldat::cshp_gw_modifications() |>
  dplyr::mutate(
    gweyear = lubridate::year(end),
    gwemonth = lubridate::month(end),
    gweday = lubridate::day(end),
    gwsyear = lubridate::year(start),
    gwsmonth = lubridate::month(start),
    gwsday = lubridate::day(start)
  ) |>
  dplyr::mutate(
    gweyear = dplyr::if_else(gweyear == max(gweyear), lubridate::year(max(ged$date_end)), gweyear)
  ) |>
  dplyr::mutate(
    start = lubridate::ymd(paste0(gwsyear, "-", gwsmonth, "-", gwsday)),
    end = lubridate::ymd(paste0(gweyear, "-", gwemonth, "-", gweday))
  ) |>
  dplyr::mutate(
    date_interval = lubridate::interval(start, end)
  ) |>
  dplyr::rename(gwsdate = start, gwedate = end)


pgstartdate <- pgoptions$get_start_date()
pgenddate <- pgoptions$get_end_date()
pgcrs <- pgoptions$get_crs()
pgncol <- pgoptions$get_ncol()
pgnrow <- pgoptions$get_nrow()
pgtempres <- pgoptions$get_temporal_resolution()
pgoptions$set_start_date(as.Date("1989-12-31"))
pgoptions$set_end_date(as.Date(ged$date_end |> max()))
pgoptions$set_temporal_resolution("1 year")
pgoptions$set_nrow(360)
pgoptions$set_ncol(720)
pgoptions$set_crs("epsg:4326")


measurement_dates <- pg_dates()
dist <- priogrid::ucdpged_distance_within_country(measurement_dates[1], ged = ged, cshp = cshp)
for(i in 2:length(measurement_dates)){
	tmp <- priogrid::ucdpged_distance_within_country(measurement_dates[i], ged = ged, cshp = cshp)
	terra::add(dist) <- tmp
}
names(dist) <- as.character(measurement_dates)


# SPAM
spam <- read_csv("data_raw/spam2020V1r0_global_harvested_area/spam2020V1r0_global_H_TA.csv") |> # both irrigated and rainfed
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)
spam_phys <- read_csv("data_raw/spam2020V1r0_global_physical_area-2/spam2020V1r0_global_A_TA.csv") |> # both irrigated and rainfed
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

spam_template <- terra::rast("data_raw/spam2020V1r0_global_physical_area/spam2020_v1r0_global_A_VEGE_I.tif")

spam |> sf::st_geometry() |> plot(pch = ".")

spam <- spam_phys |>
  dplyr::rowwise() |>
  dplyr::mutate(area = sum(c_across(ends_with("_A")), na.rm = T)) |>
  dplyr::ungroup()

spam <- spam |>
  dplyr::mutate(larea = log1p(area))
plot(spam["larea"], pch = ".")

r <- terra::rasterize(spam, spam_template, field = "larea")
names(r) <- "crop_larea"
r_agg <- priogrid::robust_transformation(r, agg_fun = "sum")

r2 <- terra::rasterize(spam, spam_template, field = "area")
names(r2) <- "crop_area"


cshp$crop_country_sum <- exactextractr::exact_extract(r2, cshp, fun = "sum")

sf::sf_use_s2(FALSE)
cshp <- cshp |> dplyr::mutate(carea = as.numeric(sf::st_area(geometry))/1e4) # to hectar
sf::sf_use_s2(TRUE)

cshp <- cshp |>
  dplyr::mutate(crop_country_share = crop_country_sum / carea)

cshp_cs <- cshp |> dplyr::filter(as.Date("2024-12-31") %within% date_interval)
plot(cshp_cs["crop_country_share"])

crop_area <- cshp |> dplyr::select(gwcode, date_interval, crop_country_sum, crop_country_share)
sf::st_geometry(crop_area) <- NULL

mydates <- seq.Date(as.Date("1970-12-31"),
                    as.Date("2024-12-31"), by = "year")

crop_area_yearly <- tibble()
for(i in 1:length(mydates)){
  tmp <- crop_area |>
    dplyr::filter(mydates[i] %within% date_interval) |>
    dplyr::mutate(year = lubridate::year(mydates[i])) |>
    dplyr::select(gwcode, year, crop_country_sum, crop_country_share)
  crop_area_yearly <- dplyr::bind_rows(crop_area_yearly, tmp)
}


conflict_incidence <- ged_counts
conflict_incidence[!is.na(conflict_incidence)] <- 1


ged_dist <- (1/((dist/1e6)+1))

crop_ged_distance <- ged_dist * r_agg
crop_ged_brd <- log(ged_counts) * r_agg
crop_ged_incidence <- conflict_incidence * r_agg


crop_ged_brd[["2024-12-31"]] |> plot(main = "log(GED BRDs) * log(area), 2024")
crop_ged_distance[["2024-12-31"]] |> plot(main = "GED Distance * log(area), 2024")
crop_ged_incidence[["2024-12-31"]] |> plot(main = "GED Event * log(area), 2024")


crop_ged_distance_df <- crop_ged_distance |> priogrid::rast_to_df(static= FALSE, varname = "crop_ged_distance")
crop_ged_brd_df <- crop_ged_brd |> priogrid::rast_to_df(static= FALSE, varname = "crop_ged_brd")
crop_ged_incidence_df <- crop_ged_incidence |> priogrid::rast_to_df(static= FALSE, varname = "crop_ged_incidence")

gwcodes_df <- gwcodes |> priogrid::rast_to_df(static = FALSE, varname = "gwcode")

df <- dplyr::left_join(gwcodes_df, crop_ged_distance_df, by = c("pgid", "measurement_date")) |>
  dplyr::left_join(crop_ged_brd_df, by = c("pgid", "measurement_date")) |>
  dplyr::left_join(crop_ged_incidence_df, by = c("pgid", "measurement_date")) |>
  na.omit() |>
  dplyr::mutate(year = lubridate::year(measurement_date)) |>
  dplyr::group_by(gwcode, year) |>
  dplyr::summarise(crop_ged_distance_m = mean(crop_ged_distance, na.rm = TRUE),
                   crop_ged_distance_s = sum(crop_ged_distance, na.rm = TRUE),
                   crop_ged_brd_m = mean(crop_ged_brd, na.rm = TRUE),
                   crop_ged_brd_s = sum(crop_ged_brd, na.rm = TRUE),
                   crop_ged_incidence_m = mean(crop_ged_incidence, na.rm = TRUE),
                   crop_ged_incidence_s = sum(crop_ged_incidence, na.rm = TRUE))

df <- dplyr::left_join(crop_area_yearly, df, by = c("gwcode", "year")) |>
  tsibble::tsibble(key = "gwcode", index = "year")

area_weighted_df <- df |>
  tsibble::tsibble(key = "gwcode", index = "year") |>
  dplyr::mutate(across(starts_with("crop_ged"), ~ if_else(is.na(.x) & year >= 1989, 0, .x))) |>
  area_weighted_synthetic_data(2019)

write_csv(area_weighted_df, "data/crop_ged_area_weighted.csv")
