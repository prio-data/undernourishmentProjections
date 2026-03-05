library(data.table)
library(ggplot2)
library(ggpubr)
library(foreach)
library(doMC)
library(plotly)
library(splines)

registerDoMC(8)

path_sim <- "data/sim/"

source(file = "R/simulation_functions.R")
source(file = "R/simulation_functions_plot.R")
source(file = "R/ppc_functions.R")


# Data -------------------------------------------------------------------------
filename <- "data/main.csv"

data_raw <- fread(file = filename)

data <- data_raw[, .(gwcode, year, pol = v2x_polyarchy, gdppc, country_name)]

# fix missing country name
data <- foreach(gwcode_i = unique(data$gwcode), .combine = rbind) %do% {
	data_i <- data[gwcode == gwcode_i & year >= 1960]

	country_name_i <- unique(data_i[!is.na(country_name)]$country_name)

	if (length(country_name_i) == 1) {
		data_i$country_name <- country_name_i
		return(data_i)
	} else {
		print(gwcode_i)
		return(NULL)
	}
}

# TODO: Make this into a function
source("R/test_add_region.R")

# ------------------------------------------------------------------------------

data_sim <- data[1989 <= year & year < 2024]
data_sim <- data_sim[pol > 1e-6]
data_sim <- data_sim[region != "NA"]

S <- 500

# PPC Goodness-of-fit Analysis -------------------------------------------------
data_train <- copy(data)
data_train <- data_train[pol > 1e-6]
data_train[, pol_trans := sigmoid_inv(pol)]
data_train[, pol_trans_delta := c(diff(pol_trans), NA), by = "gwcode"]
data_train <- data_train[!is.na(pol + pol_trans + pol_trans_delta)]

res <- ppc_polyarchy(data_sim, data_train, Nsim = 1000)
print(res$summary)

# plot of simulations
plot_cases(
	gwcodes = c(812, 211, 800),
	N_plot = 200,
	save_path = "figures/base/democracy_simulation_examples.png"
)
# ------------------------------------------------------------------------------



# SSP1 -------------------------------------------------------------------------

data_train_ssp1 <- data[1991 <= year & year <= 2010 & region == "OECD"]
data_train_ssp1 <- data_train_ssp1[pol > 1e-6]
data_train_ssp1[, pol_trans := sigmoid_inv(pol)]
data_train_ssp1[, pol_trans_delta := c(diff(pol_trans), NA), by = "gwcode"]
data_train_ssp1 <- data_train_ssp1[!is.na(pol + pol_trans + pol_trans_delta)]

gwcode_list_ <- sort(unique(data_sim$gwcode))

sim_ <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	data_i <- data_sim[gwcode == gwcode_i]

	if (nrow(data_i) < 1) return(NULL)
	if (last(data_i$year) != 2023) return(NULL)
	if (is.na(last(data_i$pol))) return(NULL)


	polyarchy_start_i <- last(data_i$pol)
	polyarchy_end_i <- 0.85
	country_name_i <- last(data_i$country_name)
	year_i <- last(data_i$year)
	region_i <- last(data_i$region)

	data_sim_i <- sim_polyarchy(n = 27, N = S, polyarchy_start = polyarchy_start_i, polyarchy_end = polyarchy_end_i, polyarchy_data = data_train_ssp1, alpha = 1)

	data_sim_0 <- data_i[, .(country_name, gwcode, year, pol, region, sim = 0)]
	data_sim_i <- data_sim_i[, .(country_name = country_name_i, gwcode = gwcode_i, year = index + year_i, pol, region = region_i, sim)]

	data_sim_i <- rbindlist(list(data_sim_0, data_sim_i), use.names = TRUE)


	return(data_sim_i)
}

plot_dem_summary(data = sim_, filename = "sim_summary_ssp1_dem.pdf", path = path_sim)
if (!is.null(path_sim))  fwrite(sim_, file = paste0(path_sim, "/ssp1_dem.csv"))
rm(sim_)
# ------------------------------------------------------------------------------





# SSP2 -------------------------------------------------------------------------

data_train_ssp2 <- data[1991 <= year & year <= 2022]
data_train_ssp2 <- data_train_ssp2[pol > 1e-6]
data_train_ssp2[, pol_trans := sigmoid_inv(pol)]
data_train_ssp2[, pol_trans_delta := c(diff(pol_trans), NA), by = "gwcode"]
data_train_ssp2 <- data_train_ssp2[!is.na(pol + pol_trans + pol_trans_delta)]

gwcode_list_ <- sort(unique(data_sim$gwcode))

sim_ <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	data_i <- data_sim[gwcode == gwcode_i]

	if (nrow(data_i) < 1) return(NULL)
	if (last(data_i$year) != 2023) return(NULL)
	if (is.na(last(data_i$pol))) return(NULL)


	polyarchy_start_i <- last(data_i$pol)
	polyarchy_end_i <- polyarchy_start_i
	country_name_i <- last(data_i$country_name)
	year_i <- last(data_i$year)
	region_i <- last(data_i$region)

	data_sim_i <- sim_polyarchy(n = 27, N = S, polyarchy_start = polyarchy_start_i, polyarchy_end = polyarchy_end_i, polyarchy_data = data_train_ssp2, alpha = 1)

	data_sim_0 <- data_i[, .(country_name, gwcode, year, pol, region, sim = 0)]
	data_sim_i <- data_sim_i[, .(country_name = country_name_i, gwcode = gwcode_i, year = index + year_i, pol, region = region_i, sim)]

	data_sim_i <- rbindlist(list(data_sim_0, data_sim_i), use.names = TRUE)


	return(data_sim_i)
}

plot_dem_summary(data = sim_, filename = "sim_summary_ssp2_dem.pdf", path = path_sim)
if (!is.null(path_sim)) fwrite(sim_, file = paste0(path_sim, "/ssp2_dem.csv"))
rm(sim_)
# ------------------------------------------------------------------------------





# SSP3 -------------------------------------------------------------------------

data_train_ssp3 <- data[1946 <= year & year <= 1990]
data_train_ssp3[, pol_trans := sigmoid_inv(pol)]
data_train_ssp3[, pol_trans_delta := c(diff(pol_trans), NA), by = "gwcode"]
data_train_ssp3 <- data_train_ssp3[pol > 1e-6]
data_train_ssp3 <- data_train_ssp3[!is.na(pol + pol_trans + pol_trans_delta)]

gwcode_list_ <- sort(unique(data_sim$gwcode))

sim_ <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	data_i <- data_sim[gwcode == gwcode_i]

	if (nrow(data_i) < 1) return(NULL)
	if (last(data_i$year) != 2023) return(NULL)
	if (is.na(last(data_i$pol))) return(NULL)


	polyarchy_start_i <- last(data_i$pol)
	polyarchy_end_i <- 0.30
	country_name_i <- last(data_i$country_name)
	year_i <- last(data_i$year)
	region_i <- last(data_i$region)

	data_sim_i <- sim_polyarchy(n = 27, N = S, polyarchy_start = polyarchy_start_i, polyarchy_end = polyarchy_end_i, polyarchy_data = data_train_ssp3, alpha = 1)

	data_sim_0 <- data_i[, .(country_name, gwcode, year, pol, region, sim = 0)]
	data_sim_i <- data_sim_i[, .(country_name = country_name_i, gwcode = gwcode_i, year = index + year_i, pol, region = region_i, sim)]

	data_sim_i <- rbindlist(list(data_sim_0, data_sim_i), use.names = TRUE)


	return(data_sim_i)
}

plot_dem_summary(data = sim_, filename = "sim_summary_ssp3_dem.pdf", path = path_sim)
if (!is.null(path_sim))  fwrite(sim_, file = paste0(path_sim, "/ssp3_dem.csv"))
rm(sim_)

# ------------------------------------------------------------------------------






# SSP4 -------------------------------------------------------------------------
gwcode_list_ <- sort(unique(data_sim$gwcode))

data_gdppc <- data_sim[2010 <= year & year <= 2023, .(gdppc = mean(gdppc, na.rm = TRUE)), by = c("region", "gwcode")]
data_gdppc_quantile <- data_gdppc[, .(quantile = quantile(gdppc, 0.80, na.rm = TRUE)), by = "region"]

data_sim[, is_rich := 0]

for (region_i in unique(data_gdppc$region)) {
	gwcode_i <- data_gdppc[region == region_i & gdppc >= data_gdppc_quantile[region == region_i]$quantile]$gwcode
	data_sim[gwcode %in% gwcode_i, is_rich := 1]
}


sim_ <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	data_i <- data_sim[gwcode == gwcode_i]

	if (nrow(data_i) < 1) return(NULL)
	if (last(data_i$year) != 2023) return(NULL)
	if (is.na(last(data_i$pol))) return(NULL)

	polyarchy_start_i <- last(data_i$pol)
	country_name_i <- last(data_i$country_name)
	year_i <- last(data_i$year)
	region_i <- last(data_i$region)
	is_rich_i <- last(data_i$is_rich)

	if (is_rich_i == 1) {
		data_sim_i <- sim_polyarchy(n = 27, N = S, polyarchy_start = polyarchy_start_i, polyarchy_end = polyarchy_start_i, polyarchy_data = data_train_ssp2, alpha = 1)
	} else {
		data_sim_i <- sim_polyarchy(n = 27, N = S, polyarchy_start = polyarchy_start_i, polyarchy_end = 0.30, polyarchy_data = data_train_ssp3, alpha = 1)
	}

	data_sim_0 <- data_i[, .(country_name, gwcode, year, pol, region, sim = 0)]
	data_sim_i <- data_sim_i[, .(country_name = country_name_i, gwcode = gwcode_i, year = index + year_i, pol, region = region_i, sim)]

	data_sim_i <- rbindlist(list(data_sim_0, data_sim_i), use.names = TRUE)


	return(data_sim_i)
}

plot_dem_summary(data = sim_, filename = "sim_summary_ssp4_dem.pdf", path = path_sim)
if (!is.null(path_sim)) fwrite(sim_, file = paste0(path_sim, "/ssp4_dem.csv"))
rm(sim_)
# ------------------------------------------------------------------------------






# SSP5 -------------------------------------------------------------------------
gwcode_list_ <- sort(unique(data_sim$gwcode))

data_train_ssp5 <- data[1991 <= year & year <= 2023]
data_train_ssp5[, pol_trans := sigmoid_inv(pol)]
data_train_ssp5[, pol_trans_delta := c(diff(pol_trans), NA), by = "gwcode"]
data_train_ssp5 <- data_train_ssp5[pol > 1e-6]
data_train_ssp5 <- data_train_ssp5[!is.na(pol + pol_trans + pol_trans_delta)]

gwcode_list_ <- sort(unique(data_sim$gwcode))

data_sim[, is_democratic := 0]

for (gwcode_i in gwcode_list_) {
	data_sim_i <- data_sim[gwcode == gwcode_i]

	if (nrow(data_sim_i) < 1) next
	if (last(data_sim_i$year) != 2023) next
	if (is.na(last(data_sim_i$pol))) next

	fit_i <- lm(pol ~ year, data = data_sim_i)
	pred_i <- predict(fit_i, data.frame(year = 2050))

	if (pred_i > 0.6) {
		data_sim[gwcode == gwcode_i, is_democratic := 1]
	}

}


sim_ <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	data_i <- data_sim[gwcode == gwcode_i]

	if (nrow(data_i) < 1) return(NULL)
	if (last(data_i$year) != 2023) return(NULL)
	if (is.na(last(data_i$pol))) return(NULL)

	polyarchy_start_i <- last(data_i$pol)
	country_name_i <- last(data_i$country_name)
	year_i <- last(data_i$year)
	region_i <- last(data_i$region)
	is_democratic_i <- last(data_i$is_democratic)

	if (is_democratic_i == 1) {
		data_sim_i <- sim_polyarchy(n = 27, N = S, polyarchy_start = polyarchy_start_i, polyarchy_end = 0.85, polyarchy_data = data_train_ssp5, alpha = 1)
	} else {
		data_sim_i <- sim_polyarchy(n = 27, N = S, polyarchy_start = polyarchy_start_i, polyarchy_end = 0.15, polyarchy_data = data_train_ssp5, alpha = 1)
	}

	data_sim_0 <- data_i[, .(country_name, gwcode, year, pol, region, sim = 0)]
	data_sim_i <- data_sim_i[, .(country_name = country_name_i, gwcode = gwcode_i, year = index + year_i, pol, region = region_i, sim)]

	data_sim_i <- rbindlist(list(data_sim_0, data_sim_i), use.names = TRUE)


	return(data_sim_i)
}

plot_dem_summary(data = sim_, filename = "sim_summary_ssp5_dem.pdf", path = path_sim)
if (!is.null(path_sim)) fwrite(sim_, file = paste0(path_sim, "/ssp5_dem.csv"))
rm(sim_)
# ------------------------------------------------------------------------------




