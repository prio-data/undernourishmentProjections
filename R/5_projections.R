# See prepare_projections.R
all_projections <- arrow::read_parquet("data/base_projections.parquet")

all_projections[scenario == "SSP1"]$scenario <- "SSP1-2.6"
all_projections[scenario == "SSP2"]$scenario <- "SSP2-4.5"
all_projections[scenario == "SSP3"]$scenario <- "SSP3-7.0"
all_projections[scenario == "SSP4"]$scenario <- "SSP4-7.0"
all_projections[scenario == "SSP5"]$scenario <- "SSP5-8.5"



# Just do once
if(simulation_alternative == "base" & cv_approach == "regression" & TIME_INTERVAL == 1){
	log10_1p_trans <- scales::new_transform(
		name = "log10_1p",
		transform = function(x) log10(1 + x),
		inverse = function(x) 10^x - 1,
		domain = c(0, Inf)
	)

	all_projections$sim_modulo <- all_projections$sim %% 50
	# aggregate every 10th simulation
	agg_df <- all_projections[year >= 2024, .(best = mean(best), v2x_polyarchy = mean(v2x_polyarchy)), .(scenario, gwcode, year, sim_modulo)]
	agg_df <- agg_df[,-"sim_modulo"]
	scenarios <- names(plotting_colors)

	brd_plot <- function(s){
		to_plot <- rbindlist(list(main_df[year >= 1990, c("gwcode", "year", "best", "v2x_polyarchy")], agg_df[scenario == s, -"scenario"]))
		to_plot$scenario <- ifelse(to_plot$year >= 2024, s, "historical")
		agg_year <- to_plot[, .(best = mean(best, na.rm = T), v2x_polyarchy = mean(v2x_polyarchy, na.rm = T)), .(year)]
		plotting_colors_hist <- c(plotting_colors, "historical" = "gray50")
		to_plot$alpha_level <- if_else(to_plot$scenario == "historical", 0.7, 0.05)

		ggplot() +
			geom_point(data = to_plot, mapping = aes(x = year, y = best, color = scenario, alpha = alpha_level), shape = 20, size = 0.3) +
			geom_line(data = agg_year, aes(x = year, y = best), color = "black", linewidth = 1, linetype = "solid") +
			scale_y_continuous("BRD", transform = log10_1p_trans, breaks = c(0, 10, 100, 1000, 10000, 100000),
												 labels = c("0", "10", expression(10^2), expression(10^3),expression(10^4), expression(10^5))) +
			scale_color_manual(values = plotting_colors_hist) +
			scale_x_continuous("Year", breaks = c(1990, 2024, 2050)) +
			theme_bw(base_size = 24) + theme(legend.position = "none") + ggtitle(s)
	}

	dem_plot <- function(s){
		to_plot <- rbindlist(list(main_df[year >= 1990, c("gwcode", "year", "best", "v2x_polyarchy")], agg_df[scenario == s, -"scenario"]))
		to_plot$scenario <- ifelse(to_plot$year >= 2024, s, "historical")
		agg_year <- to_plot[, .(best = mean(best, na.rm = T), v2x_polyarchy = mean(v2x_polyarchy, na.rm = T)), .(year)]
		plotting_colors_hist <- c(plotting_colors, "historical" = "gray50")
		to_plot$alpha_level <- if_else(to_plot$scenario == "historical", 0.7, 0.05)

		ggplot() +
			geom_point(data = to_plot, mapping = aes(x = year, y = v2x_polyarchy, color = scenario, alpha = alpha_level), shape = 20, size = 0.3) +
			geom_line(data = agg_year, aes(x = year, y = v2x_polyarchy), color = "black", linewidth = 1, linetype = "solid") +
			ylab("DEM") +
			scale_color_manual(values = plotting_colors_hist) +
			scale_x_continuous("Year", breaks = c(1990, 2024, 2050)) +
			theme_bw(base_size = 24) + theme(legend.position = "none")
	}

	BRD_PLOTS <- lapply(scenarios, brd_plot)
	DEM_PLOTS <- lapply(scenarios, dem_plot)

	BRD_DEM <- BRD_PLOTS[[1]] + BRD_PLOTS[[2]] + BRD_PLOTS[[3]] + BRD_PLOTS[[4]] + BRD_PLOTS[[5]] +
	DEM_PLOTS[[1]] + DEM_PLOTS[[2]] + DEM_PLOTS[[3]] + DEM_PLOTS[[4]] + DEM_PLOTS[[5]] + plot_layout(axes = "collect", ncol = 5)

	ggsave(file.path("figures", simulation_alternative, "brd_dem_projections.png"), plot = BRD_DEM, device = ragg_png,  width = 12, height = 4, scale = 1.5)

}


#### Economic growth adjustments ####

if(simulation_alternative != "no_conflict_effect"){
	all_projections[, gdppc_l1 := shift(.SD, 1), by = c("scenario", "sim", "gwcode"), .SDcols = "gdppc"]
	all_projections[, gdppc_grwth := (gdppc - gdppc_l1)/gdppc_l1]
	all_projections[best >= 25 & year >= 2024, gdppc_grwth := gdppc_grwth - 0.0233]
	all_projections[is.na(gdppc_grwth), gdppc_grwth := 0]

	all_projections[, gdppc := Reduce(
		function(x, y) x * (1 + y),
		gdppc_grwth[-1],
		init = gdppc[1],
		accumulate = TRUE
	),
	by = .(scenario, sim, gwcode)]

	all_projections[year >= 2010, .(gdppc = median(gdppc, na.rm = TRUE)), by = .(scenario, year)] |>
		ggplot(aes(x = year, y = gdppc, color = scenario)) + geom_line()
}

if(simulation_alternative == "constant_democracy"){
	# Set democracy-levels to 2023
	all_projections[year >= 2024, v2x_polyarchy := NA_real_]
	all_projections[, v2x_polyarchy := nafill(v2x_polyarchy, type = "locf"),
									by = .(scenario, sim, gwcode)]
}

if(simulation_alternative == "constant_climate"){
	# Set climate-levels to average 2023 levels (climate scenarios start in 2015, but do not deviate significantly over the first 10 years.)
	baseline <- all_projections[year == 2023,
															.(tx90pgs = mean(tx90pgs, na.rm = TRUE),
																rx5daygs = mean(rx5daygs, na.rm = TRUE),
																spei6gs = mean(spei6gs, na.rm = TRUE),
																tas = mean(tas, na.rm = TRUE)),
															by = .(gwcode)]

	all_projections[baseline, on = .(gwcode), `:=`(
		tx90pgs_base = i.tx90pgs,
		rx5daygs_base = i.rx5daygs,
		spei6gs_base = i.spei6gs,
		tas_base = i.tas
	)]

	all_projections[year >= 2024, `:=`(
		tx90pgs = tx90pgs_base,
		rx5daygs = rx5daygs_base,
		spei6gs = spei6gs_base,
		tas = tas_base
	)]

	all_projections[, c("tx90pgs_base", "rx5daygs_base", "spei6gs_base", "tas_base") := NULL]
}

#### --------- CV projections ---------------- ####
if(cv_approach == "manual"){
	pre_2024 <- all_projections[year < 2024]
	all_projections <- all_projections[year >= 2024]


	if(simulation_alternative == "no_conflict_effect"){
		all_projections[, cv_adj := -0.003]
	} else{
		all_projections[, cv_adj := 0][best >= 1000, cv_adj := 0.01]
		all_projections[, nowar := 0][best < 1000, nowar := 1]
		all_projections[, wargrp := cumsum(nowar==0)]
		all_projections[, nowaryears := cumsum(nowar), by = c("scenario", "gwcode", "sim", "wargrp")]
		all_projections[nowaryears > 0, cv_adj := -0.003]
	}

	mincv <- as.data.table(main_df)[, min(cv, na.rm = T), by = "gwcode"][, .(gwcode, mincv = V1)]
	initial_cv <- pre_2024[year == 2023][, .(scenario, gwcode, sim, fcv = cv)]
	all_projections <- merge(all_projections, initial_cv, by = c("scenario", "gwcode", "sim"))
	all_projections <- merge(all_projections, mincv, by = "gwcode")
	all_projections[scenario == "SSP1", mincv := 0.2]
	all_projections[, diff_change := cv_adj != shift(cv_adj, fill = FALSE), by = c("scenario", "gwcode", "sim")]
	all_projections[, cvgrp := cumsum(diff_change)]
	all_projections[, cv_csum := cumsum(cv_adj), by = c("scenario", "gwcode", "sim", "cvgrp")]
	all_projections[(fcv + cv_csum - mincv) < 0, cv_adj := 0]
	all_projections[, cv_csum := cumsum(cv_adj), by = c("scenario", "gwcode", "sim")]
	all_projections[(fcv + cv_csum - mincv) < 0, cv_adj := 0]
	all_projections[, cv_csum := cumsum(cv_adj), by = c("scenario", "gwcode", "sim")]
	all_projections[(fcv + cv_csum) > 0.4, cv_adj := 0]
	all_projections[, cv_csum := cumsum(cv_adj), by = c("scenario", "gwcode", "sim")]
	all_projections[, cv := fcv + cv_csum]

	all_projections <- rbindlist(list(pre_2024, all_projections), use.names = TRUE, fill = TRUE)
	setorder(all_projections, scenario, gwcode, sim, year)
}

# Aggregate projections if TIME_INTERVAL > 1
if(TIME_INTERVAL > 1){
	mean_vars <- setdiff(names(all_projections), c("scenario", "gwcode", "year", "sim", "best"))
	all_projections[, period := (year - 2024) %/% TIME_INTERVAL, by = .(scenario, gwcode, sim)]
	all_projections <- all_projections[, c(
		list(year = max(year), best = sum(best, na.rm = TRUE)),
		lapply(.SD, mean, na.rm = TRUE)
	), by = .(scenario, gwcode, period, sim), .SDcols = mean_vars]
} else{
	all_projections$period <- all_projections$year
}


if(cv_approach == "regression"){
	result <- simulate_newdata(
		fit_cv,
		newdata = all_projections,
		by = c("scenario", "sim"),
		nsim = 1, # 1 simulated draw per sim in all_projections
		type = "data",
		include_pred = TRUE,
		seed = 42
	)

	result <- result[year>=2024]
	result[, cv_sim := cv[1] + cumsum(.pdi1_cv_sim), by = .(scenario, sim, gwcode)]
	result$cv <- result$cv_sim
} else{
	result <- all_projections # then CV was calculated manually
}

result[, .(scenario, gwcode, sim, year, cv, best)] |>
	ggplot(aes(x = year, y = cv, color = scenario)) + geom_smooth() + facet_wrap(~scenario)

result[gwcode == 530 & sim == 1, .(scenario, sim, year, cv, best)] |>
	ggplot(aes(x = year, y = cv, color = scenario)) + geom_line() + facet_wrap(~scenario, ncol = 1)


#### ----------- DES projections ---------- ####
des_result <- simulate_newdata(
	fit_des,
	newdata = all_projections,
	by = c("scenario", "sim"),
	nsim = 1, # 1 simulated draw per sim in all_projections
	type = "data",
	include_pred = TRUE,
	seed = 42
)

des_result <- des_result[year>=2024]
des_result[, des_sim := des + cumsum(.pdi1_des_sim), by = c("scenario", "gwcode", "sim")] # Simulations!
result <- merge(result, des_result[, c("scenario", "gwcode", "year", "sim", "des_sim")], by = c("scenario", "gwcode", "year", "sim"))

des_result[gwcode == 530 & sim == 1] |>
	ggplot(aes(x = year, y = gdppc, color = scenario)) + geom_line()
des_result[gwcode == 530 & sim == 1] |>
	ggplot(aes(x = year, y = mu, color = scenario)) + geom_line()
des_result[gwcode == 530 & sim == 1] |>
	ggplot(aes(x = year, y = .pdi1_des_sim, color = scenario)) + geom_line()

result[gwcode == 530 & sim == 1] |>
	ggplot(aes(x = year, y = des_sim, color = scenario)) + geom_line()

#### Calculate PoU and Number Undernourished ####
result[, pou := poldat::estimate_undernourishment(des_sim, cv, mder, population)$prevalence_of_undernourishment]
result[, nou := poldat::estimate_undernourishment(des_sim, cv, mder, population)$number_undernourished]

#### Aggregated results ####

# result[scenario == "SSP1"]$scenario <- "SSP1-2.6"
# result[scenario == "SSP2"]$scenario <- "SSP2-4.5"
# result[scenario == "SSP3"]$scenario <- "SSP3-7.0"
# result[scenario == "SSP4"]$scenario <- "SSP4-7.0"
# result[scenario == "SSP5"]$scenario <- "SSP5-8.5"

global_agg <- result[, .(
	nou = sum(nou, na.rm = TRUE),
	pou = weighted.mean(pou, population, na.rm = TRUE),
	des = weighted.mean(des_sim, population, na.rm = TRUE),
	mder = weighted.mean(mder, population, na.rm = TRUE),
	cv = weighted.mean(cv, population, na.rm = TRUE),
	cv_non_weight = mean(cv, na.rm = TRUE),
	best = sum(best, na.rm = TRUE),
	gdppc_non_weight = mean(gdppc, na.rm = TRUE),
	gdppc = weighted.mean(gdppc, population, na.rm = TRUE),
	v2x_polyarchy = weighted.mean(v2x_polyarchy, population, na.rm = TRUE),
	tx90pgs = weighted.mean(tx90pgs, population, na.rm = TRUE),
	rx5daygs = weighted.mean(rx5daygs, population, na.rm = TRUE),
	population = sum(population, na.rm = TRUE)
), by = .(scenario, year, sim)]
global_agg[, total_des := des * population]


saveRDS(global_agg, file.path("results", paste0("timeint", TIME_INTERVAL), simulation_alternative, cv_approach, "global_agg.rds"))


country_agg <- result[, .(
	nou = sum(nou, na.rm = TRUE),
	pou = mean(pou, na.rm = TRUE),
	des = mean(des_sim, na.rm = TRUE),
	mder = mean(mder, na.rm = TRUE),
	cv = mean(cv, na.rm = TRUE),
	best = sum(best, na.rm = TRUE),
	gdppc = mean(gdppc, na.rm = TRUE),
	v2x_polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
	tx90pgs = mean(tx90pgs, na.rm = TRUE),
	rx5daygs = mean(rx5daygs, na.rm = TRUE),
	population = sum(population, na.rm = TRUE)
), by = .(scenario, gwcode, year)]
country_agg[, total_des := des * population]


saveRDS(country_agg, file.path("results", paste0("timeint", TIME_INTERVAL), simulation_alternative, cv_approach, "country_agg.rds"))

global_hist <- main_df[gwcode %in% unique(result$gwcode)]
global_hist[, nou := poldat::estimate_undernourishment(des, cv, mder, population)$number_undernourished]
global_hist <- global_hist[, .(
	nou = sum(nou, na.rm = TRUE),
	pou = weighted.mean(pou, population, na.rm = TRUE),
	des = weighted.mean(des, population, na.rm = TRUE),
	mder = weighted.mean(mder, population, na.rm = TRUE),
	cv = weighted.mean(cv, population, na.rm = TRUE),
	cv_non_weight = mean(cv, na.rm = TRUE),
	best = sum(best, na.rm = TRUE),
	gdppc_non_weight = mean(gdppc, na.rm = TRUE),
	gdppc = weighted.mean(gdppc, population, na.rm = TRUE),
	v2x_polyarchy = weighted.mean(v2x_polyarchy, population, na.rm = TRUE),
	tx90pgs = weighted.mean(tx90pgs, population, na.rm = TRUE),
	rx5daygs = weighted.mean(rx5daygs, population, na.rm = TRUE),
	population = sum(population, na.rm = TRUE)
),
by = year
]
global_hist[, total_des := des * population]
global_hist <- na.omit(global_hist)

saveRDS(global_hist, file.path("results", paste0("timeint", TIME_INTERVAL), "global_hist.rds"))

