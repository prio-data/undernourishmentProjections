# See prepare_projections.R
all_projections <- arrow::read_parquet("data/base_projections.parquet")

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

#### ----------- DES projections ---------- ####
result <- simulate_newdata(
	fit_des,
	newdata = all_projections,
	by = c("scenario", "sim"),
	nsim = 1, # 1 simulated draw per sim in all_projections
	type = "data",
	include_pred = TRUE,
	seed = 42
)

result[gwcode == 530 & sim == 1] |>
	ggplot(aes(x = year, y = gdppc, color = scenario)) + geom_line()
result[gwcode == 530 & sim == 1] |>
	ggplot(aes(x = year, y = mu, color = scenario)) + geom_line()
result[gwcode == 530 & sim == 1] |>
	ggplot(aes(x = year, y = .pdi1_des_sim, color = scenario)) + geom_line()

result <- result[year>=2024]
result[, des_sim := des + cumsum(.pdi1_des_sim), by = c("scenario", "gwcode", "sim")] # Simulations!

result[gwcode == 530 & sim == 1] |>
	ggplot(aes(x = year, y = des_sim, color = scenario)) + geom_line()


#### --------- CV projections ---------------- ####




if(cv_approach == "manual"){
	if(simulation_alternative == "no_conflict_effect"){
		result[, cv_adj := -0.003]
	} else{
		result[, cv_adj := 0][best >= 1000, cv_adj := 0.01]
		result[, nowar := 0][best < 1000, nowar := 1]
		result[, wargrp := cumsum(nowar==0)]
		result[, nowaryears := cumsum(nowar), by = c("scenario", "gwcode", "sim", "wargrp")]
		result[nowaryears > 0, cv_adj := -0.003]
	}

	mincv <- as.data.table(main_df)[, min(cv, na.rm = T), by = "gwcode"][, .(gwcode, mincv = V1)]
	initial_cv <- result[, .SD[1], by = c("scenario", "gwcode", "sim")][, .(scenario, gwcode, sim, fcv = cv)]
	result <- merge(result, initial_cv, by = c("scenario", "gwcode", "sim"))
	result <- merge(result, mincv, by = "gwcode")
	result[scenario == "SSP1", mincv := 0.2]
	result[, diff_change := cv_adj != shift(cv_adj, fill = FALSE), by = c("scenario", "gwcode", "sim")]
	result[, cvgrp := cumsum(diff_change)]
	result[, cv_csum := cumsum(cv_adj), by = c("scenario", "gwcode", "sim", "cvgrp")]
	result[(fcv + cv_csum - mincv) < 0, cv_adj := 0]
	result[, cv_csum := cumsum(cv_adj), by = c("scenario", "gwcode", "sim")]
	result[(fcv + cv_csum - mincv) < 0, cv_adj := 0]
	result[, cv_csum := cumsum(cv_adj), by = c("scenario", "gwcode", "sim")]
	result[(fcv + cv_csum) > 0.4, cv_adj := 0]
	result[, cv_csum := cumsum(cv_adj), by = c("scenario", "gwcode", "sim")]
	result[, cv := fcv + cv_csum]
} else{
	cv_result <- simulate_newdata(
		fit_cv10,
		newdata = all_projections,
		by = c("scenario", "sim"),
		nsim = 1, # 1 simulated draw per sim in all_projections
		type = "data",
		include_pred = TRUE,
		seed = 42
	)

	cv_result <- cv_result[year>=2024]
	#cv_result[, chain := year %% 1]
	#cv_result[, cv_sim := cv[1] + cumsum(.pdi1_cv_sim), by = .(scenario, sim, gwcode, chain)]
	cv_result[, cv_sim := cv[1] + cumsum(.pdi1_cv_sim), by = .(scenario, sim, gwcode)]

	result <- merge(result, cv_result[, c("scenario", "gwcode", "year", "sim", "cv_sim")], by = c("scenario", "gwcode", "year", "sim"))
	result$cv <- result$cv_sim
}

result[, .(scenario, gwcode, sim, year, cv, best)] |>
	ggplot(aes(x = year, y = cv, color = scenario)) + geom_smooth() + facet_wrap(~scenario)

result[, pou := poldat::estimate_undernourishment(des_sim, cv, mder, population)$prevalence_of_undernourishment]
result[, nou := poldat::estimate_undernourishment(des_sim, cv, mder, population)$number_undernourished]


#### Aggregated results ####

result[scenario == "SSP1"]$scenario <- "SSP1-2.6"
result[scenario == "SSP2"]$scenario <- "SSP2-4.5"
result[scenario == "SSP3"]$scenario <- "SSP3-7.0"
result[scenario == "SSP4"]$scenario <- "SSP4-7.0"
result[scenario == "SSP5"]$scenario <- "SSP5-8.5"

global_agg <- result[, .(
	nou = sum(nou, na.rm = TRUE),
	pou = weighted.mean(pou, population, na.rm = TRUE),
	des = weighted.mean(des_sim, population, na.rm = TRUE),
	mder = weighted.mean(mder, population, na.rm = TRUE),
	cv = weighted.mean(cv, population, na.rm = TRUE),
	best = sum(best, na.rm = TRUE),
	gdppc_non_weight = mean(gdppc, na.rm = TRUE),
	gdppc = weighted.mean(gdppc, population, na.rm = TRUE),
	v2x_polyarchy = weighted.mean(v2x_polyarchy, population, na.rm = TRUE),
	tx90pgs = weighted.mean(tx90pgs, population, na.rm = TRUE),
	rx5daygs = weighted.mean(rx5daygs, population, na.rm = TRUE),
	population = sum(population, na.rm = TRUE)
), by = .(scenario, year, sim)]
global_agg[, total_des := des * population]

saveRDS(global_agg, file.path("results", simulation_alternative, cv_approach, "global_agg.rds"))

global_hist <- main_df[gwcode %in% unique(result$gwcode)]
global_hist[, nou := poldat::estimate_undernourishment(des, cv, mder, population)$number_undernourished]
global_hist <- global_hist[, .(
	nou = sum(nou, na.rm = TRUE),
	pou = weighted.mean(pou, population, na.rm = TRUE),
	des = weighted.mean(des, population, na.rm = TRUE),
	mder = weighted.mean(mder, population, na.rm = TRUE),
	cv = weighted.mean(cv, population, na.rm = TRUE),
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

saveRDS(global_hist, file.path("results", "global_hist.rds"))
