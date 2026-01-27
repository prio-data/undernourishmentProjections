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
	# Set democracy-levels to 2023
	all_projections[year >= 2024, tx90pgs := NA_real_]
	all_projections[, tx90pgs := nafill(tx90pgs, type = "locf"),
									by = .(scenario, sim, gwcode)]
}

#### ----------- DES projections ---------- ####
result <- simulate_newdata(
	fit1,
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

result <- result[year>=2024]
result[, des_sim := des + cumsum(.pdi1_des_sim), by = c("scenario", "gwcode", "sim")] # Simulations!

result[gwcode == 530 & sim == 1] |>
	ggplot(aes(x = year, y = des_sim, color = scenario)) + geom_line()


#### --------- CV projections ---------------- ####

mincv <- as.data.table(main_df)[, min(cv, na.rm = T), by = "gwcode"][, .(gwcode, mincv = V1)]

if(simulation_alternative == "no_conflict_effect"){
	result[, cv_adj := -0.003]
} else{
	result[, cv_adj := 0][best >= 1000, cv_adj := 0.01]
	result[, nowar := 0][best < 1000, nowar := 1]
	result[, wargrp := cumsum(nowar==0)]
	result[, nowaryears := cumsum(nowar), by = c("scenario", "gwcode", "sim", "wargrp")]
	result[nowaryears > 0, cv_adj := -0.003]
}

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

result[gwcode == 750, .(scenario, gwcode, sim, year, cv, best)] |>
	ggplot(aes(x = year, y = cv, group = sim, color = scenario)) + geom_line() + facet_wrap(~scenario)


#### Plot projections ####

result[scenario == "SSP1"]$scenario <- "SSP1-2.6"
result[scenario == "SSP2"]$scenario <- "SSP2-4.5"
result[scenario == "SSP3"]$scenario <- "SSP3-7.0"
result[scenario == "SSP4"]$scenario <- "SSP4-7.0"
result[scenario == "SSP5"]$scenario <- "SSP5-8.5"


plotting_colors <- c("SSP1" = "#1E9620",
										 "SSP2" = "#4576BF",
										 "SSP3" = "#F21111",
										 "SSP4" = "#E88831",
										 "SSP5" = "#8036A8")

names(plotting_colors) <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP4-7.0", "SSP5-8.5")

result[, pou := poldat::estimate_undernourishment(des_sim, cv, mder, population)$prevalence_of_undernourishment]
result[, nou := poldat::estimate_undernourishment(des_sim, cv, mder, population)$number_undernourished]

to_plot <- result[year == 2050,
			 .(
			 	nou = sum(nou),
			 	pou = mean(pou)
			 ),
			 by = .(scenario, gwcode)]

last_observed <- main_df[year == 2023, c("gwcode", "pou")]
data.table::setnames(last_observed, old = "pou", new = "pou2023")

to_plot <- merge(to_plot, last_observed, by = "gwcode")
to_plot[, pou_diff := pou - pou2023]


map <- cshapes::cshp(date = as.Date("2019-01-01"))
last_observed <- left_join(map, main_df[year == 2023], by = "gwcode") |>
	dplyr::mutate(scenario = "Observed 2023", nou = (pou * population)*1000)
ssp1_map <- left_join(map, to_plot |> dplyr::filter(scenario == "SSP1-2.6"), by = "gwcode") |> dplyr::mutate(scenario = "SSP1-2.6")
ssp2_map <- left_join(map, to_plot |> dplyr::filter(scenario == "SSP2-4.5"), by = "gwcode") |> dplyr::mutate(scenario = "SSP2-4.5")
ssp3_map <- left_join(map, to_plot |> dplyr::filter(scenario == "SSP3-7.0"), by = "gwcode") |> dplyr::mutate(scenario = "SSP3-7.0")
ssp4_map <- left_join(map, to_plot |> dplyr::filter(scenario == "SSP4-7.0"), by = "gwcode") |> dplyr::mutate(scenario = "SSP4-7.0")
ssp5_map <- left_join(map, to_plot |> dplyr::filter(scenario == "SSP5-8.5"), by = "gwcode") |> dplyr::mutate(scenario = "SSP5-8.5")
maps <- bind_rows(ssp1_map, ssp2_map, ssp3_map, ssp4_map, ssp5_map)
maps$pou_diff |> summary()
A <- ggplot(ssp1_map) +
	geom_sf(aes(fill = pou_diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.26, 0.085)) +
	theme_bw(base_size = 24) + labs(subtitle = "ΔPoU SSP1 2050")
B <- ggplot(ssp2_map) +
	geom_sf(aes(fill = pou_diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.26, 0.085)) +
	theme_bw(base_size = 24) + labs(subtitle = "ΔPoU SSP2 2050")
C <- ggplot(ssp3_map) +
	geom_sf(aes(fill = pou_diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.26, 0.085)) +
	theme_bw(base_size = 24) + labs(subtitle = "ΔPoU SSP3 2050")
D <- ggplot(ssp4_map) +
	geom_sf(aes(fill = pou_diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.26, 0.085)) +
	theme_bw(base_size = 24) + labs(subtitle = "ΔPoU SSP4 2050")
E <- ggplot(ssp5_map) +
	geom_sf(aes(fill = pou_diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.26, 0.085)) +
	theme_bw(base_size = 24) + labs(subtitle = "ΔPoU SSP5 2050")
F1 <- ggplot(last_observed) +
	geom_sf(aes(fill = pou)) +
	scale_fill_viridis_c("PoU") +
	theme_bw(base_size = 24) + labs(subtitle = "PoU 2023")

F1 + A + B + C + D + E + plot_layout(guides = "collect")
ggsave(file.path("figures", simulation_alternative, "pou_map.png"), device = ragg_png,  width = 12, height = 5, scale = 1.5)


to_plot <- result[gwcode == 475 & sim <10, -"des"]
setnames(to_plot, old = "des_sim", new = "des")

ggplot(to_plot) +
	geom_line(data = main_df[gwcode == 475], aes(x = year, y = des,)) +
	geom_line(data = to_plot, aes(x = year, y = des, color = scenario, group = sim)) +
	facet_wrap(~scenario) +
	scale_color_manual(values = plotting_colors) +
	labs(title = "Example DES Projections for\nNigeria (475)",
			 x = "Year",
			 y = "DES") +
	theme_bw(base_size = 16) + theme(legend.position = "bottom")
ggsave(file.path("figures", simulation_alternative, "des_nigeria_example.png"), device = ragg_png,  width = 12, height = 12)



to_plot <- result[year == 2050, .(
	pou = weighted.mean(pou, population)
), by = .(scenario, sim)]
to_plot$scenario <- forcats::fct_rev(to_plot$scenario)

POU_BOX <- ggplot(to_plot, aes(y = scenario, x = pou, fill = scenario)) +
	geom_boxplot() +
	ylab("") + xlab("PoU") +
	geom_vline(xintercept = 0.025, linetype = "dashed") + # SDG Zero hunger goal
	geom_vline(xintercept = 0.091, linetype = "dotted") + # Last observed according to FAO
	scale_fill_manual("Scenario", values = plotting_colors, guide = "none") +
	theme_bw(base_size = 24)

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
	population = sum(population, na.rm = TRUE)
), by = .(scenario, year, sim)]
global_agg[, total_des := des * population]


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
	population = sum(population, na.rm = TRUE)
	),
	by = year
]
global_hist[, total_des := des * population]
global_hist <- na.omit(global_hist)

#### CV
to_plot <- global_agg[,.(
	q50 = median(cv, na.rm = T),
	q05 = quantile(cv, p = 0.05, na.rm = T),
	q95 = quantile(cv, p = 0.95, na.rm = T)
	),
	.(scenario, year)]

CV <- ggplot() +
	geom_line(data = global_hist, aes(x = year, y = cv), color = "black") +
	geom_line(data = to_plot, aes(x = year, y = q50, color = scenario, group = scenario)) +
	ylab("CV") +
	scale_color_manual(values = plotting_colors, guide = "none") +
	theme_bw()

#### GDPPC
to_plot <- global_agg[,.(
	q50 = median(gdppc_non_weight, na.rm = T),
	q05 = quantile(gdppc_non_weight, p = 0.05, na.rm = T),
	q95 = quantile(gdppc_non_weight, p = 0.95, na.rm = T)
),
.(scenario, year)]

GDPPC <- ggplot() +
	geom_line(data = global_hist, aes(x = year, y = gdppc_non_weight), color = "black") +
	geom_line(data = to_plot, aes(x = year, y = q50, color = scenario, group = scenario)) +
	ylab("GDP/capita") +
	scale_color_manual(values = plotting_colors, guide = "none") +
	scale_fill_manual(values = plotting_colors, guide = "none") +
	theme_bw()

#### DES
to_plot <- global_agg[,.(
	q50 = median(des, na.rm = T),
	q05 = quantile(des, p = 0.05, na.rm = T),
	q95 = quantile(des, p = 0.95, na.rm = T)
),
.(scenario, year)]

DES <- ggplot() +
	geom_line(data = global_hist, aes(x = year, y = des), color = "black") +
	geom_line(data = to_plot, aes(x = year, y = q50, color = scenario, group = scenario)) +
	ylab("DES") +
	scale_color_manual(values = plotting_colors, guide = "none") +
	scale_fill_manual(values = plotting_colors, guide = "none") +
	theme_bw()


#### Global DES
to_plot <- global_agg[,.(
	q025 = quantile(total_des, p = 0.025, na.rm = T),
	q25 = quantile(total_des, p = 0.25, na.rm = T),
	q50 = median(total_des, na.rm = T),
	q75 = quantile(total_des, p = 0.75, na.rm = T),
	q975 = quantile(total_des, p = 0.975, na.rm = T)
	),
	.(scenario, year)]

DES_TOT <- ggplot() +
	geom_line(data = global_hist, aes(x = year, y = total_des), color = "black") +
	ggdist::geom_lineribbon(data = to_plot, aes(x = year, ymin = q25, ymax = q75, color = scenario, fill = scenario, group = scenario), alpha = 0.2) +
	ggdist::geom_lineribbon(data = to_plot, aes(x = year, ymin = q025, ymax = q975, color = scenario, fill = scenario, group = scenario), alpha = 0.2) +
	geom_line(data = to_plot, aes(x = year, y = q50, color = scenario, group = scenario)) +
	ylab("Total DES") +
	scale_color_manual(values = plotting_colors, guide = "none") +
	scale_fill_manual(values = plotting_colors, guide = "none") +
	theme_bw()

#### Number of Undernourished ####
to_plot <- global_agg[,.(
	q025 = quantile(nou, p = 0.025, na.rm = T),
	q25 = quantile(nou, p = 0.25, na.rm = T),
	q50 = median(nou, na.rm = T),
	q75 = quantile(nou, p = 0.75, na.rm = T),
	q975 = quantile(nou, p = 0.975, na.rm = T)
),
.(scenario, year)]


NOU <- ggplot() +
	geom_line(data = global_hist, aes(x = year, y = nou), color = "black") +
	ggdist::geom_lineribbon(data = to_plot, aes(x = year, ymin = q25, ymax = q75, color = scenario, fill = scenario, group = scenario), alpha = 0.2) +
	ggdist::geom_lineribbon(data = to_plot, aes(x = year, ymin = q025, ymax = q975, color = scenario, fill = scenario, group = scenario), alpha = 0.2) +
	geom_line(data = to_plot, aes(x = year, y = q50, color = scenario, group = scenario)) +
	ylab("Undernourished") +
	scale_color_manual("Scenario", values = plotting_colors) +
	scale_fill_manual("Scenario", values = plotting_colors) +
	scale_x_continuous(breaks = c(2024, 2050)) +
	theme_bw() +
	facet_wrap(~scenario, ncol = 5)

#### Prevalence of undernourishement ####

to_plot <- global_agg[,.(
	q025 = quantile(pou, p = 0.025, na.rm = T),
	q25 = quantile(pou, p = 0.25, na.rm = T),
	q50 = median(pou, na.rm = T),
	q75 = quantile(pou, p = 0.75, na.rm = T),
	q975 = quantile(pou, p = 0.975, na.rm = T)
),
.(scenario, year)]

POU <- ggplot() +
	geom_line(data = global_hist, aes(x = year, y = pou), color = "black") +
	ggdist::geom_lineribbon(data = to_plot, aes(x = year, ymin = q25, ymax = q75, color = scenario, fill = scenario, group = scenario), alpha = 0.2) +
	ggdist::geom_lineribbon(data = to_plot, aes(x = year, ymin = q025, ymax = q975, color = scenario, fill = scenario, group = scenario), alpha = 0.2) +
	geom_line(data = to_plot, aes(x = year, y = q50, color = scenario, group = scenario)) +
	ylab("PoU") +
	scale_color_manual(values = plotting_colors) +
	scale_fill_manual(values = plotting_colors) +
	theme_bw() + facet_wrap(~scenario, ncol = 5)


#### Conflict ####
to_plot <- global_agg[,.(
	q50 = median(best, na.rm = T),
	q05 = quantile(best, p = 0.05, na.rm = T),
	q95 = quantile(best, p = 0.95, na.rm = T)
	),
	.(scenario, year)]

BRD <- ggplot() +
	geom_line(data = global_hist, aes(x = year, y = best), color = "black") +
	#ggdist::geom_lineribbon(data = to_plot, aes(x = year, ymin = q05, ymax = q95, color = scenario, fill = scenario, group = scenario), alpha = 0.25) +
	geom_line(data = to_plot, aes(x = year, y = q50, color = scenario, group = scenario)) +
	ylab("BRD") +
	scale_color_manual(values = plotting_colors, guide = "none") +
	scale_fill_manual(values = plotting_colors, guide = "none") +
	theme_bw()

#### Democracy ####
to_plot <- global_agg[,.(
	q50 = median(v2x_polyarchy, na.rm = T),
	q05 = quantile(v2x_polyarchy, p = 0.05, na.rm = T),
	q95 = quantile(v2x_polyarchy, p = 0.95, na.rm = T)
),
.(scenario, year)]

EDI <- ggplot() +
	geom_line(data = global_hist, aes(x = year, y = v2x_polyarchy), color = "black") +
	#geom_lineribbon(data = to_plot, aes(x = year, ymin = q05, ymax = q95, color = scenario, fill = scenario, group = scenario), alpha = 0.25) +
	geom_line(data = to_plot, aes(x = year, y = q50, color = scenario, group = scenario)) +
	ylab("EDI") +
	scale_color_manual(values = plotting_colors, guide = "none") +
	scale_fill_manual(values = plotting_colors, guide = "none") +
	theme_bw()

#### MDER ####

to_plot <- global_agg[,
	.(q50 = median(mder, na.rm = T)),
	.(scenario, year)
]

MDER <- ggplot() +
	geom_line(data = global_hist, aes(x = year, y = mder), color = "black") +
	geom_line(data = to_plot, aes(x = year, y = q50, color = scenario, group = scenario)) +
	ylab("MDER") +
	scale_color_manual(values = plotting_colors, guide = "none") +
	theme_bw()


#### Global hunger plots (Main figure in article) ####
((MDER + CV + DES) / NOU & xlab("Year")) / POU_BOX +
	plot_layout(ncol = 1, heights = c(1, 2, 2), guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom")
ggsave(file.path("figures", simulation_alternative, "evolution_of_global_hunger.png"), device = ragg_png,  width = 12, height = 8, scale = 1.5)

#### Socio-political drivers plot ####
EDI + BRD + GDPPC + plot_layout(ncol = 1, guides = "collect") +
	plot_annotation(title = "Socio-political drivers") &
	theme_bw(base_size = 16) &
	theme(plot.title = element_text(hjust = 0.5, size = 16),
				plot.subtitle = element_text(hjust = 0.5, size = 14),
				plot.caption = element_text(hjust = 0.5, size = 10),
				legend.position = "bottom")
ggsave(file.path("figures", simulation_alternative, "socio_political_drivers.png"), device = ragg_png,  width = 10, height = 12)

#### PoU detailed projections ####
POU + NOU + DES + DES_TOT + plot_layout(ncol = 2, guides = "collect") +
	plot_annotation(title = "Global Food Security Projections") &
	theme_bw(base_size = 16) &
	theme(plot.title = element_text(hjust = 0.5, size = 16),
				plot.subtitle = element_text(hjust = 0.5, size = 14),
				plot.caption = element_text(hjust = 0.5, size = 10),
				legend.position = "bottom")
ggsave(file.path("figures", simulation_alternative, "pou_detailed_projections.png"), device = ragg_png,  width = 12, height = 12)

#### Example projections for specific countries ####
result[(gwcode %in% c(2, 475, 710)) & sim < 10] |>
	ggplot(aes(x = year, y = v2x_polyarchy, color = factor(gwcode), group = I(sim*gwcode))) +
	geom_line() + facet_wrap(~scenario, scales = "free_y") +
	#scale_color_manual(values = plotting_colors, guide = "none") +
	labs(title = "Example EDI Projections for\nUSA (2), China (710), and Nigeria (475)",
			 x = "Year",
			 y = "EDI") +
	theme_bw(base_size = 16) + theme(legend.position = "bottom")
ggsave(file.path("figures", simulation_alternative, "edi_usa_nigeria_china_example.png"), device = ragg_png,  width = 12, height = 12)


result[(gwcode %in% c(2, 475, 710)) & sim < 10] |>
	ggplot(aes(x = year, y = best, color = factor(gwcode), group = I(sim*gwcode))) +
	geom_line() + facet_wrap(~scenario, scales = "free_y") +
	#scale_y_continuous(transform = "log1p") +
	labs(title = "Example Conflict Projections for\nUSA (2), China (710), and Nigeria (475)",
			 x = "Year",
			 y = "BRD") +
	theme_bw(base_size = 16) + theme(legend.position = "bottom")
ggsave(file.path("figures", simulation_alternative, "conflict_usa_nigeria_china_example.png"), device = ragg_png,  width = 12, height = 12)


result[(gwcode %in% c(2, 475, 710)) & sim < 10] |>
	ggplot(aes(x = year, y = gdppc, color = factor(gwcode), group = I(sim*gwcode))) +
	geom_line() + facet_wrap(~scenario, scales = "free_y") +
	#scale_y_continuous(transform = "log1p") +
	labs(title = "Example GDPPC Projections for\nUSA (2), China (710), and Nigeria (475)",
			 x = "Year",
			 y = "GDPPC") +
	theme_bw(base_size = 16) + theme(legend.position = "bottom")
ggsave(file.path("figures", simulation_alternative, "gdppc_usa_nigeria_china_example.png"), device = ragg_png,  width = 12, height = 12)

#### Alternative plot layouts ####
MDER + plot_layout(ncol = 1, guides = "collect") +
	theme_bw(base_size = 16) &
	theme(plot.title = element_text(hjust = 0.5, size = 16),
				plot.subtitle = element_text(hjust = 0.5, size = 14),
				plot.caption = element_text(hjust = 0.5, size = 10),
				legend.position = "bottom") &
	scale_color_manual(values = plotting_colors)
ggsave(file.path("figures", simulation_alternative, "mder_projections.png"), device = ragg_png,  width = 12, height = 12)

CV + plot_layout(ncol = 1, guides = "collect") +
	theme_bw(base_size = 16) &
	theme(plot.title = element_text(hjust = 0.5, size = 16),
				plot.subtitle = element_text(hjust = 0.5, size = 14),
				plot.caption = element_text(hjust = 0.5, size = 10),
				legend.position = "bottom")
ggsave(file.path("figures", simulation_alternative, "cv_projections.png"), device = ragg_png,  width = 12, height = 12)


# Average number of battle-related deaths per year per scenario
result[year >= 2024,
		.(best = sum(best, na.rm = T)),
		.(scenario, sim, year)][,
														.(best = mean(best, na.rm = T)),
														.(scenario)]
