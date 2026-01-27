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
