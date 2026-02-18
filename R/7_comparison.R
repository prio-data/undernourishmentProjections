

global_agg_fnames_a <- file.path("results", simulation_alternatives, "regression", "global_agg.rds")
global_agg_fnames_b <- file.path("results", simulation_alternatives, "manual", "global_agg.rds")
global_agg_fnames_c <- file.path("results", paste0("timeint", c(3, 5)), "base", "regression", "global_agg.rds")
global_agg <- lapply(c(global_agg_fnames_a, global_agg_fnames_b, global_agg_fnames_c), readRDS)
approach_names <- c(paste(simulation_alternatives, "regression", sep = "-"), paste(simulation_alternatives, "manual", sep = "-"), paste0("timeint", c(3, 5)))
names(global_agg) <- approach_names
global_agg <- dplyr::bind_rows(global_agg, .id = "approach")

global_hist <- readRDS("results/global_hist.rds")

# Aggregate across simulations
cols <- c("nou", "pou", "des", "mder", "cv", "best", "v2x_polyarchy", "gdppc", "population", "tx90pgs")
to_table <- global_agg[, lapply(.SD, median, na.rm = TRUE),
					 by = .(approach, scenario, year),
					 .SDcols = cols]
setnames(to_table, c(
	"approach"	    = "approach",
	"scenario"      = "scenario",
	"year"          = "Year",
	"nou"           = "NUN",
	"pou"           = "PoU",
	"des"           = "DES",
	"mder"          = "MDER",
	"cv"            = "CV",
	"best"          = "BRD",
	"v2x_polyarchy" = "DEM",
	"gdppc"         = "GDPPC",
	"population"    = "POP",
	"tx90pgs"       = "TX90"
))

for(a in approach_names){
	tmp <- to_table[approach == a]
	tmp <- split(tmp, ~scenario)
	for(i in 1:5){
		tmp[[i]] |> ungroup() |> dplyr::select(-scenario, -approach)  |>
			gt::gt() |>
			gt::fmt_number(c("NUN", "DES", "MDER", "BRD", "GDPPC", "POP"), decimals = 0) |>
			gt::fmt_number(c("PoU", "CV"), decimals = 4) |>
			gt::fmt_number(c("DEM", "TX90"), decimals = 2) |>
			gt::tab_header(paste("Median global population-weighted results,", names(tmp)[i], "Approach:", a)) |>
			gt::gtsave(file.path("tables", paste0("main_pou_", a, "_", names(tmp)[i], ".tex")))
	}
}

summarize_quantiles <- function(dt, var, by_cols = c("approach", "scenario", "year")) {
	dt[, list(
		variable = var,
		q025 = quantile(get(var), 0.025, na.rm = TRUE),
		q25  = quantile(get(var), 0.25, na.rm = TRUE),
		q50  = median(get(var), na.rm = TRUE),
		q75  = quantile(get(var), 0.75, na.rm = TRUE),
		q975 = quantile(get(var), 0.975, na.rm = TRUE)
	), by = by_cols][, s_a := paste0(scenario, "_", approach)]
}

# Combine multiple variables
plots_data <- rbindlist(lapply(c("gdppc", "pou", "des", "nou", "cv", "cv_non_weight", "tx90pgs", "rx5daygs"), function(v) {
	summarize_quantiles(global_agg, v)
}))


plots_data[year == 2050 & variable == "pou", -c("s_a", "variable")] |>
	gt::gt() |>
	gt::fmt_number(decimals = 4) |>
	gt::fmt_number("year", decimals = 0, sep_mark = "") |>
	gt::tab_header("PoU quantiles in 2050 from population-weighted results") |>
	gt::gtsave(paste0("tables/pou_quantiles_2050.tex"))

plots_data[year == 2050 & variable == "nou", -c("s_a", "variable")] |>
	gt::gt() |>
	gt::fmt_number(decimals = 0) |>
	gt::fmt_number("year", decimals = 0, sep_mark = "") |>
	gt::tab_header("Undernourished quantiles in 2050 from population-weighted results") |>
	gt::gtsave(paste0("tables/nou_quantiles_2050.tex"))

base_plot <- function(varname, prettyname, plots_data) {
	ggplot() +
	geom_line(data = global_hist, aes(x = year, y = .data[[varname]]), color = "black") +
	geom_lineribbon(data = plots_data[variable == varname], aes(x = year, ymin = q025, ymax = q975, color = scenario, fill = scenario, group = s_a), alpha = 0.2) +
	geom_line(data = plots_data[variable == varname], aes(x = year, y = q50, color = scenario, linetype = approach, group = s_a)) +
	ylab(prettyname) +
	scale_color_manual("Scenario", values = plotting_colors) +
	scale_fill_manual("Scenario", values = plotting_colors) +
	facet_wrap(~scenario, ncol = 5) +
	theme_bw()
}

plot_without_historic <- function(varname, prettyname, plots_data) {
	ggplot() +
		geom_lineribbon(data = plots_data[variable == varname], aes(x = year, ymin = q025, ymax = q975, color = scenario, fill = scenario, group = s_a), alpha = 0.2) +
		geom_line(data = plots_data[variable == varname], aes(x = year, y = q50, color = scenario, linetype = approach, group = s_a)) +
		ylab(prettyname) +
		scale_color_manual("Scenario", values = plotting_colors) +
		scale_fill_manual("Scenario", values = plotting_colors) +
		facet_wrap(~scenario, ncol = 5) +
		theme_bw()
}

POU <- base_plot("pou", "PoU", plots_data)
NOU <- base_plot("nou", "Under-\nnourished", plots_data)
DES <- base_plot("des", "DES", plots_data)
CV <- base_plot("cv", "CV", plots_data)
GDPPC <- base_plot("gdppc", "GDPPC", plots_data)
TX90 <- base_plot("tx90pgs", "TX90", plots_data)
RX5DAY <- base_plot("rx5daygs", "RX5DAY", plots_data)

GDPPC / CV / DES / POU / NOU / TX90 / RX5DAY + plot_layout(ncol = 1, guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom",
				legend.box = "vertical") &
	scale_color_manual("Scenario", values = plotting_colors) &
	scale_fill_manual("Scenario", values = plotting_colors) &
	scale_linetype_discrete("Approach") &
	scale_x_continuous(breaks = c(2024, 2050))
ggsave("figures/approach_comparison.png", device = ragg_png,  width = 12, height = 16, scale = 1.5)

POU <- plot_without_historic("pou", "PoU", plots_data)
NOU <- plot_without_historic("nou", "Under-\nnourished", plots_data)
DES <- plot_without_historic("des", "DES", plots_data)
CV <- plot_without_historic("cv", "CV", plots_data)
GDPPC <- plot_without_historic("gdppc", "GDPPC", plots_data)

GDPPC / CV / DES / POU / NOU / TX90 / RX5DAY + plot_layout(ncol = 1, guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom",
				legend.box = "vertical") &
	scale_color_manual("Scenario", values = plotting_colors) &
	scale_fill_manual("Scenario", values = plotting_colors) &
	scale_linetype_discrete("Approach") &
	scale_x_continuous(breaks = c(2024, 2050))
ggsave("figures/approach_comparison_without_historic.png", device = ragg_png,  width = 12, height = 16, scale = 1.5)



#### Only the differences ####
summarize_diff <- function(dt, var, by_cols = c("scenario", "year"), baseline = "base-regression") {
	# Compute quantiles by approach
	agg <- dt[, .(
		q025 = quantile(get(var), 0.025, na.rm = TRUE),
		q50  = median(get(var), na.rm = TRUE),
		q975 = quantile(get(var), 0.975, na.rm = TRUE)
	), by = c(by_cols, "approach")]

	# Split baseline and others
	base_agg <- agg[approach == baseline]
	other_agg <- agg[approach != baseline]

	# Merge and compute differences
	merged <- merge(
		other_agg,
		base_agg[, .(scenario, year, q025_base = q025, q50_base = q50, q975_base = q975)],
		by = by_cols
	)

	merged[, `:=`(
		q50d  = q50_base - q50,
		q025d = q025_base - q025,
		q975d = q975_base - q975
	)]

	merged
}

# Plot function - now facet by approach
diff_plot <- function(varname, prettyname, model_approach) {
	to_plot <- diff_data[variable == varname & approach == model_approach]
	ggplot() +
		geom_line(data = to_plot, aes(x = year, y = q50d, color = scenario, group = scenario)) +
		ylab(paste0("Δ", prettyname)) +
		scale_color_manual(values = plotting_colors)
}

# Combine multiple variables
diff_data <- rbindlist(lapply(c("gdppc", "pou", "des", "nou", "cv", "tx90pgs"), function(v) {
	summarize_diff(global_agg, v)[, variable := v]
}))


POU <- diff_plot("pou", "PoU", "no_conflict_effect-regression")
NOU <- diff_plot("nou", "Under-\nnourished", "no_conflict_effect-regression")
GDPPC <- diff_plot("gdppc", "GDPPC", "no_conflict_effect-regression")
DES <- diff_plot("des", "DES", "no_conflict_effect-regression")
CV <- diff_plot("cv", "CV", "no_conflict_effect-regression")

layout <- "
CCAA
CCBB
"

DES + POU + NOU + plot_layout(design = layout, guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom") &
	scale_color_manual("Scenario", values = plotting_colors)
ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
ggsave("figures/approach_comparison_no_conflict_diff.png", device = ragg_png,  width = 12, height = 4, scale = 1.5)


layout <- "
EEEEAABB
EEEECCDD
"
NOU + CV + DES + GDPPC + POU + plot_layout(design = layout, guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom") &
	scale_color_manual("Scenario", values = plotting_colors)
ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
ggsave("figures/approach_comparison_no_conflict_diff_expanded.png", device = ragg_png,  width = 12, height = 4, scale = 1.5)


POU <- diff_plot("pou", "PoU", "constant_democracy-regression")
NOU <- diff_plot("nou", "Under-\nnourished", "constant_democracy-regression")
GDPPC <- diff_plot("gdppc", "GDPPC", "constant_democracy-regression")
DES <- diff_plot("des", "DES", "constant_democracy-regression")
CV <- diff_plot("cv", "CV", "constant_democracy-regression")

layout <- "
CCAA
CCBB
"

DES + POU + NOU + plot_layout(design = layout, guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom") &
	scale_color_manual("Scenario", values = plotting_colors)
ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
ggsave("figures/approach_comparison_constant_democracy_diff.png", device = ragg_png,  width = 12, height = 4, scale = 1.5)

POU <- diff_plot("pou", "PoU", "constant_climate-regression")
NOU <- diff_plot("nou", "Under-\nnourished", "constant_climate-regression")
GDPPC <- diff_plot("gdppc", "GDPPC", "constant_climate-regression")
DES <- diff_plot("des", "DES", "constant_climate-regression")
CV <- diff_plot("cv", "CV", "constant_climate-regression")

layout <- "
CCAA
CCBB
"

DES + POU + NOU + plot_layout(design = layout, guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom") &
	scale_color_manual("Scenario", values = plotting_colors)
ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
ggsave("figures/approach_comparison_constant_climate_diff.png", device = ragg_png,  width = 12, height = 4, scale = 1.5)


global_agg_fnames_cv <- file.path("results", "base", cv_approaches, "global_agg.rds")
global_agg_cv <- lapply(global_agg_fnames_cv, readRDS)
names(global_agg_cv) <- cv_approaches
global_agg_cv <- dplyr::bind_rows(global_agg_cv, .id = "approach")

plots_data_cv <- rbindlist(lapply(c("gdppc", "pou", "des", "nou", "cv", "cv_non_weight", "tx90pgs", "rx5daygs"), function(v) {
	summarize_quantiles(global_agg_cv, v)
}))

POU <- base_plot("pou", "PoU", plots_data_cv)
NOU <- base_plot("nou", "Under-\nnourished", plots_data_cv)
DES <- base_plot("des", "DES", plots_data_cv)
CV <- base_plot("cv", "CV", plots_data_cv)
GDPPC <- base_plot("gdppc", "GDPPC", plots_data_cv)
TX90 <- base_plot("tx90pgs", "TX90", plots_data_cv)

CV / POU / NOU + plot_layout(ncol = 1, guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom",
				legend.box = "vertical") &
	scale_color_manual("Scenario", values = plotting_colors) &
	scale_fill_manual("Scenario", values = plotting_colors) &
	scale_linetype_discrete("Approach") &
	scale_x_continuous(breaks = c(2024, 2050))
ggsave("figures/approach_comparison_manual_cv.png", device = ragg_png,  width = 12, height = 12, scale = 1.5)



country_agg_fnames <- file.path("results", c("base", "no_conflict_effect"), "regression", "country_agg.rds")
country_agg <- lapply(country_agg_fnames, readRDS)
names(country_agg) <- c("base", "no_conflict_effect")
country_agg <- dplyr::bind_rows(country_agg, .id = "approach")

wide_dt <- dcast(country_agg[year == 2050],
							scenario + gwcode ~ approach,
							value.var = "pou")
wide_dt[, diff := base - no_conflict_effect]
map <- cshapes::cshp(date = as.Date("2019-01-01"))

ssp1_map <- left_join(map, wide_dt[scenario == "SSP1-2.6"], by = "gwcode") |> dplyr::mutate(scenario = "SSP1-2.6")
ssp2_map <- left_join(map, wide_dt[scenario == "SSP2-4.5"], by = "gwcode") |> dplyr::mutate(scenario = "SSP2-4.5")
ssp3_map <- left_join(map, wide_dt[scenario == "SSP3-7.0"], by = "gwcode") |> dplyr::mutate(scenario = "SSP3-7.0")
ssp4_map <- left_join(map, wide_dt[scenario == "SSP4-7.0"], by = "gwcode") |> dplyr::mutate(scenario = "SSP4-7.0")
ssp5_map <- left_join(map, wide_dt[scenario == "SSP5-8.5"], by = "gwcode") |> dplyr::mutate(scenario = "SSP5-8.5")
maps <- bind_rows(ssp1_map, ssp2_map, ssp3_map, ssp4_map, ssp5_map)
maps$diff |> summary()
A <- ggplot(ssp1_map) +
	geom_sf(aes(fill = diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.013, 0.12)) +
	theme_bw(base_size = 24) + labs(subtitle = "SSP1-2.6")
B <- ggplot(ssp2_map) +
	geom_sf(aes(fill = diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.013, 0.12)) +
	theme_bw(base_size = 24) + labs(subtitle = "SSP2.4-5")
C <- ggplot(ssp3_map) +
	geom_sf(aes(fill = diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.013, 0.12)) +
	theme_bw(base_size = 24) + labs(subtitle = "SSP3-7.0")
D <- ggplot(ssp4_map) +
	geom_sf(aes(fill = diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.013, 0.12)) +
	theme_bw(base_size = 24) + labs(subtitle = "SSP4-7.0")
E <- ggplot(ssp5_map) +
	geom_sf(aes(fill = diff)) +
	scale_fill_viridis_c("ΔPoU", option = "turbo", limits = c(-0.013, 0.12)) +
	theme_bw(base_size = 24) + labs(subtitle = "SSP5-8.5")

MAP_DIFF_POU <- A + B + C + D + E + plot_layout(guides = "collect") + plot_annotation(caption = "Base - No conf approach, 2050") &
	theme_bw(base_size = 24)
ggsave("figures/pou_base_vs_noconf_map.png", MAP_DIFF_POU, device = ragg_png,  width = 12, height = 5, scale = 1.5)


#### Compare time aggregations ####
time_agg_fnames <- file.path("results", paste0("timeint", c(3, 5)) , "base", "regression", "global_agg.rds")
time_agg_fnames <- c(time_agg_fnames, file.path("results", "base", "regression", "global_agg.rds"))
time_agg <- lapply(time_agg_fnames, readRDS)
names(time_agg) <- c(paste0("timeint", c(3, 5)), "timeint1")
time_agg <- dplyr::bind_rows(time_agg, .id = "approach")

time_plots_data <- rbindlist(lapply(c("gdppc", "pou", "des", "nou", "cv", "cv_non_weight", "tx90pgs", "rx5daygs"), function(v) {
	summarize_quantiles(time_agg, v)
}))


POU <- base_plot("pou", "PoU", time_plots_data)
NOU <- base_plot("nou", "Under-\nnourished", time_plots_data)
DES <- base_plot("des", "DES", time_plots_data)
CV <- base_plot("cv", "CV", time_plots_data)
GDPPC <- base_plot("gdppc", "GDPPC", time_plots_data)
TX90 <- base_plot("tx90pgs", "TX90", time_plots_data)

CV / DES / POU / NOU + plot_layout(ncol = 1, guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom",
				legend.box = "vertical") &
	scale_color_manual("Scenario", values = plotting_colors) &
	scale_fill_manual("Scenario", values = plotting_colors) &
	scale_linetype_discrete("Approach") &
	scale_x_continuous(breaks = c(2024, 2050))
ggsave("figures/time_approach_comparison.png", device = ragg_png,  width = 12, height = 12, scale = 1.5)

