

global_agg_fnames <- file.path("results", simulation_alternatives, "regression", "global_agg.rds")
global_agg <- lapply(global_agg_fnames, readRDS)
names(global_agg) <- simulation_alternatives
global_agg <- dplyr::bind_rows(global_agg, .id = "approach")

global_hist <- readRDS("results/global_hist.rds")

# Aggregate across simulations
cols <- c("nou", "pou", "des", "mder", "cv", "best", "v2x_polyarchy", "gdppc", "population", "tx90pgs", "rx5daygs")
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
	"tx90pgs"       = "TX90",
	"rx5daygs"      = "RX5DAY"
))

for(simulation_alternative in simulation_alternatives){
	tmp <- to_table[approach == simulation_alternative]
	tmp <- split(tmp, ~scenario)
	for(i in 1:5){
		tmp[[i]] |> ungroup() |> dplyr::select(-scenario, -approach)  |>
			gt::gt() |>
			gt::fmt_number(c("NUN", "DES", "MDER", "BRD", "GDPPC", "POP"), decimals = 0) |>
			gt::fmt_number(c("PoU", "CV", "DEM", "TX90", "RX5DAY"), decimals = 2) |>
			gt::tab_header(paste("Median global population-weighted results,", names(tmp)[i], "Approach:", simulation_alternative)) |>
			gt::gtsave(file.path("tables", simulation_alternative, paste0("main_pou_", names(tmp[i]), ".tex")))
	}
}

summarize_quantiles <- function(dt, var, by_cols = c("approach", "scenario", "year")) {
	dt[, .(
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

base_plot <- function(varname, prettyname) {
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

plot_without_historic <- function(varname, prettyname) {
	ggplot() +
		geom_lineribbon(data = plots_data[variable == varname], aes(x = year, ymin = q025, ymax = q975, color = scenario, fill = scenario, group = s_a), alpha = 0.2) +
		geom_line(data = plots_data[variable == varname], aes(x = year, y = q50, color = scenario, linetype = approach, group = s_a)) +
		ylab(prettyname) +
		scale_color_manual("Scenario", values = plotting_colors) +
		scale_fill_manual("Scenario", values = plotting_colors) +
		facet_wrap(~scenario, ncol = 5) +
		theme_bw()
}

POU <- base_plot("pou", "PoU")
NOU <- base_plot("nou", "Under-\nnourished")
DES <- base_plot("des", "DES")
CV <- base_plot("cv", "CV")
GDPPC <- base_plot("gdppc", "GDPPC")
TX90 <- base_plot("tx90pgs", "TX90")
RX5DAY <- base_plot("rx5daygs", "RX5DAY")

GDPPC / CV / DES / POU / NOU / TX90 / RX5DAY + plot_layout(ncol = 1, guides = "collect", axes = "collect") &
	theme_bw(base_size = 24) &
	theme(legend.position = "bottom",
				legend.box = "vertical") &
	scale_color_manual("Scenario", values = plotting_colors) &
	scale_fill_manual("Scenario", values = plotting_colors) &
	scale_linetype_discrete("Approach") &
	scale_x_continuous(breaks = c(2024, 2050))
ggsave("figures/approach_comparison.png", device = ragg_png,  width = 12, height = 16, scale = 1.5)

POU <- plot_without_historic("pou", "PoU")
NOU <- plot_without_historic("nou", "Under-\nnourished")
DES <- plot_without_historic("des", "DES")
CV <- plot_without_historic("cv", "CV")
GDPPC <- plot_without_historic("gdppc", "GDPPC")

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
summarize_diff <- function(dt, var, by_cols = c("scenario", "year"), baseline = "base") {
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
		by = c("scenario", "year")
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

#diff_data <- diff_data[approach %in% c("base", "no_conflict_effect")]


POU <- diff_plot("pou", "PoU", "no_conflict_effect")
NOU <- diff_plot("nou", "Under-\nnourished", "no_conflict_effect")
GDPPC <- diff_plot("gdppc", "GDPPC", "no_conflict_effect")
DES <- diff_plot("des", "DES", "no_conflict_effect")
CV <- diff_plot("cv", "CV", "no_conflict_effect")

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


POU <- diff_plot("pou", "PoU", "constant_democracy")
NOU <- diff_plot("nou", "Under-\nnourished", "constant_democracy")
GDPPC <- diff_plot("gdppc", "GDPPC", "constant_democracy")
DES <- diff_plot("des", "DES", "constant_democracy")
CV <- diff_plot("cv", "CV", "constant_democracy")

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

POU <- diff_plot("pou", "PoU", "constant_climate")
NOU <- diff_plot("nou", "Under-\nnourished", "constant_climate")
GDPPC <- diff_plot("gdppc", "GDPPC", "constant_climate")
DES <- diff_plot("des", "DES", "constant_climate")
CV <- diff_plot("cv", "CV", "constant_climate")

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
