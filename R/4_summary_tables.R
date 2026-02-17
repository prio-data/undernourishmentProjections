fitted_df_des <- heterolm::transform_newdata(fit_des, main_df)
fitted_df_cv <- heterolm::transform_newdata(fit_cv, main_df)

ngroups <- function(x) length(unique(x))
summarytable <- main_df |>
	as_tibble() |>
	dplyr::select(gwcode, year,
								des, gdppc, best, population, v2x_polyarchy,
								cv, mder, pou, tx90pgs, rx5daygs) |>
	tbl_summary(
		missing_text = "Missing",
		label = list(
			gwcode ~ "GW Countrycode",
			year ~ "Year",
			gdppc ~ "GDP/capita",
			v2x_polyarchy ~ "Electoral Democracy Index",
			population ~ "Population",
			des ~ "Dietary energy supply",
			cv ~ "Coefficient of Variation (of DES)",
			mder ~ "Minimal Energy Dietary Energy Supply",
			pou ~ "Prevalence of undernourishment",
			best ~ "UCDP/PRIO battle-related deaths",
			tx90pgs ~ "Temperature extremes during growing season (share of days)",
			rx5daygs ~ "Maximum 5-day precipitation during growing season (mm)"
		),
		statistic = list(-all_of(c("year", "gwcode")) ~ "{median} ({min}, {max})",
										 all_of("year") ~ "{min} to {max}",
										 all_of("gwcode") ~ "n={ngroups}",
										 all_categorical() ~ "{n} ({p}%)"),
		digits = list("year" = label_style_number(digits = 0, big.mark = ""))
	) |>
	gtsummary::modify_caption("Summary statistics of dataset")

summarytable_des <- fitted_df_des |>
	as_tibble() |>
	dplyr::select(
		gwcode,
		year,
		.pdi1_des,
		.pl1_g_gdppc_3,
		.pl1_log_gdppc,
		.pl1_v2x_polyarchy,
		.pl1_d_v2x_polyarchy_3,
		.pl1_ms_yt01_transform_best_3,
		.pl1_yt01_transform_best,
		.pl1_g_population_3,
		.pl1_log_population,
		.pl1_tx90pgs,
		.pl1_d_tx90pgs_3
	) |>
	na.omit() |>
	tbl_summary(
		missing_text = "Missing",
		label = list(
			gwcode ~ "GW Countrycode",
			year ~ "Year",
			.pdi1_des ~ "ΔDES",
			.pl1_g_gdppc_3 ~ "GDPPC_ΔP3",
			.pl1_log_gdppc ~ "lGDPPC",
			.pl1_d_v2x_polyarchy_3 ~ "DEM_Δ3",
			.pl1_v2x_polyarchy ~ "DEM",
			.pl1_ms_yt01_transform_best_3 ~ "BRD_MA3",
			.pl1_yt01_transform_best ~ "BRD",
			.pl1_g_population_3 ~ "POP_ΔP3",
			.pl1_log_population ~ "lPOP",
			.pl1_d_tx90pgs_3 ~ "TX90_Δ3",
			.pl1_tx90pgs ~ "TX90"
		),
		statistic = list(-all_of(c("year", "gwcode")) ~ "{median} ({min}, {max})",
										 all_of("year") ~ "{min} to {max}",
										 all_of("gwcode") ~ "n={ngroups}",
										 all_categorical() ~ "{n} ({p}%)"),
		digits = list("year" = label_style_number(digits = 0, big.mark = ""))
	) |>
	gtsummary::modify_caption("Summary statistics of training data for ΔDES model")

cplot_fit_cv_mean <- coefplot(fit_cv, which = "mean") +
	scale_x_discrete(labels = c(".pl1_I_best_1000" = "BRD>1000",
															".pl1_g_gdppc_3" = "GDPPC_∆P3",
															".pl1_d_tx90pgs_3" = "TX90_∆3")) +
	ylab("Std. Coefficient Estimate") + ggtitle("E(∆CV)")

cplot_fit_cv_variance <- coefplot(fit_cv, which = "variance") +
	scale_x_discrete(labels = c(".pl1_log_gdppc" = "lGDPPC",
															".pl1_v2x_polyarchy" = "DEM",
															".pl1_I_v2x_polyarchy_2" = "DEMsq")) +
	ylab("Std. Coefficient Estimate") + ggtitle("V(∆CV)")

summarytable_cv <- fitted_df_cv |>
	as_tibble() |>
	dplyr::select(
		gwcode,
		year,
		.pdi1_cv,
		.pl1_I_best_1000,
		.pl1_g_gdppc_3,
		.pl1_d_tx90pgs_3,
		.pl1_log_gdppc,
		.pl1_v2x_polyarchy,
		.pl1_I_v2x_polyarchy_2
	) |>
	na.omit() |>
	tbl_summary(
		missing_text = "Missing",
		label = list(
			gwcode ~ "GW Countrycode",
			year ~ "Year",
			.pdi1_cv ~ "ΔCV",
			.pl1_I_best_1000 ~ "BRD>1000",
			.pl1_g_gdppc_3 ~ "GDPPC_ΔP3",
			.pl1_d_tx90pgs_3 ~ "TX90_Δ3",
			.pl1_log_gdppc ~ "lGDPPC",
			.pl1_v2x_polyarchy ~ "DEM",
			.pl1_I_v2x_polyarchy_2 ~ "DEMsq"
		),
		statistic = list(-all_of(c("year", "gwcode")) ~ "{median} ({min}, {max})",
										 all_of("year") ~ "{min} to {max}",
										 all_of("gwcode") ~ "n={ngroups}",
										 all_categorical() ~ "{n} ({p}%)"),
		digits = list("year" = label_style_number(digits = 0, big.mark = ""))
	) |>
	gtsummary::modify_caption("Summary statistics of training data for ΔCV model")

summarytable |> gtsummary::as_gt()  |> gt::gtsave(file.path("tables", simulation_alternative, "summary_table.tex"), label = "tab:summary_table")
summarytable_des |> gtsummary::as_gt() |> gt::gtsave(file.path("tables", simulation_alternative, "summary_table_des.tex"), label = "tab:summary_table_des")
summarytable_cv |> gtsummary::as_gt() |> gt::gtsave(file.path("tables", simulation_alternative, "summary_table_cv.tex"), label = "tab:summary_table_cv")

A <- ggplot(main_df, aes(x = cv, y = des)) + geom_point(shape = 20, alpha = 0.3) + geom_smooth(method = "lm", se = TRUE) +
	theme_bw(base_size = 24) + ylab("DES") + xlab("CV")
ggsave(file.path("figures", simulation_alternative, "cv_against_des.png"), A, device = ragg_png, scale = 1.5, width = 6, height = 6)
B <- ggplot(main_df, aes(x = year, y = des)) + geom_point(shape = 20, alpha = 0.3) + geom_smooth(se = TRUE) +
	theme_bw(base_size = 24) + ylab("DES") + xlab("Year")
ggsave(file.path("figures", simulation_alternative, "des_against_year.png"), B, device = ragg_png, scale = 1.5, width = 6, height = 6)
C <- ggplot(main_df[year >=2000], aes(x = year, y = cv)) + geom_point(shape = 20, alpha = 0.3) + geom_smooth(se = TRUE) +
	theme_bw(base_size = 24) + ylab("CV") + xlab("Year")
ggsave(file.path("figures", simulation_alternative, "cv_against_year.png"), C, device = ragg_png, scale = 1.5, width = 6, height = 6)

A + B + C
ggsave(file.path("figures", simulation_alternative, "cv_des_year.png"), device = ragg_png, scale = 1.5, width = 12, height = 5)

nlag <- 1
# Plot continuous conflict instead of the binary data
f_cv_to_plot <- pdiff(cv, nlag) ~
	plag(best, nlag) +
	plag(pgrowth(gdppc, 3), nlag) +
	plag(pdiff(tx90pgs, 3), nlag) |
	plag(log(gdppc), nlag) +
	plag(v2x_polyarchy, nlag) +
	plag(I(v2x_polyarchy^2), nlag)

fit_cv_to_plot <- hetero(f_cv_to_plot, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fitted_df_cv_to_plot <- heterolm::transform_newdata(fit_cv_to_plot, main_df)

A <- ggplot(fitted_df_cv_to_plot, aes(x= .pl1_best, y = .pdi1_cv)) +
	geom_point() +
	geom_smooth(method = "gam", se = TRUE) +
	scale_x_continuous("BRD", transform = "log1p", breaks = c(0, 1, 10, 100, 1000, 10000, 100000), limits = c(0, 100000), labels = scales::comma) +
	ylab("Food Inequality, ΔCV") +
	theme_bw(base_size = 20)

B <- ggplot(fitted_df_cv_to_plot, aes(x= .pl1_g_gdppc_3, y = .pdi1_cv)) +
	geom_point() +
	geom_smooth(method = "gam", se = TRUE) +
	scale_x_continuous("GDPPC_ΔP3", labels = scales::comma) +
	ylab("Food Inequality, ΔCV") +
	theme_bw(base_size = 20)

C <- ggplot(fitted_df_cv_to_plot, aes(x= .pl1_v2x_polyarchy, y = .pdi1_cv)) +
	geom_point() +
	geom_smooth(method = "loess", se = TRUE) +
	scale_x_continuous("DEM", labels = scales::comma) +
	ylab("Food Inequality, ΔCV") +
	theme_bw(base_size = 20)

D <- ggplot(fitted_df_cv_to_plot, aes(x= .pl1_d_tx90pgs_3, y = .pdi1_cv)) +
	geom_point() +
	geom_smooth(method = "loess", se = TRUE) +
	scale_x_continuous("TX90_Δ3", labels = scales::comma) +
	ylab("Food Inequality, ΔCV") +
	theme_bw(base_size = 20)

A + B + C + D + plot_layout(axes = "collect")

ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
ggsave(file.path("figures", simulation_alternative, "cv_against_covariates.png"), device = ragg_png, width = 12, height = 12)


#f_base but without transforming conflict (for plotting)
f_base_to_plot <- pdiff(des, 1) ~
	plag(best, 1) +
	plag(pdiff(tx90pgs, 3), 1) +
	plag(pdiff(rx5daygs, 3), 1) +
	plag(pgrowth(gdppc, 3), 1) +
	plag(pdiff(v2x_polyarchy, 3), 1) +
	plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
	plag(pgrowth(population, 3), 1) |
	plag(yt01$transform(best), 1) +
	plag(tx90pgs, 1) +
	plag(rx5daygs, 1) +
	plag(log(gdppc), 1) +
	plag(v2x_polyarchy, 1) +
	plag(I(v2x_polyarchy^2), 1) +
	plag(log(population), 1)

fit_des_to_plot <- hetero(f_base_to_plot, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fitted_df_ds_to_plot <- heterolm::transform_newdata(fit_des_to_plot, main_df)

A <- ggplot(fitted_df_ds_to_plot, aes(x= .pl1_best, y = .pdi1_des)) +
	geom_point() +
	geom_smooth(method = "gam", se = TRUE) +
	scale_x_continuous("BRD", transform = "log1p", breaks = c(0, 1, 10, 100, 1000, 10000, 100000), limits = c(0, 100000), labels = scales::comma) +
	ylab("ΔDES") +
	theme_bw(base_size = 20)

B <- ggplot(fitted_df_ds_to_plot, aes(x= .pl1_g_gdppc_3, y = .pdi1_des)) +
	geom_point() +
	geom_smooth(method = "gam", se = TRUE) +
	scale_x_continuous("GDPPC_ΔP3", labels = scales::comma) +
	ylab("ΔDES") +
	theme_bw(base_size = 20)

C <- ggplot(fitted_df_ds_to_plot, aes(x= .pl1_d_v2x_polyarchy_3, y = .pdi1_des)) +
	geom_point() +
	geom_smooth(method = "gam", se = TRUE) +
	scale_x_continuous("DEM_Δ3", labels = scales::comma) +
	ylab("ΔDES") +
	theme_bw(base_size = 20)

D <- ggplot(fitted_df_ds_to_plot, aes(x= .pl1_d_tx90pgs_3, y = .pdi1_des)) +
	geom_point() +
	geom_smooth(method = "gam", se = TRUE) +
	scale_x_continuous("TX90_Δ3", labels = scales::comma) +
	ylab("ΔDES") +
	theme_bw(base_size = 20)

A + B + C + D + plot_layout(axes = "collect")
ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
ggsave(file.path("figures", simulation_alternative, "des_against_covariates.png"), device = ragg_png, width = 12, height = 12)


to_plot <- main_df[gwcode %in% c(2, 372, 530, 750), c("gwcode", "year", "cv", "des", "best")]
to_plot$cname <- case_when(to_plot$gwcode == 2 ~ "USA",
													 to_plot$gwcode == 372 ~ "Georgia",
													 to_plot$gwcode == 530 ~ "Ethiopia",
													 to_plot$gwcode == 750 ~ "India")

A <- ggplot(to_plot[year >= 2000], aes(x = year, y = cv)) +
	geom_line() +
	facet_wrap(~cname) +
	ylab("CV") + xlab("Year") +
	theme_bw(base_size = 24)

B <- ggplot(to_plot, aes(x = year, y = des)) +
	geom_line() +
	facet_wrap(~cname) +
	ylab("DES") + xlab("Year") +
	theme_bw(base_size = 24)

C <- ggplot(to_plot, aes(x = year, y = best)) +
	geom_line() +
	scale_y_continuous("BRD", transform = "log1p", breaks = c(0, 1, 10, 100, 1000, 10000, 100000), limits = c(0, 100000)) +
	facet_wrap(~cname) +
	ylab("BRD") + xlab("Year") +
	theme_bw(base_size = 24)

A + B + C + patchwork::plot_layout(axes = "collect")
ggsave(file.path("figures", "historical_des_cv_selected_countries.png"), device = ragg_png, width = 12, height = 6, scale = 1.5)

to_plot <- main_df[gwcode %in% c(100, 432, 700, 850), c("gwcode", "year", "cv", "des", "best")]
to_plot$cname <- case_when(to_plot$gwcode == 700 ~ "Afghanistan",
													 to_plot$gwcode == 432 ~ "Mali",
													 to_plot$gwcode == 850 ~ "Indonesia",
													 to_plot$gwcode == 100 ~ "Colombia")

A <- ggplot(to_plot[year >= 2000], aes(x = year, y = cv)) +
	geom_line() +
	facet_wrap(~cname) +
	ylab("CV") + xlab("Year") +
	theme_bw(base_size = 24)

B <- ggplot(to_plot, aes(x = year, y = des)) +
	geom_line() +
	facet_wrap(~cname) +
	ylab("DES") + xlab("Year") +
	theme_bw(base_size = 24)

C <- ggplot(to_plot, aes(x = year, y = best)) +
	geom_line() +
	scale_y_continuous("BRD", transform = "log1p", breaks = c(0, 1, 10, 100, 1000, 10000, 100000), limits = c(0, 100000)) +
	facet_wrap(~cname) +
	ylab("BRD") + xlab("Year") +
	theme_bw(base_size = 24)

A + B + C + patchwork::plot_layout(axes = "collect")
ggsave(file.path("figures", "historical_des_cv_selected_countries2.png"), device = ragg_png, width = 12, height = 6, scale = 1.5)
