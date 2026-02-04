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
		.pl1_rx5daygs,
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
			.pl1_tx90pgs ~ "TX90",
			.pl1_rx5daygs ~ "RX5DAY_Δ3",
			.pl1_rx5daygs ~ "RX5DAY"
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
			.pdi1_cv = "∆CV",
			.pl1_I_best_1000 = "BRD>1000",
			.pl1_g_gdppc_3 = "GDPPC_∆P3",
			.pl1_d_tx90pgs_3 = "TX90_∆3",
			.pl1_log_gdppc = "lGDPPC",
			.pl1_v2x_polyarchy = "DEM",
			.pl1_I_v2x_polyarchy_2 = "DEMsq"
		),
		statistic = list(-all_of(c("year", "gwcode")) ~ "{median} ({min}, {max})",
										 all_of("year") ~ "{min} to {max}",
										 all_of("gwcode") ~ "n={ngroups}",
										 all_categorical() ~ "{n} ({p}%)"),
		digits = list("year" = label_style_number(digits = 0, big.mark = ""))
	) |>
	gtsummary::modify_caption("Summary statistics of training data for ΔCV model")

summarytable |> gtsummary::as_gt()  |> gt::gtsave(file.path("tables", simulation_alternative, "summary_table.tex"))
summarytable_des |> gtsummary::as_gt() |> gt::gtsave(file.path("tables", simulation_alternative, "summary_table_des.tex"))
summarytable_cv |> gtsummary::as_gt() |> gt::gtsave(file.path("tables", simulation_alternative, "summary_table_cv.tex"))


