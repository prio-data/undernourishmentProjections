fitted_df <- heterolm::transform_newdata(fit1, main_df)

ngroups <- function(x) length(unique(x))
summarytable <- main_df |>
	as_tibble() |>
	dplyr::select(gwcode, year,
								des, gdppc, best, population, v2x_polyarchy,
								cv, mder, pou) |>
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
			best ~ "UCDP/PRIO battle-related deaths"
		),
		statistic = list(-all_of(c("year", "gwcode")) ~ "{median} ({min}, {max})",
										 all_of("year") ~ "{min} to {max}",
										 all_of("gwcode") ~ "n={ngroups}",
										 all_categorical() ~ "{n} ({p}%)"),
		digits = list("year" = label_style_number(digits = 0, big.mark = ""))
	) |>
	gtsummary::modify_caption("Summary statistics of dataset")

summarytable2 <- fitted_df |>
	as_tibble() |>
	dplyr::select(
		gwcode,
		year,
		.pdi1_des,
		.pl1_g_gdppc_3,
		.pl1_log_gdppc,
		.pl1_v2x_polyarchy,
		.pl1_d_v2x_polyarchy_10,
		.pl1_ms_yt01_transform_max_best_0_5,
		.pl1_yt01_transform_max_best_0,
		.pl1_g_population_5,
		.pl1_log_population,
		.pl1_tx90pgs,
		.pl1_d_tx90pgs_5
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
			.pl1_v2x_polyarchy ~ "DEM",
			.pl1_d_v2x_polyarchy_10 ~ "DEM_Δ10",
			.pl1_ms_yt01_transform_max_best_0_5 ~ "BRD_MA5",
			.pl1_yt01_transform_max_best_0 ~ "BRD",
			.pl1_g_population_5 ~ "POP_ΔP5",
			.pl1_log_population ~ "lPOP",
			.pl1_tx90pgs ~ "TX90",
			.pl1_d_tx90pgs_5 ~ "TX90_Δ5"
		),
		statistic = list(-all_of(c("year", "gwcode")) ~ "{median} ({min}, {max})",
										 all_of("year") ~ "{min} to {max}",
										 all_of("gwcode") ~ "n={ngroups}",
										 all_categorical() ~ "{n} ({p}%)"),
		digits = list("year" = label_style_number(digits = 0, big.mark = ""))
	) |>
	gtsummary::modify_caption("Summary statistics of training data for ΔDES model")

summarytable |> gtsummary::as_gt()  |> gt::gtsave(file.path("tables", simulation_alternative, "summary_table.tex"))
summarytable2 |> gtsummary::as_gt() |> gt::gtsave(file.path("tables", simulation_alternative, "summary_table_train.tex"))


