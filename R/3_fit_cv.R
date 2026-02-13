if(TIME_INTERVAL != 1){
	if(simulation_alternative == "no_conflict_effect"){
		f_cv <- pdiff(cv, 1) ~
			plag(pgrowth(gdppc, 1), 1) +
			plag(pdiff(tx90pgs, 1), 1) |
			plag(log(gdppc), 1) +
			plag(v2x_polyarchy, 1) +
			plag(I(v2x_polyarchy^2), 1)
	} else{
		f_cv <- pdiff(cv, 1) ~
			plag(I(best>1000), 1) +
			plag(pgrowth(gdppc, 1), 1) +
			plag(pdiff(tx90pgs, 1), 1) |
			plag(log(gdppc), 1) +
			plag(v2x_polyarchy, 1) +
			plag(I(v2x_polyarchy^2), 1)
	}
} else{
	if(simulation_alternative == "no_conflict_effect"){
		f_cv <- pdiff(cv, 1) ~
			plag(pgrowth(gdppc, 3), 1) +
			plag(pdiff(tx90pgs, 3), 1) |
			plag(log(gdppc), 1) +
			plag(v2x_polyarchy, 1) +
			plag(I(v2x_polyarchy^2), 1)
	} else{
		f_cv <- pdiff(cv, 1) ~
			plag(I(best>1000), 1) +
			plag(pgrowth(gdppc, 3), 1) +
			plag(pdiff(tx90pgs, 3), 1) |
			plag(log(gdppc), 1) +
			plag(v2x_polyarchy, 1) +
			plag(I(v2x_polyarchy^2), 1)
	}
}

fit_cv <- hetero(f_cv, data = main_df, panel.id = ~ gwcode + period, method = "nlm")

# Just do this once
if(simulation_alternative == "base" & cv_approach == "regression" & TIME_INTERVAL == 1){
	f1 <- pdiff(cv, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(yt01$transform(best), 1)
	f2 <- pdiff(cv, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pgrowth(population, 3)) |
		plag(yt01$transform(best), 1) +
		plag(log(population), 1)
	f3 <- pdiff(cv, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pgrowth(population, 3)) +
		plag(pgrowth(gdppc, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(population), 1) +
		plag(log(gdppc), 1)
	f4 <- pdiff(cv, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pgrowth(population, 3)) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(population), 1) +
		plag(log(gdppc), 1) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1)
	f5 <- pdiff(cv, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pgrowth(population, 3)) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(population), 1) +
		plag(log(gdppc), 1) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)
	f6 <- pdiff(cv, 1) ~
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(gdppc), 1) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)
	f7 <- pdiff(cv, 1) ~
		plag(pmsum(I(best>1000), 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(gdppc), 1) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)
	f8 <- pdiff(cv, 1) ~
		plag(pmsum(I(best>1000), 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(gdppc), 1) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(spei6gs, 1)


	f9 <- pdiff(cv, 1) ~
		plag(I(best>1000), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) |
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)

	f10 <- pdiff(cv, 1) ~
		plag(I(best>1000), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)

	f11 <- pdiff(cv, 1) ~
		plag(I(best>1000), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1)


	fit_cv1 <- hetero(f1, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv2 <- hetero(f2, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv3 <- hetero(f3, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv4 <- hetero(f4, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv5 <- hetero(f5, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv6 <- hetero(f6, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv7 <- hetero(f7, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv8 <- hetero(f8, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv9 <- hetero(f9, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv10 <- hetero(f10, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
	fit_cv11 <- hetero(f11, data = main_df, panel.id = ~ gwcode + year, method = "nlm")

	comp <- compare_models(fit_cv1, fit_cv2, fit_cv3, fit_cv4, fit_cv5, fit_cv6, fit_cv7, fit_cv8, fit_cv9, fit_cv10, fit_cv11, original_data = main_df, test = T)
	comp

	print(xtable(comp$comparison,
							 label = "tab:cv_alternative_specifications_gof",
							 caption = "$/Delta CV$ models, goodness-of-fit",
							 digits = 1), booktabs = TRUE,
				file = file.path("tables", simulation_alternative, "cv_alternative_specifications_gof.tex"))

	alt_cv_specifications <- hetero_table("a" = comp$refitted_models[[1]],
																				 "b" = comp$refitted_models[[2]],
																				 "c" = comp$refitted_models[[3]],
																				 "d" = comp$refitted_models[[4]],
																				 "e" = comp$refitted_models[[5]],
																				 "f" = comp$refitted_models[[6]],
																				 "g" = comp$refitted_models[[7]],
																				 "h" = comp$refitted_models[[8]],
																				 "i" = comp$refitted_models[[9]],
																				 "j" = comp$refitted_models[[10]],
																				 "k" = comp$refitted_models[[11]],
																				 title = "$\\Delta CV$ alternative specifications \\label{tab:cv_alt_specifications}",
																				 label_style = "latex",
																				 add_args = list(fmt = fmt_decimal(digits = 4),
																				 								escape = FALSE,
																				 								gof_omit = "p_mean|q_var|Panel FE|Time FE|Panel SD")
																				)

	tinytable::save_tt(alt_cv_specifications, file.path("tables", simulation_alternative, "cv_alt_specifications.tex"), overwrite = TRUE)

	#modelsummary(comp$refitted_models)

	main_df[, cv_variance := sd(cv, na.rm = T), .(gwcode)]
	main_df[, some_conflict := sum(best, na.rm = T) > 0, .(gwcode)]
	main_df[, developing := max(gdppc, na.rm = T) < 10000, .(gwcode)]
	main_df[, low_des := max(des, na.rm = T) < 2500, .(gwcode)]
	main_df[, high_des := max(des, na.rm = T) > 3500, .(gwcode)]

	hist(main_df$cv_variance)

	cv_base <- fit_cv9
	cv_variance <- hetero(f9, data = main_df[cv_variance > 0.01], panel.id = ~ gwcode + year, method = "nlm")
	cv_conflict <- hetero(f9, data = main_df[some_conflict == TRUE], panel.id = ~ gwcode + year, method = "nlm")
	cv_developing <- hetero(f9, data = main_df[developing == TRUE], panel.id = ~ gwcode + year, method = "nlm")
	cv_low_des <- hetero(f9, data = main_df[low_des == TRUE], panel.id = ~ gwcode + year, method = "nlm")
	cv_high_des <- hetero(f9, data = main_df[high_des == TRUE], panel.id = ~ gwcode + year, method = "nlm")

	alt_cv_subsets <- hetero_table("base" = cv_base,
							 "varying_cv" = cv_variance,
							 "conflict_history" =  cv_conflict,
							 "GDPPC < 10K" = cv_developing,
							 "DES < 2500" = cv_low_des,
							 "DES > 3500" = cv_high_des,
							 title = "$\\Delta CV$ alternative data subsets \\label{tab:cv_alt_subsets}",
							 label_style = "latex",
							 add_args = list(fmt = fmt_decimal(digits = 4),
							 								escape = FALSE,
							 								gof_omit = "p_mean|q_var|Panel FE|Time FE|Panel SD")
	)

	tinytable::save_tt(alt_cv_subsets, file.path("tables", simulation_alternative, "cv_alt_subsets.tex"), overwrite = TRUE)


	cv_dem <- fit_cv5
	cv_variance <- hetero(f5, data = main_df[cv_variance > 0.01], panel.id = ~ gwcode + year, method = "nlm")
	cv_conflict <- hetero(f5, data = main_df[some_conflict == TRUE], panel.id = ~ gwcode + year, method = "nlm")
	cv_developing <- hetero(f5, data = main_df[developing == TRUE], panel.id = ~ gwcode + year, method = "nlm")
	cv_low_des <- hetero(f5, data = main_df[low_des == TRUE], panel.id = ~ gwcode + year, method = "nlm")
	cv_high_des <- hetero(f5, data = main_df[high_des == TRUE], panel.id = ~ gwcode + year, method = "nlm")

	alt_cv_subsets_dem <- hetero_table("base" = cv_dem,
							 "varying_cv" = cv_variance,
							 "conflict_history" =  cv_conflict,
							 "GDPPC < 10K" = cv_developing,
							 "DES < 2500" = cv_low_des,
							 "DES > 3500" = cv_high_des,
							 title = "$\\Delta CV$ alternative data subsets with democracy \\label{tab:cv_alt_subsets_dem}",
							 label_style = "latex",
							 add_args = list(fmt = fmt_decimal(digits = 4),
							 								escape = FALSE,
							 								gof_omit = "p_mean|q_var|Panel FE|Time FE|Panel SD")
	)

	tinytable::save_tt(alt_cv_subsets_dem, file.path("tables", simulation_alternative, "cv_alt_subsets_dem.tex"), overwrite = TRUE)


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

	cplot_fit_des_mean <- coefplot(fit_des, which = "mean") +
		scale_x_discrete(labels = c(
			".pl1_ms_yt01_transform_best_3" = "BRD_MS3",
			".pl1_g_gdppc_3" = "GDPPC_∆P3",
			".pl1_d_tx90pgs_3" = "TX90_∆3",
			".pl1_d_v2x_polyarchy_3" = "DEM_∆3",
			".pl1_d_I_v2x_polyarchy_2_3" = "DEMsq_∆3",
			".pl1_g_population_3" = "POP_∆P3")) + ylab("Std. Coefficient Estimate") +
		ggtitle("E(∆DES)")

	cplot_fit_des_variance <- coefplot(fit_des, which = "variance") +
		scale_x_discrete(labels = c(
			".pl1_yt01_transform_best" = "BRD",
			".pl1_tx90pgs" = "TX90",
			".pl1_log_gdppc" = "lGDPPC",
			".pl1_v2x_polyarchy" = "DEM",
			".pl1_I_v2x_polyarchy_2" = "DEMsq",
			".pl1_log_population" = "lPOP")) + ylab("Std. Coefficient Estimate") +
		ggtitle("V(∆DES)")

	(cplot_fit_des_mean + cplot_fit_des_variance) /
	(cplot_fit_cv_mean + cplot_fit_cv_variance) + patchwork::plot_layout(nrow = 2, axes = "collect") + patchwork::plot_annotation(tag_levels = "A") &
		theme_bw(base_size = 24)
	ggsave(file.path("figures", simulation_alternative, "effect_plot.png"), device = ragg_png,  width = 12, height = 6, scale = 1.5)
}
