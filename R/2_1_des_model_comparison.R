yt01 <- scales::yj_trans(p = 0.1)


# Original version (varied length of moving windows based on theoretical expectations)
des_fit_var <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 5), 1) +
		plag(pdiff(tx90pgs, 5), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 10), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 10), 1) +
		plag(pgrowth(population, 5), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)

# Consistent 3-year moving window
des_fit_mv3 <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)

# Consistent 5-year moving window
des_fit_mv5 <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 5), 1) +
		plag(pdiff(tx90pgs, 5), 1) +
		plag(pgrowth(gdppc, 5), 1) +
		plag(pdiff(v2x_polyarchy, 5), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 5), 1) +
		plag(pgrowth(population, 5), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)

comp <- compare_models(des_fit_var, des_fit_mv3, des_fit_mv5, original_data = main_df, test = T)
comp
print(xtable(comp$comparison,
			 label = "tab:des_moving_window_gof",
			 caption = "$/Delta DES$ models with different moving window times",
			 digits = 1), booktabs = TRUE,
			file = file.path("tables", simulation_alternative, "des_moving_window_gof.tex"))

des_moving_window_regression <- hetero_table(
	"des_var" = comp$refitted_models[[1]],
  "des_mv3" = comp$refitted_models[[2]],
  "des_mv5" = comp$refitted_models[[3]],
	title = "$\\Delta DES$ models with different moving windows \\label{tab:des_moving_window_regression}",
	label_style = "latex",
	add_args = list(fmt = fmt_decimal(digits = 4),
									escape = FALSE,
									gof_omit = "p_mean|q_var|Panel FE|Time FE|Panel SD")
)

tinytable::save_tt(des_moving_window_regression, file.path("tables", simulation_alternative, "des_moving_window_regression.tex"), overwrite = TRUE)


# Result: Original version is slightly better, but consistent 3-year moving window is just as good in-sample. We keep mv3 for simplicity.
base <- des_fit_mv3

drop_dem <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(log(gdppc), 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)

drop_conf <- hetero(
	pdiff(des, 1) ~
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(tx90pgs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)

drop_climate <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)

drop_pop <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)



drop_demsq <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)
comp <- compare_models(base, drop_demsq, original_data = main_df, test = T)
comp
print(xtable(comp$comparison,
						 label = "tab:des_drop_demsq_gof",
						 caption = "$/Delta DES$ models, base vs dropping squared democracy term",
						 digits = 1), booktabs = TRUE,
			file = file.path("tables", simulation_alternative, "des_drop_demsq_gof.tex"))


hetero_table("base" = comp$refitted_models[[1]],
						 "drop_demsq" = comp$refitted_models[[2]])
# Slightly better with than without, but both are viable.

drop_variance <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		1,
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)
comp <- compare_models(base, drop_variance, original_data = main_df, test = T)
comp
print(xtable(comp$comparison,
						 label = "tab:des_drop_variance_gof",
						 caption = "$/Delta DES$ models, base vs dropping variance terms",
						 digits = 1), booktabs = TRUE,
			file = file.path("tables", simulation_alternative, "des_drop_variance_gof.tex"))
hetero_table("base" = comp$refitted_models[[1]],
						 "drop_variance" = comp$refitted_models[[2]])
# Much worse fit if we drop variance component.

add_country_variance <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1) +
		panel.sd(),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)
comp <- compare_models(base, add_country_variance, original_data = main_df, test = T)
comp
print(xtable(comp$comparison,
						 label = "tab:des_add_country_variance_gof",
						 caption = "$/Delta DES$ models, base vs adding country-level historical variance",
						 digits = 1), booktabs = TRUE,
			file = file.path("tables", simulation_alternative, "des_add_country_variance_gof.tex"))
hetero_table("base" = comp$refitted_models[[1]],
						 "add_country_variance" = comp$refitted_models[[2]])
# Much better fit if we add the historical country-level variance as predictor.
# There is variance across countries that can be explained by factors not captured by our co-variates.
# At the same time, it is not clear how such fixed effects should be assumed to change into the future.


add_extreme_precipitation <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)
comp <- compare_models(base, add_extreme_precipitation, original_data = main_df, test = T)
comp
print(xtable(comp$comparison,
						 label = "tab:des_add_extreme_precipitation_gof",
						 caption = "$/Delta DES$ models, base vs adding ARCH(1)",
						 digits = 1), booktabs = TRUE,
			file = file.path("tables", simulation_alternative, "des_add_extreme_precipitation_gof.tex"))
hetero_table("base" = comp$refitted_models[[1]],
						 "add_extreme_precipitation" = comp$refitted_models[[2]])

swap_spei6 <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pmsum(spei6gs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(spei6gs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)
comp <- compare_models(base, swap_spei6, original_data = main_df, test = T)
comp
print(xtable(comp$comparison,
						 label = "tab:des_swap_spei6_gof",
						 caption = "$/Delta DES$ models, base vs swapping TX90 and SPEI-6",
						 digits = 1), booktabs = TRUE,
			file = file.path("tables", simulation_alternative, "des_swap_spei6_gof.tex"))
hetero_table("base" = comp$refitted_models[[1]],
						 "swap_spei6" = comp$refitted_models[[2]])
# Worse fit swapping TX90 with SPEI6.

add_spei6 <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pmsum(spei6gs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(spei6gs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)
comp <- compare_models(base, add_spei6, original_data = main_df, test = T)
comp
print(xtable(comp$comparison,
						 label = "tab:des_add_spei6_gof",
						 caption = "$/Delta DES$ models, base vs adding SPEI-6",
						 digits = 1), booktabs = TRUE,
			file = file.path("tables", simulation_alternative, "des_add_spei6_gof.tex"))
hetero_table("base" = comp$refitted_models[[1]],
						 "add_spei6" = comp$refitted_models[[2]])
# Not better just adding SPEI6

# All
comp <- compare_models(base, drop_demsq, drop_variance, add_country_variance, add_extreme_precipitation, swap_spei6, add_spei6, original_data = main_df, test = T)
comp

print(xtable(comp$comparison,
						 label = "tab:des_alternative_specifications_gof",
						 caption = "$/Delta DES$ models, base vs adding SPEI-6",
						 digits = 1), booktabs = TRUE,
			file = file.path("tables", simulation_alternative, "des_alternative_specifications_gof.tex"))

alt_des_specifications <- hetero_table("base" = comp$refitted_models[[1]],
						 "drop_demsq" = comp$refitted_models[[2]],
						 "drop_variance" = comp$refitted_models[[3]],
						 "add_country_variance" = comp$refitted_models[[4]],
						 "add_extreme_precipitation" = comp$refitted_models[[5]],
						 "swap_spei6" = comp$refitted_models[[6]],
						 "add_spei6" = comp$refitted_models[[7]],
						 title = "$\\Delta DES$ model variants \\label{tab:des_alt_specifications}",
						 label_style = "latex",
						 add_args = list(fmt = fmt_decimal(digits = 4),
						 								escape = FALSE,
						 								gof_omit = "p_mean|q_var|Panel FE|Time FE|Panel SD")
)

tinytable::save_tt(alt_des_specifications, file.path("tables", simulation_alternative, "des_alt_specifications.tex"), overwrite = TRUE)


comp <- compare_models(base, drop_dem, drop_conf, drop_climate, drop_pop, original_data = main_df, test = T)
comp

print(xtable(comp$comparison,
						 label = "tab:des_alternative_specifications2_gof",
						 caption = "$/Delta DES$ models, base vs dropping covariates",
						 digits = 1), booktabs = TRUE,
			file = file.path("tables", simulation_alternative, "des_alternative_specifications2_gof.tex"))

alt_des_specifications2 <- hetero_table("base" = comp$refitted_models[[1]],
																			 "drop_dem" = comp$refitted_models[[2]],
																			 "drop_conf" = comp$refitted_models[[3]],
																			 "drop_climate" = comp$refitted_models[[4]],
																			 "drop_pop" = comp$refitted_models[[5]],
																				title = "$\\Delta DES$ model variants, cont \\label{tab:des_alt_specifications2}",
																				label_style = "latex",
																				add_args = list(fmt = fmt_decimal(digits = 4),
																												escape = FALSE,
																												gof_omit = "p_mean|q_var|Panel FE|Time FE|Panel SD")
)

tinytable::save_tt(alt_des_specifications2, file.path("tables", simulation_alternative, "des_alt_specifications2.tex"), overwrite = TRUE)



nlag <- 3
fit_mv3_nlag <- hetero(
	pdiff(des, nlag) ~
		plag(pmsum(yt01$transform(best), 3), nlag) +
		plag(pdiff(tx90pgs, 3), nlag) +
		plag(pgrowth(gdppc, 3), nlag) +
		plag(pdiff(v2x_polyarchy, 3), nlag) +
		plag(pdiff(I(v2x_polyarchy^2), 3), nlag) +
		plag(pgrowth(population, 3), nlag)
	|
		plag(yt01$transform(best), nlag) +
		plag(tx90pgs, nlag) +
		plag(log(gdppc), nlag) +
		plag(v2x_polyarchy, nlag) +
		plag(I(v2x_polyarchy^2), nlag) +
		plag(log(population), nlag),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "nlm"
)

hetero_table(base, fit_mv3_nlag)
# Same patterns, but stronger results when considering 2-5 years than 1 year.
# Certain effects drop off over very long time-spans. E.g., conflict.


fit_mv3_2000 <- hetero(
	pdiff(des, 1) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
		plag(pgrowth(population, 3), 1)
	|
		plag(yt01$transform(best), 1) +
		plag(tx90pgs, 1) +
		plag(log(gdppc), 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(log(population), 1),
	data = main_df[year >= 2000],
	panel.id = ~ gwcode + year,
	method = "nlm"
)

hetero_table(base, fit_mv3_2000)
# Quite similar results


main_df[, cv_variance := sd(cv, na.rm = T), .(gwcode)]
main_df[, some_conflict := sum(best, na.rm = T) > 0, .(gwcode)]
main_df[, developing := max(gdppc, na.rm = T) < 10000, .(gwcode)]
main_df[, low_cv := max(cv, na.rm = T) < 0.23, .(gwcode)]
main_df[, high_cv := max(cv, na.rm = T) > 0.3, .(gwcode)]

des_variance <- hetero(f_base, data = main_df[cv_variance > 0.01], panel.id = ~ gwcode + year, method = "nlm")
des_conflict <- hetero(f_base, data = main_df[some_conflict == TRUE], panel.id = ~ gwcode + year, method = "nlm")
des_developing <- hetero(f_base, data = main_df[developing == TRUE], panel.id = ~ gwcode + year, method = "nlm")
des_low_cv <- hetero(f_base, data = main_df[low_cv == TRUE], panel.id = ~ gwcode + year, method = "nlm")
des_high_cv <- hetero(f_base, data = main_df[high_cv == TRUE], panel.id = ~ gwcode + year, method = "nlm")

alt_des_subsets <- hetero_table("base" = base,
															 "varying_cv" = des_variance,
															 "conflict_history" =  des_conflict,
															 "GDPPC < 10K" = des_developing,
															 "CV < 0.23" = des_low_cv,
															 "CV > 0.3" = des_high_cv,
																title = "$\\Delta DES$ alternative subsets \\label{tab:des_alt_subsets}",
																label_style = "latex",
																add_args = list(fmt = fmt_decimal(digits = 4),
																								escape = FALSE,
																								gof_omit = "p_mean|q_var|Panel FE|Time FE|Panel SD")
)

tinytable::save_tt(alt_des_subsets, file.path("tables", simulation_alternative, "des_alt_subsets.tex"), overwrite = TRUE)

