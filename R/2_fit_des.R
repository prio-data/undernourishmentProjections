yt01 <- scales::yj_trans(p = 0.1)


if(simulation_alternative == "no_conflict_effect"){
	fit1 <- hetero(
		pdiff(des, 1) ~
			plag(pdiff(tx90pgs, 5), 1) +     # warm_d_l1
			plag(pgrowth(gdppc, 3), 1) +     # econ_d_prop_l1
			plag(pdiff(v2x_polyarchy, 10), 1) + # dem_d_l1
			plag(pdiff(I(v2x_polyarchy^2), 10), 1) + #dem_d2_l1
			plag(pgrowth(population, 5), 1)  # pop_d_prop_l1
		|
			plag(tx90pgs, 1) +               # tx90pgs_l1
			plag(log(gdppc), 1) +                # lgdppc_l1
			plag(v2x_polyarchy, 1) +         # v2x_polyarchy_l1
			plag(I(v2x_polyarchy^2), 1) +      # v2x_polyarchy_2_l1
			plag(log(population), 1),            # lpopulation_l1
		data = main_df,
		panel.id = ~ gwcode + year,
		method = "BFGS"
	)
} else {
	fit1 <- hetero(
		pdiff(des, 1) ~
			plag(pmsum(yt01$transform(best), 5), 1) +      # conflict_l1
			plag(pdiff(tx90pgs, 5), 1) +     # warm_d_l1
			plag(pgrowth(gdppc, 3), 1) +     # econ_d_prop_l1
			plag(pdiff(v2x_polyarchy, 10), 1) + # dem_d_l1
			plag(pdiff(I(v2x_polyarchy^2), 10), 1) + #dem_d2_l1
			plag(pgrowth(population, 5), 1)  # pop_d_prop_l1
		|
			plag(yt01$transform(best), 1) +  # ytbest_l1
			plag(tx90pgs, 1) +               # tx90pgs_l1
			plag(log(gdppc), 1) +                # lgdppc_l1
			plag(v2x_polyarchy, 1) +         # v2x_polyarchy_l1
			plag(I(v2x_polyarchy^2), 1) +      # v2x_polyarchy_2_l1
			plag(log(population), 1),            # lpopulation_l1
		data = main_df,
		panel.id = ~ gwcode + year,
		method = "BFGS"
	)

	cplot_fit1 <- coefplot(fit1)
	me <- marginal_effects(fit1, "best", level = 0.95, n_points = 100, type = "shift", clamp = c(0, Inf))
	meplot <- plot(me)
	meplot <- meplot + ggtitle("Marginal effects")

	cplot_fit1 + meplot + patchwork::plot_layout(nrow = 1, axes = "collect") &
		theme_bw(base_size = 24)
	ggsave(file.path("figures", simulation_alternative, "des_figure.png"), device = ragg_png,  width = 12, height = 4, scale = 1.5)
}


# Original version (varied length of moving windows based on theoretical expectations)
fit_orig <- hetero(
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
	method = "BFGS"
)

# Consistent 3-year moving window
fit_mv3 <- hetero(
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
	method = "BFGS"
)

# Consistent 5-year moving window
fit_mv5 <- hetero(
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
	method = "BFGS"
)

comp <- compare_models(fit_orig, fit_mv3, fit_mv5, original_data = main_df, test = T)
comp
modelsummary(comp$refitted_models)
# Result: Original version is slightly better, but consistent 3-year moving window is just as good in-sample. We keep mv3 for simplicity.

fit_mv3_drop_poly2 <- hetero(
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
	method = "BFGS"
)
comp <- compare_models(fit_mv3, fit_mv3_drop_poly2, original_data = main_df, test = T)
comp
modelsummary(comp$refitted_models)
# Slightly better with than without, but both are viable.

fit_mv3_drop_hetero <- hetero(
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
	method = "BFGS"
)
comp <- compare_models(fit_mv3, fit_mv3_drop_hetero, original_data = main_df, test = T)
comp
modelsummary(comp$refitted_models)
# Much worse fit if we drop variance component.

fit_mv3_country_variance <- hetero(
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
	method = "BFGS"
)
comp <- compare_models(fit_mv3, fit_mv3_country_variance, original_data = main_df, test = T)
comp
modelsummary(comp$refitted_models)
# Much better fit if we add the historical country-level variance as predictor.
# There is variance across countries that can be explained by factors not captured by our co-variates.
# At the same time, it is not clear how such fixed effects should be assumed to change into the future.


fit_mv3_arch1 <- hetero(
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
		arch(1),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "BFGS"
)
comp <- compare_models(fit_mv3, fit_mv3_arch1, original_data = main_df, test = T)
comp
modelsummary(comp$refitted_models)
# The variance is auto-regressive. I.e., if there was much change in DES the year before, chances are higher of change the next year.

fit_mv3_arch10 <- hetero(
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
		arch(10),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "BFGS"
)
comp <- compare_models(fit_mv3, fit_mv3_arch1, fit_mv3_arch10, original_data = main_df, test = T)
comp
modelsummary(comp$refitted_models)
# There is relatively less to gain from further auto-regressive terms.


fit_mv3_add_extreme_precipitation <- hetero(
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
	method = "BFGS"
)
comp <- compare_models(fit_mv3, fit_mv3_add_extreme_precipitation, original_data = main_df, test = T)
comp
modelsummary(comp$refitted_models)
# Better fit, but perhaps surprising? Lower variance and increased DES from more extreme precipitation days.

fit_mv3_spei6 <- hetero(
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
	method = "BFGS"
)
comp <- compare_models(fit_mv3, fit_mv3_spei6, original_data = main_df, test = T)
comp
modelsummary(comp$refitted_models)
# Worse fit swapping TX90 with SPEI6.

fit_mv3_add_spei6 <- hetero(
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
	method = "BFGS"
)
comp <- compare_models(fit_mv3, fit_mv3_add_spei6, original_data = main_df, test = T)
comp
modelsummary(comp$refitted_models)
# Not better just adding SPEI6


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
	method = "BFGS"
)

summary(fit_mv3)
summary(fit_mv3_nlag)
# Same patterns, but stronger results when considering 2-5 years than 1 year.
# Certain effects drop off over very long time-spans. E.g., conflict.
