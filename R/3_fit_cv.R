if(simulation_alternative == "no_conflict_effect"){
	f_cv <- pdiff(cv, nlag) ~
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) |
		plag(log(gdppc), nlag) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)
} else{
	f_cv <- pdiff(cv, nlag) ~
		plag(I(best>1000), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) |
		plag(log(gdppc), nlag) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)
}

fit_cv <- hetero(f_cv, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")

cv_test <- FALSE
if(cv_test){
	main_df[, cv_variance := sd(cv, na.rm = T), .(gwcode)]
	main_df[, some_conflict := sum(best, na.rm = T) > 0, .(gwcode)]
	main_df[, developing := max(gdppc, na.rm = T) < 5000, .(gwcode)]

	hist(main_df$cv_variance)

	cv_data <- main_df[cv_variance > 0.01 & some_conflict & developing]

	nlag <- 1

	f1 <- pdiff(cv, nlag) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(yt01$transform(best), 1)
	f2 <- pdiff(cv, nlag) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pgrowth(population, 3)) |
		plag(yt01$transform(best), 1) +
		plag(log(population), nlag)
	f3 <- pdiff(cv, nlag) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pgrowth(population, 3)) +
		plag(pgrowth(gdppc, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(population), nlag) +
		plag(log(gdppc), nlag)
	f4 <- pdiff(cv, nlag) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pgrowth(population, 3)) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(population), nlag) +
		plag(log(gdppc), nlag) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1)
	f5 <- pdiff(cv, nlag) ~
		plag(pmsum(yt01$transform(best), 3), 1) +
		plag(pgrowth(population, 3)) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) +
		plag(pdiff(v2x_polyarchy, 3), 1) +
		plag(pdiff(I(v2x_polyarchy^2), 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(population), nlag) +
		plag(log(gdppc), nlag) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)
	f6 <- pdiff(cv, nlag) ~
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(gdppc), nlag) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)
	f7 <- pdiff(cv, nlag) ~
		plag(pmsum(I(best>1000), 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(gdppc), nlag) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)
	f8 <- pdiff(cv, nlag) ~
		plag(pmsum(I(best>1000), 3), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) +
		plag(pdiff(rx5daygs, 3), 1) |
		plag(yt01$transform(best), 1) +
		plag(log(gdppc), nlag) +
		plag(tx90pgs, 1) +
		plag(rx5daygs, 1) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1) +
		plag(spei6gs, 1)


	f9 <- pdiff(cv, nlag) ~
		plag(I(best>1000), 1) +
		plag(pgrowth(gdppc, 3), 1) +
		plag(pdiff(tx90pgs, 3), 1) |
		plag(log(gdppc), nlag) +
		plag(v2x_polyarchy, 1) +
		plag(I(v2x_polyarchy^2), 1)


	fit_cv1 <- hetero(f1, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv2 <- hetero(f2, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv3 <- hetero(f3, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv4 <- hetero(f4, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv5 <- hetero(f5, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv6 <- hetero(f6, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv7 <- hetero(f7, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv8 <- hetero(f8, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv9 <- hetero(f9, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")

	comp <- compare_models(fit_cv1, fit_cv2, fit_cv3, fit_cv4, fit_cv5, fit_cv6, fit_cv7, fit_cv8, fit_cv9, original_data = main_df, test = T)
	comp

	#modelsummary(comp$refitted_models)

	fit_cv1_limited <- hetero(f1, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv2_limited <- hetero(f2, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv3_limited <- hetero(f3, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv4_limited <- hetero(f4, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv5_limited <- hetero(f5, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv6_limited <- hetero(f6, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv7_limited <- hetero(f7, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv8_limited <- hetero(f8, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
	fit_cv9_limited <- hetero(f9, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")

	comp <- compare_models(fit_cv1_limited,
												 fit_cv2_limited,
												 fit_cv3_limited,
												 fit_cv4_limited,
												 fit_cv5_limited,
												 fit_cv6_limited,
												 fit_cv7_limited,
												 fit_cv8_limited,
												 fit_cv9_limited, original_data = main_df, test = T)
	comp

	summary(fit_cv9)
	summary(fit_cv9_limited)
}
