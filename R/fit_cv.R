nlag <- 1

f1 <- pdiff(cv, nlag) ~
	plag(pmsum(yt01$transform(best), 5), nlag) +
	plag(pdiff(tx90pgs, 5), nlag) +
	plag(pgrowth(gdppc, 3), nlag) +
	plag(pdiff(v2x_polyarchy, 10), nlag) +
	plag(pdiff(I(v2x_polyarchy^2), 10), nlag) +
	plag(pgrowth(population, 5), nlag)
|
	plag(yt01$transform(best), nlag) +
	plag(tx90pgs, nlag) +
	plag(log(gdppc), nlag) +
	plag(v2x_polyarchy, nlag) +
	plag(I(v2x_polyarchy^2), nlag) +
	plag(log(population), nlag)

f1 <- pdiff(cv, nlag) ~
	plag(yt01$transform(best), nlag) +
	plag(log(population), nlag) |
	plag(tx90pgs, nlag) +
	plag(log(gdppc), nlag)
f2 <- pdiff(cv, nlag) ~
	plag(yt01$transform(best), nlag) |
	plag(tx90pgs, nlag) +
	plag(log(gdppc), nlag)
f3 <- pdiff(cv, nlag) ~
	plag(yt01$transform(best), nlag) +
	plag(log(population), nlag)
f4 <- pdiff(cv, nlag) ~
	plag(pmsum(yt01$transform(best), 5), nlag) +
	plag(log(population), nlag) |
	plag(tx90pgs, nlag) +
	plag(log(gdppc), nlag)
f5 <- pdiff(cv, nlag) ~
	plag(yt01$transform(best), nlag) +
	plag(log(population), nlag) |
	plag(spei6gs, nlag) +
	plag(log(gdppc), nlag)
f6 <- pdiff(cv, nlag) ~
	plag(yt01$transform(best), nlag) +
	plag(log(population), nlag) +
	plag(spei6gs, nlag)|
	plag(log(gdppc), nlag)
f7 <- pdiff(cv, nlag) ~
	plag(yt01$transform(best), nlag) +
	plag(log(population), nlag) |
	plag(tx90pgs, nlag) +
	plag(spei6gs, nlag) +
	plag(log(gdppc), nlag)

fit_cv1 <- hetero(f1, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv2 <- hetero(f2, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv3 <- hetero(f3, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv4 <- hetero(f4, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv5 <- hetero(f5, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv6 <- hetero(f6, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv7 <- hetero(f7, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")

comp <- compare_models(fit_cv1, fit_cv2, fit_cv3, fit_cv4, fit_cv5, fit_cv6, fit_cv7, original_data = main_df, test = T)
comp

library(modelsummary)
modelsummary(comp$refitted_models)

fit_cv <- hetero(
	pdiff(cv, nlag) ~
		plag(pmsum(yt01$transform(best), 5), nlag) +      # conflict_l1
		plag(pdiff(tx90pgs, 5), nlag) +     # warm_d_l1
		plag(pgrowth(gdppc, 3), nlag) +     # econ_d_prop_l1
		plag(pdiff(v2x_polyarchy, 10), nlag) + # dem_d_l1
		plag(pdiff(I(v2x_polyarchy^2), 10), nlag) + #dem_d2_l1
		plag(pgrowth(population, 5), nlag)  # pop_d_prop_l1
	|
		plag(yt01$transform(best), nlag) +  # ytbest_l1
		plag(tx90pgs, nlag) +               # tx90pgs_l1
		plag(log(gdppc), nlag) +                # lgdppc_l1
		plag(v2x_polyarchy, nlag) +         # v2x_polyarchy_l1
		plag(I(v2x_polyarchy^2), nlag) +      # v2x_polyarchy_2_l1
		plag(log(population), nlag),            # lpopulation_l1
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "BFGS"
)

fit_cv <- hetero(
	pdiff(cv, nlag) ~
		plag(yt01$transform(best), nlag) +
		plag(tx90pgs, nlag) +
		plag(log(population), nlag) |
		plag(yt01$transform(best), nlag) +
		plag(tx90pgs, nlag) +
		plag(log(population), nlag),
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "BFGS"
)

summary(fit_cv)



fit_cv2 <- hetero(
	pdiff(cv, nlag) ~
		plag(pdiff(tx90pgs, 5), nlag) +     # warm_d_l1
		plag(pgrowth(gdppc, 3), nlag) +     # econ_d_prop_l1
		plag(pdiff(v2x_polyarchy, 10), nlag) + # dem_d_l1
		plag(pdiff(I(v2x_polyarchy^2), 10), nlag) + #dem_d2_l1
		plag(pgrowth(population, 5), nlag),  # pop_d_prop_l1,
	data = main_df,
	panel.id = ~ gwcode + year,
	method = "BFGS"
)

tst <- heterolm::transform_newdata(fit_cv2, newdata = main_df)

m1 <- lm(fit_cv2$frame$parsed$mean_formula, data = tst)
summary(m1)
summary(fit_cv2)
