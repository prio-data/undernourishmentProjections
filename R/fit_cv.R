main_df[, cv_variance := sd(cv, na.rm = T), .(gwcode)]

cv_data <- main_df[cv_variance > 0]

nlag <- 1
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
f8 <- pdiff(cv, nlag) ~
	plag(yt01$transform(best), nlag) +
	plag(log(population), nlag) |
	plag(tx90pgs, nlag) +
	plag(rx5daygs, nlag) +
	plag(log(gdppc), nlag)

f9 <- pdiff(cv, nlag) ~
	plag(yt01$transform(best), nlag) +
	plag(log(population), nlag) +
	plag(pdiff(tx90pgs, 3), 1) +
	plag(pdiff(rx5daygs, 3), 1) +
	plag(pgrowth(gdppc, 3), 1) +
	plag(pdiff(v2x_polyarchy, 3), 1) +
	plag(pdiff(I(v2x_polyarchy^2), 3), 1) |
	plag(tx90pgs, nlag) +
	plag(rx5daygs, 1) +
	plag(spei6gs, nlag) +
	plag(log(gdppc), nlag)

f10 <- pdiff(cv, nlag) ~
	plag(yt01$transform(best), nlag) +
	plag(log(population), nlag) +
	plag(pdiff(tx90pgs, 3), 1) +
	plag(pdiff(rx5daygs, 3), 1) +
	plag(pgrowth(gdppc, 3), 1) |
	plag(tx90pgs, nlag) +
	plag(rx5daygs, 1) +
	plag(spei6gs, nlag) +
	plag(log(gdppc), nlag)

fit_cv1 <- hetero(f1, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv2 <- hetero(f2, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv3 <- hetero(f3, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv4 <- hetero(f4, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv5 <- hetero(f5, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv6 <- hetero(f6, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv7 <- hetero(f7, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv8 <- hetero(f8, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv9 <- hetero(f9, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv10 <- hetero(f10, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")

comp <- compare_models(fit_cv1, fit_cv2, fit_cv3, fit_cv4, fit_cv5, fit_cv6, fit_cv7, fit_cv8, fit_cv9, fit_cv10, original_data = main_df, test = T)
comp

fit_cv1 <- hetero(f1, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv2 <- hetero(f2, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv3 <- hetero(f3, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv4 <- hetero(f4, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv5 <- hetero(f5, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv6 <- hetero(f6, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv7 <- hetero(f7, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv8 <- hetero(f8, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")

comp <- compare_models(fit_cv1, fit_cv2, fit_cv3, fit_cv4, fit_cv5, fit_cv6, fit_cv7, fit_cv8, original_data = main_df, test = T)
comp

fit_cv7a <- hetero(f7, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
fit_cv7b <- hetero(f7, data = cv_data, panel.id = ~ gwcode + year, method = "BFGS")

summary(fit_cv7a)
summary(fit_cv7b)

modelsummary(comp$refitted_models)
