yt01 <- scales::yj_trans(p = 0.1)

f_des <- pdiff(des, 1) ~
	plag(pmsum(yt01$transform(best), 3), 1) +
	plag(pdiff(tx90pgs, 3), 1) +
	plag(pgrowth(gdppc, 3), 1) +
	plag(pdiff(v2x_polyarchy, 3), 1) +
	plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
	plag(pgrowth(population, 3), 1) |
	plag(yt01$transform(best), 1) +
	plag(tx90pgs, 1) +
	plag(log(gdppc), 1) +
	plag(v2x_polyarchy, 1) +
	plag(I(v2x_polyarchy^2), 1) +
	plag(log(population), 1)

f_des_long <- pdiff(des, 1) ~
	plag(yt01$transform(best), 1) +
	plag(pdiff(tx90pgs, 1), 1) +
	plag(pgrowth(gdppc, 1), 1) +
	plag(pdiff(v2x_polyarchy, 1), 1) +
	plag(pdiff(I(v2x_polyarchy^2), 1), 1) +
	plag(pgrowth(population, 1), 1) |
	plag(yt01$transform(best), 1) +
	plag(tx90pgs, 1) +
	plag(log(gdppc), 1) +
	plag(v2x_polyarchy, 1) +
	plag(I(v2x_polyarchy^2), 1) +
	plag(log(population), 1)

f_cv <- pdiff(cv, 1) ~
	plag(I(best>1000), 1) +
	plag(pgrowth(gdppc, 3), 1) +
	plag(pdiff(tx90pgs, 3), 1) |
	plag(log(gdppc), 1) +
	plag(v2x_polyarchy, 1) +
	plag(I(v2x_polyarchy^2), 1)

f_cv_long <- pdiff(cv, 1) ~
	plag(I(best>1000), 1) +
	plag(pgrowth(gdppc, 1), 1) +
	plag(pdiff(tx90pgs, 1), 1) |
	plag(log(gdppc), 1) +
	plag(v2x_polyarchy, 1) +
	plag(I(v2x_polyarchy^2), 1)


# Load and prepare main data
main_df <- data.table::fread("data/imputed_main.csv")
data.table::setnames(main_df, "calorie_var", "cv")

unit_var <- "gwcode"
time_var <- "year"
outcomes <- c("des", "pou", "mder", "cv")
explanatory_variables <- c("best", "gdppc", "population",
													 "v2x_polyarchy", "secprop", "tx90pgs", "spei6gs", "spi6gs", "rx5daygs", "tas")

main_df <- main_df[, c(unit_var, time_var, outcomes, explanatory_variables), with = FALSE]
main_df$period <- main_df$year
mean_vars <- setdiff(names(main_df), c("gwcode", "year", "best"))

main_df3 <- main_df
main_df5 <- main_df

main_df3[, period := (year - min(year)) %/% 3, by = gwcode]
main_df3 <- main_df3[, c(
	list(year = max(year), best = sum(best, na.rm = TRUE)),
	lapply(.SD, mean, na.rm = TRUE)
), by = .(gwcode, period), .SDcols = mean_vars]

main_df5[, period := (year - min(year)) %/% 5, by = gwcode]
main_df5 <- main_df5[, c(
	list(year = max(year), best = sum(best, na.rm = TRUE)),
	lapply(.SD, mean, na.rm = TRUE)
), by = .(gwcode, period), .SDcols = mean_vars]


fit_des <- hetero(f_des, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
fit_des3 <- hetero(f_des_long, data = main_df3, panel.id = ~ gwcode + period, method = "nlm")
fit_des5 <- hetero(f_des_long, data = main_df5, panel.id = ~ gwcode + period, method = "nlm")
fit_cv <- hetero(f_cv, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
fit_cv3 <- hetero(f_cv_long, data = main_df3, panel.id = ~ gwcode + period, method = "nlm")
fit_cv5 <- hetero(f_cv_long, data = main_df5, panel.id = ~ gwcode + period, method = "nlm")


des_alt_time_intervals <- hetero_table("1-year" = fit_des, "3-year" = fit_des3, "5-year" = fit_des5,
 title = "$\\Delta DES$ alternative time-intervals \\label{tab:des_time_intervals}",
 label_style = "latex",
 add_args = list(fmt = fmt_decimal(digits = 4),
								escape = FALSE,
								gof_omit = "p_mean|q_var|Panel FE|Time FE|Panel SD")
)

tinytable::save_tt(des_alt_time_intervals, file.path("tables", simulation_alternative, "des_time_intervals.tex"), overwrite = TRUE)

cv_alt_time_intervals <- hetero_table("1-year" = fit_cv, "3-year" = fit_cv3, "5-year" = fit_cv5,
																			 title = "$\\Delta CV$ alternative time-intervals \\label{tab:des_time_intervals}",
																			 label_style = "latex",
																			 add_args = list(fmt = fmt_decimal(digits = 4),
																			 								escape = FALSE,
																			 								gof_omit = "p_mean|q_var|Panel FE|Time FE|Panel SD")
)

tinytable::save_tt(cv_alt_time_intervals, file.path("tables", simulation_alternative, "cv_time_intervals.tex"), overwrite = TRUE)




