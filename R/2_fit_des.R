yt01 <- scales::yj_trans(p = 0.1)

f_base <- pdiff(des, 1) ~
	plag(pmsum(yt01$transform(best), 3), 1) +
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

f_no_conflict_effect <-
	pdiff(des, 1) ~
	plag(pdiff(tx90pgs, 3), 1) +
	plag(pdiff(rx5daygs, 3), 1) +
	plag(pgrowth(gdppc, 3), 1) +
	plag(pdiff(v2x_polyarchy, 3), 1) +
	plag(pdiff(I(v2x_polyarchy^2), 3), 1) +
	plag(pgrowth(population, 3), 1) |
	plag(tx90pgs, 1) +
	plag(rx5daygs, 1) +
	plag(log(gdppc), 1) +
	plag(v2x_polyarchy, 1) +
	plag(I(v2x_polyarchy^2), 1) +
	plag(log(population), 1)


if(simulation_alternative == "no_conflict_effect"){
	fit_des <- hetero(f_no_conflict_effect, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")
} else{
	fit_des <- hetero(f_base, data = main_df, panel.id = ~ gwcode + year, method = "BFGS")


}

# Just do this once
if(simulation_alternative == "base" & cv_approach == "regression"){
	cplot_fit_des <- coefplot(fit_des)
	me <- marginal_effects(fit_des, "best", data = main_df, level = 0.95, values = seq(-2000, 2000, by = 50), type = "shift", clamp = c(0, Inf))
	meplot <- plot(me)
	meplot <- meplot + ggtitle("Marginal effects")

	cplot_fit_des + meplot + patchwork::plot_layout(nrow = 1, axes = "collect") &
		theme_bw(base_size = 24)
	ggsave(file.path("figures", simulation_alternative, "des_figure.png"), device = ragg_png,  width = 12, height = 4, scale = 1.5)
}



