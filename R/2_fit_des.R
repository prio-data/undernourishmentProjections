yt01 <- scales::yj_trans(p = 0.1)

f_base <- pdiff(des, NLAG) ~
	plag(pmsum(yt01$transform(best), 3), NLAG) +
	plag(pdiff(tx90pgs, 3), NLAG) +
	plag(pgrowth(gdppc, 3), NLAG) +
	plag(pdiff(v2x_polyarchy, 3), NLAG) +
	plag(pdiff(I(v2x_polyarchy^2), 3), NLAG) +
	plag(pgrowth(population, 3), NLAG) |
	plag(yt01$transform(best), NLAG) +
	plag(tx90pgs, NLAG) +
	plag(log(gdppc), NLAG) +
	plag(v2x_polyarchy, NLAG) +
	plag(I(v2x_polyarchy^2), NLAG) +
	plag(log(population), NLAG)

f_no_conflict_effect <-
	pdiff(des, NLAG) ~
	plag(pdiff(tx90pgs, 3), NLAG) +
	plag(pgrowth(gdppc, 3), NLAG) +
	plag(pdiff(v2x_polyarchy, 3), NLAG) +
	plag(pdiff(I(v2x_polyarchy^2), 3), NLAG) +
	plag(pgrowth(population, 3), NLAG) |
	plag(tx90pgs, NLAG) +
	plag(log(gdppc), NLAG) +
	plag(v2x_polyarchy, NLAG) +
	plag(I(v2x_polyarchy^2), NLAG) +
	plag(log(population), NLAG)


if(simulation_alternative == "no_conflict_effect"){
	fit_des <- hetero(f_no_conflict_effect, data = main_df, panel.id = ~ gwcode + year, method = "nlm")
} else{
	fit_des <- hetero(f_base, data = main_df, panel.id = ~ gwcode + year, method = "nlm")


}

# Just do this once
if(simulation_alternative == "base" & cv_approach == "regression" & NLAG == 1){
	cplot_fit_des <- coefplot(fit_des)
	me <- marginal_effects(fit_des, "best", data = main_df, level = 0.95, values = seq(-2000, 2000, by = 50), type = "shift", clamp = c(0, Inf))
	meplot <- plot(me)
	meplot <- meplot + ggtitle("Marginal effects")

	cplot_fit_des + meplot + patchwork::plot_layout(nrow = 1, axes = "collect") &
		theme_bw(base_size = 24)
	ggsave(file.path("figures", simulation_alternative, "des_figure.png"), device = ragg_png,  width = 12, height = 4, scale = 1.5)
}



