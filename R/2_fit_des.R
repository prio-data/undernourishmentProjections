yt01 <- scales::yj_trans(p = 0.1)
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
ggsave("figures/base/des_figure.png", device = ragg_png,  width = 12, height = 4, scale = 1.5)


