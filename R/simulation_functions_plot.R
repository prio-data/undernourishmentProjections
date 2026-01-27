library(cowplot)
library(ggpubr)



plot_battle <- function(data, low = 0.025, high = 0.975, title = "") {
	data_summary <- data[, .(best = sum(best)), by = c("year", "sim")]

	data_summary <- data_summary[, .(best_avg = mean(best),
																	 best_med = median(best),
																	 best_low = quantile(best, low),
																	 best_high = quantile(best, high)), by = c("year")]

	g <- ggplot(data_summary)
	g <- g + geom_step(aes(x = year, y = best_low, colour = "Low"))
	g <- g + geom_step(aes(x = year, y = best_high, colour = "High"))
	g <- g + geom_step(aes(x = year, y = best_med, colour = "Median"))
	g <- g + labs(x = "", y = "Battle-related Deaths", colour = "", title = title)
	g <- g + theme_bw()

	return(g)
}

plot_conflict <- function(data, low = 0.025, high = 0.975, title = "") {


	data_summary <- data[, .(conflict = sum(best > 25), nr = .N), by = c("year", "sim")]

	data_summary <- data_summary[, .(conflict_avg = mean(conflict/nr),
																	 conflict_med = median(conflict/nr),
																	 conflict_low = quantile(conflict/nr, low),
																	 conflict_high = quantile(conflict/nr, high)), by = c("year")]

	g <- ggplot(data_summary)
	g <- g + geom_step(aes(x = year, y = conflict_low, colour = "Low"))
	g <- g + geom_step(aes(x = year, y = conflict_high, colour = "High"))
	g <- g + geom_step(aes(x = year, y = conflict_med, colour = "Median"))
	g <- g + labs(x = "", y = "Battle-related Deaths > 25", colour = "", title = title)
	g <- g + theme_bw()

	return(g)
}

plot_type_of_conflict <- function(data, low = 0.025, high = 0.975, title = "") {


	data_summary <- data[, .(zero = sum(best < 25), minor = sum(25 <= best & best <= 1000), major = sum(best > 1000), nr = .N), by = c("year", "sim")]

	data_summary <- data_summary[, .(zero = mean(zero/nr),
																	 minor = mean(minor/nr),
																	 major = mean(major/nr)), by = c("year")]

	g <- ggplot(data_summary)
	g <- g + geom_step(aes(x = year, y = zero, colour = "Zero"))
	g <- g + geom_step(aes(x = year, y = minor, colour = "Minor"))
	g <- g + geom_step(aes(x = year, y = major, colour = "Major"))
	g <- g + labs(x = "", y = "Type of Conflict", colour = "", title = title)
	g <- g + theme_bw()

	return(g)
}




plot_conflict_summary <- function(data, filename = "test.pdf", path = "~/Dropbox/PRIO/undernourishment/R/figures/") {

	dir.create(path, recursive = TRUE, showWarnings = FALSE)

	plot_list <- list()

	g1 <- plot_battle(data = data, title = "Global")
	g2 <- plot_conflict(data = data)
	g3 <- plot_type_of_conflict(data = data)

	plot_list[[1]] <- g1
	plot_list[[2]] <- g2
	plot_list[[3]] <- g3

	region_list_ <- sort(unique(data$region))

	j <- 4
	for (i in seq_along(region_list_)) {

		region_i = region_list_[i]

		sim_i = data[region == region_i]
		g1_i <- plot_battle(data = sim_i, title = paste0("Region: ", region_i))
		g2_i <- plot_conflict(data = sim_i)
		g3_i <- plot_type_of_conflict(data = sim_i)

		plot_list[[j]] <- g1_i
		j <- j + 1

		plot_list[[j]] <- g2_i
		j <- j + 1

		plot_list[[j]] <- g3_i
		j <- j + 1

	}

	combined_plot <- plot_grid(plotlist = plot_list, ncol = 3)
	ggsave(paste0(path, "/", filename), plot = combined_plot, width = 10, height = 18)

}





plot_dem_avg <- function(data, low = 0.025, high = 0.975, title = "") {
	data_summary <- data[, .(pol = mean(pol)), by = c("year", "sim")]
	data_summary <- data_summary[, .(best_avg = mean(pol),
																	 best_med = median(pol),
																	 best_low = quantile(pol, low),
																	 best_high = quantile(pol, high)), by = c("year")]

	g <- ggplot(data_summary)
	g <- g + geom_step(aes(x = year, y = best_low, colour = "Low"))
	g <- g + geom_step(aes(x = year, y = best_high, colour = "High"))
	g <- g + geom_step(aes(x = year, y = best_med, colour = "Median"))
	g <- g + labs(x = "", y = "Average Polyarchy Score", colour = "", title = title)
	g <- g + theme_bw()

	return(g)
}

plot_dem <- function(data, low = 0.025, high = 0.975, title = "") {
	data_summary <- data[, .(avg = mean(pol),
													 med = median(pol),
													 low = quantile(pol, low),
													 high = quantile(pol, high)), by = c("year")]

	g <- ggplot(data_summary)
	g <- g + geom_step(aes(x = year, y = low, colour = "Low"))
	g <- g + geom_step(aes(x = year, y = high, colour = "High"))
	g <- g + geom_step(aes(x = year, y = med, colour = "Median"))
	g <- g + labs(x = "", y = "Average Polyarchy Score", colour = "", title = title)
	g <- g + theme_bw()

	return(g)
}

plot_dem_group <- function(data, group = seq(0, 1, 0.33), low = 0.025, high = 0.975, title = "") {

	data_summary <- foreach(i = seq_along(group)[-1], .combine = rbind) %do% {
		data_i <- data[, .(group_nr = sum(group[i - 1] < pol & pol < group[i]), nr = .N), by = c("year", "sim")]
		data_i[, group := group[i]]
	}

	data_summary <- data_summary[, .(avg = mean(group_nr/nr),
																	 med = median(group_nr/nr),
																	 low = quantile(group_nr/nr, low),
																	 high = quantile(group_nr/nr, high)), by = c("year", "group")]

	g <- ggplot(data_summary)
	g <- g + geom_step(aes(x = year, y = avg, colour = as.factor(group)))
	g <- g + labs(x = "", y = "Propotion of Countries", colour = "Score", title = title)
	g <- g + theme_bw()
	g <- g + ylim(c(0, 1))

	return(g)
}

plot_dem_summary <- function(data, filename = "test.pdf", path = "~/Dropbox/PRIO/undernourishment/R/figures/") {

	dir.create(path, recursive = TRUE, showWarnings = FALSE)

	plot_list <- list()

	g1 <- plot_dem_avg(data = data, title = "Global")
	g2 <- plot_dem_group(data = data, title = "")

	plot_list[[1]] <- g1
	plot_list[[2]] <- g2

	region_list_ <- sort(unique(data$region))

	for (i in seq_along(region_list_)) {

		region_i = region_list_[i]

		data_i = data[region == region_i]
		g1_i <- plot_dem_avg(data = data_i, title = paste0("Region: ", region_i))
		g2_i <- plot_dem_group(data = data_i)

		plot_list[[2*i - 1 + 2]] <- g1_i
		plot_list[[2*i + 2]] <- g2_i


	}

	combined_plot <- plot_grid(plotlist = plot_list, ncol = 2)
	print(combined_plot)

	ggsave(paste0(path, "/", filename), plot = combined_plot, width = 10, height = 18)

}










