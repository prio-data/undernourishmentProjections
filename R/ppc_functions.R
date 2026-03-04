# ------------------------------------------------------------
# PPC for polyarchy change process
# ------------------------------------------------------------
# data_sim   : data.table with columns gwcode, pol
# data_train : data.table used inside sim_polyarchy()
# Nsim       : number of simulations per country
# returns    : list(results, summary)
# ------------------------------------------------------------

ppc_polyarchy <- function(data_sim, data_train, Nsim = 1000) {

	stopifnot(
		is.data.table(data_sim),
		is.data.table(data_train),
		all(c("gwcode", "pol") %in% names(data_sim)),
		is.numeric(Nsim),
		Nsim > 0
	)

	gwcode_list <- sort(unique(data_sim$gwcode))

	# ---- helper
	compute_metrics <- function(y) {

		dy <- diff(y)
		if (length(dy) < 3) return(NULL)

		list(
			mean = mean(dy),
			var  = stats::var(dy),
			acf1 = stats::acf(dy, plot = FALSE, lag.max = 1)$acf[2]
		)
	}

	# ---- main PPC loop
	results <- foreach(gwcode_i = gwcode_list,
										 .combine = rbind,
										 .errorhandling = "remove") %dopar% {

										 	data_i <- data_sim[gwcode == gwcode_i]
										 	if (nrow(data_i) < 6) return(NULL)

										 	y_obs <- data_i$pol

										 	emp <- compute_metrics(y_obs)
										 	if (is.null(emp)) return(NULL)

										 	sim_data <- sim_polyarchy(
										 		n = length(y_obs),
										 		N = Nsim,
										 		polyarchy_start = y_obs[1],
										 		polyarchy_end   = y_obs[length(y_obs)],
										 		polyarchy_data  = data_train[gwcode == gwcode_i],
										 		alpha = 1.0
										 	)

										 	Y_sim <- split(sim_data$pol, sim_data$sim)

										 	sim_metrics <- lapply(Y_sim, compute_metrics)
										 	sim_metrics <- rbindlist(sim_metrics)
										 	sim_metrics <- sim_metrics[complete.cases(sim_metrics)]

										 	if (nrow(sim_metrics) < 20) return(NULL)

										 	mean_lo <- quantile(sim_metrics$mean, 0.05)
										 	mean_hi <- quantile(sim_metrics$mean, 0.95)

										 	var_lo  <- quantile(sim_metrics$var, 0.05)
										 	var_hi  <- quantile(sim_metrics$var, 0.95)

										 	acf_lo  <- quantile(sim_metrics$acf1, 0.05)
										 	acf_hi  <- quantile(sim_metrics$acf1, 0.95)

										 	data.table(
										 		gwcode = gwcode_i,

										 		mean_emp = emp$mean,
										 		var_emp  = emp$var,
										 		acf_emp  = emp$acf1,

										 		mean_sim_avg = mean(sim_metrics$mean),
										 		var_sim_avg  = mean(sim_metrics$var),
										 		acf_sim_avg  = mean(sim_metrics$acf1),

										 		mean_lo = mean_lo,
										 		mean_hi = mean_hi,
										 		var_lo  = var_lo,
										 		var_hi  = var_hi,
										 		acf_lo  = acf_lo,
										 		acf_hi  = acf_hi,

										 		mean_ok = emp$mean >= mean_lo & emp$mean <= mean_hi,
										 		var_ok  = emp$var  >= var_lo  & emp$var  <= var_hi,
										 		acf_ok  = emp$acf1 >= acf_lo  & emp$acf1 <= acf_hi
										 	)
										 }

	summary_table <- results[, .(
		mean_pass = mean(mean_ok),
		var_pass  = mean(var_ok),
		acf_pass  = mean(acf_ok)
	)]

	list(results = results, summary = summary_table)
}


# ------------------------------------------------------------
# Function: simulate and prepare plot data
# ------------------------------------------------------------

plot_case <- function(gwcode_i, N_plot = 200) {

	data_i <- data_sim[gwcode == gwcode_i]

	if (nrow(data_i) == 0) return(NULL)

	y_obs  <- data_i$pol
	year   <- data_i$year
	country_name_i <- unique(data_i$country_name)[1]

	# Simulate
	data_sim_i <- sim_polyarchy(
		n = length(y_obs),
		N = N_plot,
		polyarchy_start = y_obs[1],
		polyarchy_end   = y_obs[length(y_obs)],
		polyarchy_data  = data_train[gwcode == gwcode_i],
		alpha = 1.0
	)

	# Convert simulated to long format
	sim_dt <- data.table(
		year = rep(year, N_plot),
		pol  = data_sim_i$pol,
		sim  = factor(data_sim_i$sim)
	)

	obs_dt <- data.table(
		year = year,
		pol  = y_obs
	)

	list(
		sim = sim_dt,
		obs = obs_dt,
		country = country_name_i
	)
}

plot_cases <- function(gwcodes, N_plot = 200, save_path = "democracy_simulation_examples.png") {

	plot_list <- lapply(gwcodes, plot_case, N_plot = N_plot)
	plot_list <- plot_list[!sapply(plot_list, is.null)]

	sim_dt <- rbindlist(lapply(seq_along(plot_list), function(i) {
		cbind(plot_list[[i]]$sim, country = plot_list[[i]]$country)
	}))

	obs_dt <- rbindlist(lapply(seq_along(plot_list), function(i) {
		cbind(plot_list[[i]]$obs, country = plot_list[[i]]$country)
	}))

	# Plot
	p <- ggplot() +

		geom_step(
			data = sim_dt,
			aes(year, pol, group = interaction(sim, country)),
			color = "grey40",
			alpha = 0.35
		) +

		geom_step(
			data = obs_dt,
			aes(year, pol),
			color = "red",
			linewidth = 1.0
		) +

		facet_wrap(~country, ncol = 1) +

		scale_y_continuous(
			limits = c(0,1),
			name = "Democracy score"
		) +

		scale_x_continuous(
			name = "Year"
		) +

		ggtitle("Observed and simulated democracy trajectories") +

		theme_minimal(base_size = 14)

	print(p)

	ggsave(
		filename = save_path,
		plot = p,
		width = 8,
		height = 10,
		dpi = 300
	)
}


# ------------------------------------------------------------
# PPC for NB-DINARCH count process
# ------------------------------------------------------------
# data : data.table with gwcode, best, population
# fit  : parameter table with gwcode, a, b, c
# S    : simulations per country
# returns : list(ppc_table, summary)
# ------------------------------------------------------------

ppc_nb_dinarch <- function(data, fit, S = 1000) {

	stopifnot(
		is.data.table(data),
		is.data.table(fit),
		all(c("gwcode", "best", "population") %in% names(data)),
		all(c("gwcode", "a", "b", "c") %in% names(fit)),
		is.numeric(S),
		S > 0
	)

	gwcode_list <- sort(unique(data$gwcode))

	# ---- empirical metrics
	emp_metrics <- data[, .(
		mean_emp = mean(best),
		var_emp  = var(best),
		od_emp   = var(best) / mean(best),
		zi_emp   = mean(best == 0)
	), by = gwcode]

	# ---- simulations
	sim <- foreach(gwcode_i = gwcode_list, .combine = rbind) %dopar% {

		data_i <- data[gwcode == gwcode_i]
		fit_i  <- fit[gwcode == gwcode_i]

		y_i <- data_i$best
		x_i <- data_i$population

		foreach(s = 1:S, .combine = rbind) %do% {

			y_sim_i <- nb_dinarch_x_sim(
				n     = length(y_i),
				a     = fit_i$a,
				b     = fit_i$b,
				c     = fit_i$c,
				x     = x_i,
				m     = 0,
				start = y_i[1]
			)

			data.table(
				gwcode = gwcode_i,
				sim    = s,
				best   = y_sim_i
			)
		}
	}

	# ---- simulated metrics
	sim_metrics <- sim[, .(
		mean_sim = mean(best, na.rm = TRUE),
		var_sim  = var(best, na.rm = TRUE),
		od_sim   = var(best, na.rm = TRUE) / mean(best, na.rm = TRUE),
		zi_sim   = mean(best == 0)
	), by = .(gwcode, sim)]

	# ---- PPC intervals
	sim_ci <- sim_metrics[, .(
		mean_lo = quantile(mean_sim, 0.05, na.rm = TRUE),
		mean_hi = quantile(mean_sim, 0.95, na.rm = TRUE),
		var_lo  = quantile(var_sim, 0.05, na.rm = TRUE),
		var_hi  = quantile(var_sim, 0.95, na.rm = TRUE),
		od_lo   = quantile(od_sim, 0.05, na.rm = TRUE),
		od_hi   = quantile(od_sim, 0.95, na.rm = TRUE),
		zi_lo   = quantile(zi_sim, 0.05, na.rm = TRUE),
		zi_hi   = quantile(zi_sim, 0.95, na.rm = TRUE)
	), by = gwcode]

	sim_avg <- sim_metrics[, .(
		mean_sim_avg = mean(mean_sim),
		var_sim_avg  = mean(var_sim),
		od_sim_avg   = mean(od_sim),
		zi_sim_avg   = mean(zi_sim)
	), by = gwcode]

	ppc_dt <- Reduce(function(x,y) merge(x,y,by="gwcode"),
									 list(emp_metrics, sim_ci, sim_avg))

	# ---- GOF indicators
	ppc_dt[, `:=`(
		mean_ok = mean_emp >= mean_lo & mean_emp <= mean_hi,
		var_ok  = var_emp  >= var_lo  & var_emp  <= var_hi,
		od_ok   = od_emp   >= od_lo   & od_emp   <= od_hi,
		zi_ok   = zi_emp   >= zi_lo   & zi_emp   <= zi_hi
	)]

	summary_table <- ppc_dt[, .(
		mean_pass = mean(mean_ok, na.rm = TRUE),
		var_pass  = mean(var_ok, na.rm = TRUE),
		od_pass   = mean(od_ok, na.rm = TRUE),
		zi_pass   = mean(zi_ok, na.rm = TRUE)
	)]

	list(ppc_table = ppc_dt, summary = summary_table)
}
