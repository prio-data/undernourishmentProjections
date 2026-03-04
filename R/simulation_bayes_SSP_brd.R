library(data.table)
library(ggplot2)
library(ggpubr)
library(foreach)
library(doMC)
library(plotly)
library(splines)
library(rstan)

registerDoMC(10)

path_sim <- "data/simbayes/"

source(file = "R/simulation_functions.R")
source(file = "R/simulation_functions_plot.R")
source(file = "R/ppc_functions.R")

dinarch_stan_model_code <- "
  data {
    int<lower=1> N;
    int<lower=0> y[N];
    vector[N] x;
    int<lower=1> k;
    matrix[N, k] ylags;
  }
  parameters {
    real a;
    vector<lower=0, upper=1>[k] b;
    real<lower=0> c;
  }
  transformed parameters {
    vector[N] r;
    vector[N] mu;
    for (n in 1:N) {
      mu[n] = exp(a) * x[n];
      for (j in 1:k) {
        mu[n] += b[j] * ylags[n, j];
      }
      r[n] = c * mu[n];
    }
  }
  model {
    a ~ normal(0, 3);
    b ~ beta(1, 3);
    c ~ gamma(1, 10);

    for (n in 1:N) {
      y[n] ~ neg_binomial(r[n], c / (c + 1));
    }
  }
"

dinarch_model <- stan_model(model_code = dinarch_stan_model_code)

make_stan_data <- function(Y, k = 1) {
	stopifnot(is.data.table(Y))
	stopifnot(c("y", "group", "index", "x") %in% names(Y))

	setorder(Y, group, index)

	for (i in 1:k) {
		Y[, paste0("y", i) := shift(y, n = i, type = "lag"), by = group]
	}

	Yc <- na.omit(Y)
	ylags_mat <- as.matrix(Yc[, paste0("y", 1:k), with = FALSE])

	list(
		N = nrow(Yc),
		y = Yc$y,
		x = Yc$x,
		k = k,
		ylags = ylags_mat
	)
}



# Data -------------------------------------------------------------------------
filename <- "data/main.csv"
data_raw <- fread(file = filename)

data <- copy(data_raw)

gwcode_list_ = sort(unique(data$gwcode))

# check for gaps in data
data <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %do% {
	data_i <- data[gwcode == gwcode_i]

	data_i <- data_i[!is.na(gdppc)]
	data_i <- data_i[!is.na(best)]
	data_i[, TMP := c(1, diff(year))]

	if (nrow(data_i[TMP != 1]) > 0 ) {
		print(gwcode_i)
		data_i <- NULL
	}
	return(data_i)
}


# check if we see the same number of best and best > 0 in following years
data_tmp <- data[, TMP := (c(0, diff(best)) == 0) & (best > 1), by = "gwcode"][, sum(TMP), by = "year"]

data <- data[year >= 1989]
data <- data[, .(gwcode, year, best = floor(best), gdppc, country_name, population)]

# TODO: Make the below into a function.
source("R/test_add_region.R")

data <- data[region != "NA"]


gwcode_list_ <- unique(data$gwcode)

fit_bayes <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	print(gwcode_i)

	data_i <- data[gwcode == gwcode_i, .(group = 0, index = year, y = best, x = population)]
	return_i = data.table(gwcode = gwcode_i, a = NA, b = NA, c = NA)

	if (sum(data_i$y) > 0 & sum(data_i$y != 0) > 1) {

		fit_i <- NULL
		data_i <- make_stan_data(data_i, k = 1)

		try(fit_i <- sampling(
			dinarch_model,
			data = data_i,
			iter = 5000,
			chains = 3,
			seed = 42
		))

		# try(fit_i <- nb_dinarch_bayes_fit(data_i, k = 1))

		if (is.null(fit_i)) {
			return_i = data.table(gwcode = gwcode_i, a = NA, b = NA, c = NA)
		} else {
			summary_i <- summary(fit_i, pars = c("a", "b", "c"))$summary
			return_i = data.table(gwcode = gwcode_i,
														a = summary_i[1, 1],
														b = summary_i[2, 1],
														c = summary_i[3, 1])
		}
	}
	return(return_i)
}


fit_ml <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	data_i <- data[gwcode == gwcode_i, .(group = 0, index = year, y = best, x = population)]
	return_i = data.table(gwcode = gwcode_i, a = NA, b = NA, c = NA)

	if (sum(data_i$y) > 0 & sum(data_i$y != 0) > 1) {

		fit_i <- NULL
		try(fit_i <- nb_dinarch_x_fit(Y = data_i, k = 1, lambda = 0.1))

		if (is.null(fit_i)) {
			return_i = data.table(gwcode = gwcode_i, a = NA, b = NA, c = NA)
		} else {
			return_i = data.table(gwcode = gwcode_i, a = fit_i$a, b = fit_i$b, c = fit_i$c)
		}
	}

	return(return_i)
}


# LOW --------------------------------------------------------------------------
data_low <- data[gwcode %in% fit_bayes[is.na(a)]$gwcode]
data_low <- data_low[, .(group = gwcode, index = year, y = best, x = population)]
data_low_ <- make_stan_data(data_low, k = 1)

try(fit_i <- sampling(
	dinarch_model,
	data = data_low_,
	iter = 5000,
	chains = 3,
	seed = 42
))

summary_i <- summary(fit_i, pars = c("a", "b", "c"))$summary
fit_low_ <- list(a = summary_i[1, 1], b = summary_i[2, 1], c = summary_i[3, 1])

fit_bayes[gwcode %in% data_low$group]$a <- summary_i[1, 1]
fit_bayes[gwcode %in% data_low$group]$b <- summary_i[2, 1]
fit_bayes[gwcode %in% data_low$group]$c <- summary_i[3, 1]
# ------------------------------------------------------------------------------



# High -------------------------------------------------------------------------
# political stable and high GDP and/or high growth similar to the democracy
data_gdppc_summary <- data[, .(gdppc_avg = mean(gdppc)), by = "gwcode"]
gdppc_quantile <- quantile(data_gdppc_summary$gdppc_avg, c(0.75))
data_gdppc_high <- data[gwcode %in% data_gdppc_summary[gdppc_quantile <= gdppc_avg]$gwcode]
data_gdppc_high <- data_gdppc_high[, .(group = gwcode, index = year, y = best, x = population)]
data_gdppc_high_ <- make_stan_data(data_gdppc_high, k = 1)

try(fit_i <- sampling(
	dinarch_model,
	data = data_gdppc_high_,
	iter = 5000,
	chains = 3,
	seed = 42
))

fit_high_ <- summary(fit_i, pars = c("a", "b", "c"))$summary
fit_high_ <- list(a = fit_high_[1, 1], b = fit_high_[2, 1], c = fit_high_[3, 1])
fit_high_ml <- nb_dinarch_x_fit(Y = data_gdppc_high, k = 1, lambda = 0.1)
# ------------------------------------------------------------------------------



# High Conflict ----------------------------------------------------------------
data_best_summary <- data[, .(best_avg = sum(best, na.rm = TRUE)/sum(best != 0, na.rm = TRUE)), by = "gwcode"]
# best_quantile <- quantile(data_best_summary$best_avg, c(0.75), na.rm = TRUE)
# best_quantile <- quantile(data_best_summary$best_avg, c(0.25), na.rm = TRUE)
data_gdppc_high <- data[gwcode %in% data_best_summary[1 <= best_avg]$gwcode] # at least 1 BRD
data_gdppc_high <- data_gdppc_high[, .(group = gwcode, index = year, y = best, x = population)]
data_gdppc_high_ <-  make_stan_data(data_gdppc_high, k = 1)


try(fit_i <- sampling(
	dinarch_model,
	data = data_gdppc_high_,
	iter = 5000,
	chains = 3,
	seed = 42
))

fit_long_term_ <- summary(fit_i, pars = c("a", "b", "c"))$summary
fit_long_term_ <- list(a = fit_long_term_[1, 1], b = fit_long_term_[2, 1], c = fit_long_term_[3, 1])
fit_long_term_ml <- nb_dinarch_x_fit(Y = data_gdppc_high, k = 1, lambda = 0.1)
# ------------------------------------------------------------------------------





# Parameters
S <- 500
year_max <- 2050
gwcode_list_ <- sort(unique(data$gwcode))


# PPC Goodness-of-fit Analysis -------------------------------------------------
res <- ppc_nb_dinarch(data = data, fit = fit_bayes, S = 1000)
print(res$summary)
# ------------------------------------------------------------------------------



# SSP1 -------------------------------------------------------------------------
set.seed(1)

a_end <- fit_low_$a
b_end <- fit_low_$b
c_end <- fit_low_$c

sim_ssp1 <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	y_sim <- foreach(s = 1:S, .combine = rbind) %do% {

		data_i <- data[gwcode == gwcode_i]
		data_i <- data_i[1:(.N - sample(5, 1) - 1)]
		fit_i <- fit_bayes[gwcode == gwcode_i]
		year_i <- data[gwcode == gwcode_i]$year
		region_i <- first(data[gwcode == gwcode_i]$region)

		year_sim <- max(data_i$year + 1):year_max
		N <- length(year_sim)

		y_i <- data_i$best
		x_i <- data_i$population
		x_sim_i <- rep(last(x_i), N)

		a_sim <- tmp_function(a = fit_i$a, b = a_end, N = N)
		b_sim <- pmax(tmp_function(a = fit_i$b, b = b_end, N = N), 0)
		c_sim <- pmax(tmp_function(a = fit_i$c, b = c_end, N = N), 0)


		y_sim_i <- nb_dinarch_x_sim(n = N,
																a = a_sim,
																b = b_sim,
																c = c_sim,
																x = x_sim_i,
																m = 0,
																start = last(y_i))

		return_i = data.table(best = c(y_i, y_sim_i), year = min(year_i):year_max, sim = s, region = region_i)
	}

	y_sim[, gwcode := gwcode_i]

}

# plot
plot_conflict_summary(data = sim_ssp1, filename = "sim_summary_ssp1_brd.pdf", path = path_sim)
if (!is.null(path_sim)) fwrite(sim_ssp1, file = paste0(path_sim, "/ssp1_brd.csv"))
rm(sim_ssp1)
# ------------------------------------------------------------------------------




# SSP2 -------------------------------------------------------------------------
set.seed(1)

sim_ssp2 <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	y_sim <- foreach(s = 1:S, .combine = rbind) %do% {

		set.seed(s)

		data_i <- data[gwcode == gwcode_i]
		data_i <- data_i[1:(.N - sample(5, 1) - 1)]
		fit_i <- fit_bayes[gwcode == gwcode_i]
		year_i <- data[gwcode == gwcode_i]$year
		region_i <- first(data[gwcode == gwcode_i]$region)

		year_sim <- max(data_i$year + 1):year_max
		N <- length(year_sim)

		y_i <- data_i$best
		x_i <- data_i$population
		x_sim_i <- rep(last(x_i), N)

		a_sim <- fit_i$a
		b_sim <- fit_i$b
		c_sim <- fit_i$c

		y_sim_i <- nb_dinarch_x_sim(n = N,
																a = a_sim,
																b = b_sim,
																c = c_sim,
																x = x_sim_i,
																m = 0,
																start = last(y_i))

		return_i = data.table(best = c(y_i, y_sim_i), year = min(year_i):year_max, sim = s, region = region_i)
	}

	y_sim[, gwcode := gwcode_i]
}

plot_conflict_summary(data = sim_ssp2, filename = "sim_summary_ssp2_brd.pdf", path = path_sim)
if (!is.null(path_sim)) fwrite(sim_ssp2, file = paste0(path_sim, "/ssp2_brd.csv"))
rm(sim_ssp2)
# ------------------------------------------------------------------------------





# SSP3 -------------------------------------------------------------------------
set.seed(1)

a_end <- fit_long_term_$a
b_end <- fit_long_term_$b
c_end <- fit_long_term_$c

sim_ssp3 <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	y_sim <- foreach(s = 1:S, .combine = rbind) %do% {

		data_i <- data[gwcode == gwcode_i]
		data_i <- data_i[1:(.N - sample(5, 1) - 1)]
		fit_i <- fit_bayes[gwcode == gwcode_i]
		year_i <- data[gwcode == gwcode_i]$year
		region_i <- first(data[gwcode == gwcode_i]$region)

		year_sim <- max(data_i$year + 1):year_max
		N <- length(year_sim)

		y_i <- data_i$best
		x_i <- data_i$population
		x_sim_i <- rep(last(x_i), N)

		a_sim <- pmax(fit_i$a, tmp_function(a = fit_i$a, b = a_end, N = N))
		b_sim <- pmax(fit_i$b, tmp_function(a = fit_i$b, b = b_end, N = N))
		c_sim <- pmin(fit_i$c, tmp_function(a = fit_i$c, b = c_end, N = N))

		y_sim_i <- nb_dinarch_x_sim(n = N,
																a = a_sim,
																b = b_sim,
																c = c_sim,
																x = x_sim_i,
																m = 0,
																start = last(y_i))

		return_i = data.table(best = c(y_i, y_sim_i), year = min(year_i):year_max, sim = s, region = region_i)
	}

	y_sim[, gwcode := gwcode_i]
}

# plot
plot_conflict_summary(data = sim_ssp3, filename = "sim_summary_ssp3_brd.pdf", path = path_sim)
if (!is.null(path_sim)) fwrite(sim_ssp3, file = paste0(path_sim, "/ssp3_brd.csv"))
rm(sim_ssp3)
# ------------------------------------------------------------------------------


# SSP4 -------------------------------------------------------------------------
data_gdppc <- data[2010 <= year & year <= 2023, .(gdppc = mean(gdppc, na.rm = TRUE)), by = c("region", "gwcode")]
data_gdppc_quantile <- data_gdppc[, .(quantile = quantile(gdppc, 0.75, na.rm = TRUE)), by = "region"]

data[, is_rich := 0]

for (region_i in unique(data_gdppc$region)) {
	gwcode_i <- data_gdppc[region == region_i & gdppc >= data_gdppc_quantile[region == region_i]$quantile]$gwcode
	data[gwcode %in% gwcode_i, is_rich := 1]
}

sim_ssp4 <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	y_sim <- foreach(s = 1:S, .combine = rbind) %do% {

		data_i <- data[gwcode == gwcode_i]
		data_i <- data_i[1:(.N - sample(5, 1) - 1)]
		fit_i <- fit_bayes[gwcode == gwcode_i]
		year_i <- data[gwcode == gwcode_i]$year
		region_i <- first(data[gwcode == gwcode_i]$region)
		is_rich_i <- first(data[gwcode == gwcode_i]$is_rich)

		year_sim <- max(data_i$year + 1):year_max
		N <- length(year_sim)

		y_i <- data_i$best
		x_i <- data_i$population
		x_sim_i <- rep(last(x_i), N)

		# SSP5
		a_sim <- tmp_function(a = fit_i$a, b = fit_high_$a, N = N)
		b_sim <- pmax(tmp_function(a = fit_i$b, b = fit_high_$b, N = N), 0)
		c_sim <- pmax(tmp_function(a = fit_i$c, b = fit_high_$c, N = N), 0)


		if (is_rich_i == 0) {
			a_sim <- pmax(fit_i$a, tmp_function(a = fit_i$a, b = a_end, N = N))
			b_sim <- pmax(fit_i$b, tmp_function(a = fit_i$b, b = b_end, N = N))
			c_sim <- pmin(fit_i$c, tmp_function(a = fit_i$c, b = c_end, N = N))
		}

		y_sim_i <- nb_dinarch_x_sim(n = N,
																a = a_sim,
																b = b_sim,
																c = c_sim,
																x = x_sim_i,
																m = 0,
																start = last(y_i))

		return_i = data.table(best = c(y_i, y_sim_i), year = min(year_i):year_max, sim = s, region = region_i)
	}

	y_sim[, gwcode := gwcode_i]
}

plot_conflict_summary(data = sim_ssp4, filename = "sim_summary_ssp4_brd.pdf", path = path_sim)
if (!is.null(path_sim)) fwrite(sim_ssp4, file = paste0(path_sim, "/ssp4_brd.csv"))
rm(sim_ssp4)
# ------------------------------------------------------------------------------




# SSP5 -------------------------------------------------------------------------
a_end <- fit_high_$a
b_end <- fit_high_$b
c_end <- fit_high_$c

sim_ssp5 <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %dopar% {

	y_sim <- foreach(s = 1:S, .combine = rbind) %do% {

		data_i <- data[gwcode == gwcode_i]
		data_i <- data_i[1:(.N - sample(5, 1) - 1)]
		fit_i <- fit_bayes[gwcode == gwcode_i]
		year_i <- data[gwcode == gwcode_i]$year
		region_i <- first(data[gwcode == gwcode_i]$region)

		year_sim <- max(data_i$year + 1):year_max
		N <- length(year_sim)

		y_i <- data_i$best
		x_i <- data_i$population
		x_sim_i <- rep(last(x_i), N)

		a_sim <- tmp_function(a = fit_i$a, b = a_end, N = N)
		b_sim <- pmax(tmp_function(a = fit_i$b, b = b_end, N = N), 0)
		c_sim <- pmax(tmp_function(a = fit_i$c, b = c_end, N = N), 0)

		y_sim_i <- nb_dinarch_x_sim(n = N,
																a = a_sim,
																b = b_sim,
																c = c_sim,
																x = x_sim_i,
																m = 0,
																start = last(y_i))

		return_i = data.table(best = c(y_i, y_sim_i), year = min(year_i):year_max, sim = s, region = region_i)
	}

	y_sim[, gwcode := gwcode_i]
}

# plot
plot_conflict_summary(data = sim_ssp5, filename = "sim_summary_ssp5_brd.pdf", path = path_sim)
if (!is.null(path_sim)) fwrite(sim_ssp5, file = paste0(path_sim, "/ssp5_brd.csv"))
rm(sim_ssp5)
# ------------------------------------------------------------------------------


















if (FALSE) {
	# Debug, plotting and stuff
	#

	gwcode_list_ <- fit_ml[!is.na(a)]$gwcode
	gwcode_list_ <- 530

	S <- 100
	year_max <- 2050
	sim_ml <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %do% {

		y_sim <- foreach(s = 1:S, .combine = rbind) %do% {

			data_i <- data[gwcode == gwcode_i]
			data_i <- data_i[1:(.N - sample(5, 1) - 1)]
			fit_i <- fit_ml[gwcode == gwcode_i]
			year_i <- data[gwcode == gwcode_i]$year
			region_i <- first(data[gwcode == gwcode_i]$region)

			year_sim <- max(data_i$year + 1):year_max
			N <- length(year_sim)

			y_i <- data_i$best
			x_i <- data_i$population
			x_sim_i <- rep(last(x_i), N)

			a_sim <- fit_i$a
			b_sim <- fit_i$b
			c_sim <- fit_i$c


			y_sim_i <- nb_dinarch_x_sim(n = N,
																	a = a_sim,
																	b = b_sim,
																	c = c_sim,
																	x = x_sim_i,
																	m = 0,
																	start = last(y_i))

			return_i = data.table(best = c(y_i, y_sim_i),
														year = min(year_i):year_max,
														sim = s,
														region = region_i)
		}

		y_sim[, gwcode := gwcode_i]

	}

	sim_ml[, type := "ml"]



	sim_bayes <- foreach(gwcode_i = gwcode_list_, .combine = rbind) %do% {

		y_sim <- foreach(s = 1:S, .combine = rbind) %do% {

			data_i <- data[gwcode == gwcode_i]
			data_i <- data_i[1:(.N - sample(5, 1) - 1)]
			fit_i <- fit_bayes[gwcode == gwcode_i]
			year_i <- data[gwcode == gwcode_i]$year
			region_i <- first(data[gwcode == gwcode_i]$region)

			year_sim <- max(data_i$year + 1):year_max
			N <- length(year_sim)

			y_i <- data_i$best
			x_i <- data_i$population
			x_sim_i <- rep(last(x_i), N)

			a_sim <- fit_i$a
			b_sim <- fit_i$b
			c_sim <- fit_i$c


			y_sim_i <- nb_dinarch_x_sim(n = N,
																	a = a_sim,
																	b = b_sim,
																	c = c_sim,
																	x = x_sim_i,
																	m = 0,
																	start = last(y_i))

			return_i = data.table(best = c(y_i, y_sim_i),
														year = min(year_i):year_max,
														sim = s,
														region = region_i)
		}

		y_sim[, gwcode := gwcode_i]

	}

	sim_bayes[, type := "ml"]

	s <- 2
	k <- 1
	g <- ggplot(sim_bayes[gwcode == gwcode_list_[k]])
	g <- g + geom_step(aes(x = year, y = best, colour = as.factor(sim)))
	print(ggplotly(g, dynamicTicks = TRUE))


	g <- ggplot(sim_ml[gwcode == gwcode_list_[k]])
	g <- g + geom_step(aes(x = year, y = best, colour = as.factor(sim)))
	print(ggplotly(g, dynamicTicks = TRUE))

	fit_bayes[gwcode == 530]
	fit_ml[gwcode == 530]

}







