# Functions for simulation of polyarchy ----------------------------------------
sigmoid <- function(x) {
	return(1/(1 + exp(-x)))
}

sigmoid_inv <- function(y) {
	return(-log(1/y - 1) )
}

sim_polyarchy <- function(n, N, polyarchy_start, polyarchy_data, polyarchy_trans_seq = NULL, polyarchy_end = NULL, alpha = 0.1) {

	if (is.null(polyarchy_end)) {
		polyarchy_end <- polyarchy_start
	}

	pol_delta <- 0.1

	pol_start <- polyarchy_start
	pol_start_trans <- sigmoid_inv(pol_start)
	pol_end <- polyarchy_end
	pol_end_trans <- sigmoid_inv(pol_end)

	pol_seq <- seq(pol_start_trans, pol_end_trans, length.out = n + 1)
	pol_delta_trans_seq <- diff(pol_seq)


	polyarchy_high <- polyarchy_data[pol > 1 - pol_delta]
	polyarchy_low <- polyarchy_data[pol < pol_delta]


	sim <- foreach(i = 1:N, .combine = rbind) %do% {

		pol_seq_i <- 1:n + NA
		pol_trans_i <- 1:n + NA

		pol_j <- polyarchy_start

		for (j in 1:n) {

			if (j > 1) pol_j <- pol_seq_i[j - 1]

			pol_trans_j <- sigmoid_inv(pol_j)
			pol_trans_end_delta_j <- (pol_end_trans - pol_trans_j) * (0.25 - (j/(n + 10) - 0.5)^2) * alpha

			pol_train_trans_j <- polyarchy_data[(pol - pol_delta <= pol_j) & (pol_j <= pol + pol_delta)]$pol_trans_delta

			if (length(pol_train_trans_j) == 0) {
				if (pol_j > pol_high) {
					pol_trans_delta_j <- sample(polyarchy_data$pol_trans_delta, 1)
				} else if (pol_j < pol_low) {
					pol_trans_delta_j <- sample(polyarchy_data$pol_trans_delta, 1)
				} else {
					pol_trans_delta_j <- sample(polyarchy_data$pol_trans_delta, 1)
				}
			} else {
				pol_trans_delta_j <- sample(pol_train_trans_j, 1)
			}

			pol_trans_next <- pol_trans_j + sample(c(-1, 1), 1) * pol_trans_delta_j + pol_trans_end_delta_j
			# pol_trans_next <- pol_trans_j + sample(c(-1, 1), 1)*0.2 + pol_trans_end_delta_j

			pol_trans_i[j] <- pol_trans_next
			pol_next <- sigmoid(pol_trans_next)
			pol_seq_i[j] <- pol_next

			if (pol_seq_i[j] == 1) {
				pol_seq_i[j] <- pol_seq_i[j] - 1e-6
				cat("Warning: ...")
			}


		}

		return(data.table(pol = pol_seq_i, index = 1:n, sim = i))
	}
	return(sim)
}
# ------------------------------------------------------------------------------






# Functions for simulation of conflict -----------------------------------------

nb_dinarch_fit <- function(Y, k = 1, start = NULL, a_null = FALSE) {

	stopifnot(is.data.table(Y))
	stopifnot(c("y", "group", "index") %in% names(Y))

	Y <- Y[, .(group, index, y)]

	y_k_times_b <- ""

	if (k > 0) {
		Y[, tmp := y]
		Y[, tmp := c(NA, tmp[-.N]), by = "group"]
		eval(parse(text = "Y[, y1 := tmp]"))

		y_k_times_b <- paste0(y_k_times_b, "y1*b[1]")

		if (k > 1) {
			for (i in 2:k) {
				Y[, tmp := c(NA, tmp[-.N]), by = "group"]
				eval(parse(text = paste0("Y[, y", i, " := tmp]")))

				y_k_times_b <- paste0(y_k_times_b, " + y", i, "*b[", i, "]")
			}
		}
		Y[, tmp := NULL]
	}
	# y_k_times_b <- paste0(y_k_times_b, ")")

	# fit with optim(...)
	nb_dinarch_mll <- function(par) {

		# print(par)

		a <- exp(par[1])
		b <- 1/(1 + exp(-par[1 + 1:k]))
		c <- exp(par[1 + k + 1])

		# r <- c*(a + y_k%*%b)
		# p <- c/(1 + c)

		code = paste0("Y[, r := c*(a + ", y_k_times_b, ")]")
		eval(parse(text = code))
		Y[, p :=  c/(1 + c)]

		r_min = min(Y[, r], na.rm = TRUE)
		p <- c/(1 + c)

		if (r_min > 0 && 0 < p && p < 1 && sum(b) < 1) {
			return(-sum(Y[, loglik := dnbinom(x = y, size = r, prob = p, log = TRUE)]$loglik, na.rm = TRUE))
		} else {
			return(1e6)
		}
	}

	if (a_null) {
		nb_dinarch_mll <- function(par) {

			b <- 1/(1 + exp(-par[0 + 1:k]))
			c <- exp(par[0 + k + 1])

			code = paste0("Y[, r := c*(1e-5 + ", y_k_times_b, ")]")
			eval(parse(text = code))
			Y[, p :=  c/(1 + c)]

			r_min = min(Y[, r], na.rm = TRUE)
			p <- c/(1 + c)

			if (r_min > 0 && 0 < p && p < 1 && sum(b) < 1) {
				return(-sum(Y[, loglik := dnbinom(x = y, size = r, prob = p, log = TRUE)]$loglik, na.rm = TRUE))
			} else {
				return(1e6)
			}
		}
	}

	if (is.null(start)) {
		start <- c(0, rep(-1, k), -1)
	}

	fit <- optim(par = start, fn = nb_dinarch_mll, method = "Nelder-Mead")
	# fit <- nlm(p = fit$par, f = nb_dinarch_mll, iterlim = 1000, gradtol = 1e-9)


	a_hat <- exp(fit$par[1])
	b_hat <- 1/(1 + exp(-fit$par[1 + 1:k]))
	c_hat <- exp(fit$par[1 + k + 1])

	if (a_null) {
		a_hat <- 1e-5
		b_hat <- 1/(1 + exp(-fit$par[0 + 1:k]))
		c_hat <- exp(fit$par[0 + k + 1])
	}


	loglik_max <- -fit$value

	return(list(loglik_max = loglik_max,
							a = a_hat,
							b = b_hat,
							c = c_hat))

}

nb_dinarch_x_fit <- function(Y, k = 1, start = NULL, lambda = 0.5) {

	stopifnot(is.data.table(Y))
	stopifnot(c("y", "group", "index", "x") %in% names(Y))

	Y <- Y[, .(group, index, y, x)]

	y_k_times_b <- ""

	if (k > 0) {
		Y[, tmp := y]
		Y[, tmp := c(NA, tmp[-.N]), by = "group"]
		eval(parse(text = "Y[, y1 := tmp]"))

		y_k_times_b <- paste0(y_k_times_b, "y1*b[1]")

		if (k > 1) {
			for (i in 2:k) {
				Y[, tmp := c(NA, tmp[-.N]), by = "group"]
				eval(parse(text = paste0("Y[, y", i, " := tmp]")))

				y_k_times_b <- paste0(y_k_times_b, " + y", i, "*b[", i, "]")
			}
		}
		Y[, tmp := NULL]
	}

	# fit with optim(...)
	nb_dinarch_mll <- function(par) {

		# print(par)

		a <- par[1]
		b <- 1/(1 + exp(-par[1 + 1:k]))
		c <- exp(par[1 + k + 1])

		code = paste0("Y[, r := c*(exp(a) * x  + ", y_k_times_b, ")]")
		eval(parse(text = code))
		Y[, p :=  c/(1 + c)]

		r_min = min(Y[, r], na.rm = TRUE)
		p <- c/(1 + c)
		penalty_b <- -lambda * log(1 - max(b) + 1e-5)
		penalty_c <- -10 * lambda * log(min(c, 1) + 1e-5)
		n <- nrow(Y)

		if (r_min > 0 && 0 < p && p < 1 && sum(b) < 1) {
			return(-sum(Y[, loglik := dnbinom(x = y, size = r, prob = p, log = TRUE)]$loglik, na.rm = TRUE) + (penalty_b + penalty_c)/n)
		} else {
			return(1e6)
		}
	}

	if (is.null(start)) {
		start <- c(0, rep(-1, k), -1)
	}

	fit <- optim(par = start, fn = nb_dinarch_mll, method = "Nelder-Mead")
	# fit <- nlm(p = fit$par, f = nb_dinarch_mll, iterlim = 1000, gradtol = 1e-9)


	a_hat <- fit$par[1]
	b_hat <- 1/(1 + exp(-fit$par[1 + 1:k]))
	c_hat <- exp(fit$par[1 + k + 1])

	loglik_max <- -fit$value

	return(list(loglik_max = loglik_max,
							a = a_hat,
							b = b_hat,
							c = c_hat))

}

nb_dinarch_x_sim <- function(n, a, b, c, x, m = 100, start = NULL) {

	if (max(b) >= 1 | min(b) < 0) {
		stop()
	}

	# if (min(a) < 0) {
	#   stop()
	# }

	if (min(c) < 0) {
		stop()
	}

	if (length(a) == 1) {
		a <- rep(a, n)
	}

	if (length(b) == 1) {
		b <- rep(b, n)
	}

	if (length(c) == 1) {
		c <- rep(c, n)
	}

	# k <- length(b)
	N <- n + m

	p <- 1 - 1/(1 + c)
	y <- rep(0, N)

	# if (m > 0) {
	#   y[1] <- rnbinom(1, size = c[1]*a[1], prob = p[1])
	# }
	#
	# if (k > 1 & m > 0) {
	#   for (i in 2:k) {
	#     y_i <- rep(0, k)
	#     y_i[i - min(k, i - 1):1] <- y[i - 1:min(k, i - 1)]
	#
	#     y[i] <- rnbinom(1, size = c[i]*(a[i] + sum(b[i]*y_i)), prob = p[i])
	#   }
	# }

	k <- 1
	if (!is.null(start)) {
		y[1:k] <- start
	}

	for (i in (k + 1):N) {
		y[i] <- rnbinom(1, size = c[i]*(exp(a[i])*x[i] + sum(b[i]*y[i - 1:k])), prob = p[i])

		# original 0.1 over 5 years
		if (i > 5) {
			start <- i - 5
			end <- (i - 1)

			if (sum(y[start:end]) > x[i]*1e6*0.03) y[i] <- 0
		}
	}

	return(y[(m + 1):N])

}

nb_dinarch_sim <- function(n, a, b, c, m = 100, start = NULL) {

	if (sum(b) >= 1 | min(b) < 0) {
		stop()
	}

	if (min(a) < 0) {
		stop()
	}

	if (c < 0 | 1 < c) {
		stop()
	}

	if (length(a) == 1) {
		a <- rep(a, n)
	}

	k <- length(b)
	N <- n + m

	p <- 1 - 1/(1 + c)

	y <- rep(0, N)

	if (m > 0) {
		y[1] <- rnbinom(1, size = c*a[1], prob = p)
	}

	if (k > 1 & m > 0) {
		for (i in 2:k) {
			y_i <- rep(0, k)
			y_i[i - min(k, i - 1):1] <- y[i - 1:min(k, i - 1)]

			y[i] <- rnbinom(1, size = c*(a[i] + sum(b*y_i)), prob = p)
		}
	}

	if (!is.null(start)) {
		y[1:k] <- start
	}

	for (i in (k + 1):N) {
		y[i] <- rnbinom(1, size = c*(a[i] + sum(b*y[i - 1:k])), prob = p)
	}

	return(y[(m + 1):N])

}

nb_dinarch_dynamic_sim <- function(n, a, b, c, m = 100, start = NULL) {

	if (max(b) >= 1 | min(b) < 0) {
		stop()
	}

	if (min(a) < 0) {
		stop()
	}

	if (c < 0 | 1 < c) {
		stop()
	}

	if (length(a) == 1) {
		a <- rep(a, n)
	}

	if (length(b) == 1) {
		b <- rep(b, n)
	}

	if (length(c) == 1) {
		c <- rep(c, n)
	}

	k <- length(b)
	N <- n + m


	p <- 1 - 1/(1 + c)
	y <- rep(0, N)

	if (m > 0) {
		y[1] <- rnbinom(1, size = c[1]*a[1], prob = p[1])
	}

	if (k > 1 & m > 0) {
		for (i in 2:k) {
			y_i <- rep(0, k)
			y_i[i - min(k, i - 1):1] <- y[i - 1:min(k, i - 1)]

			y[i] <- rnbinom(1, size = c[i]*(a[i] + sum(b[i]*y_i)), prob = p[i])
		}
	}

	if (!is.null(start)) {
		y[1:k] <- start
	}

	for (i in (k + 1):N) {
		y[i] <- rnbinom(1, size = c[i]*(a[i] + sum(b[i]*y[i - 1:k])), prob = p[i])
	}

	return(y[(m + 1):N])

}

nb_dinarch_sim_test <- function(n, a, b, c, m = 100, start = NULL) {

	if (max(b) >= 1 | min(b) < 0) {
		stop()
	}

	if (min(a) < 0) {
		stop()
	}

	if (min(c) < 0 | 1 < max(c)) {
		stop()
	}

	if (length(a) == 1) {
		a <- rep(a, n)
	}

	if (length(b) == 1) {
		b <- rep(b, n)
	}

	if (length(c) == 1) {
		c <- rep(c, n)
	}

	# k <- length(b)
	N <- n + m

	p <- 1 - 1/(1 + c)
	y <- rep(0, N)

	# if (m > 0) {
	#   y[1] <- rnbinom(1, size = c[1]*a[1], prob = p[1])
	# }
	#
	# if (k > 1 & m > 0) {
	#   for (i in 2:k) {
	#     y_i <- rep(0, k)
	#     y_i[i - min(k, i - 1):1] <- y[i - 1:min(k, i - 1)]
	#
	#     y[i] <- rnbinom(1, size = c[i]*(a[i] + sum(b[i]*y_i)), prob = p[i])
	#   }
	# }

	k <- 1
	if (!is.null(start)) {
		y[1:k] <- start
	}

	for (i in (k + 1):N) {
		y[i] <- rnbinom(1, size = c[i]*(a[i] + sum(b[i]*y[i - 1:k])), prob = p[i])
	}

	return(y[(m + 1):N])

}



tmp_normalise <- function(x) {
	(x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

tmp_function <- function(a, b, N) {

	x <- seq(-3, 9, length.out = N)
	sigmoid <- function(x) 1 / (1 + exp(-x))
	y <- a + (b - a) * sigmoid(x)

	return(y)
}

# ------------------------------------------------------------------------------










