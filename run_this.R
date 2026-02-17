# renv::install("prio-data/heterolm", rebuild = T)
library(heterolm)
library(data.table)
library(gtsummary)
library(patchwork)
library(ggplot2)
library(ggdist)
library(dplyr)
library(lubridate)
library(tidyr)
library(arrow)
library(modelsummary)
library(xtable)

plotting_colors <- c("SSP1-2.6" = "#1E9620",
										 "SSP2-4.5" = "#4576BF",
										 "SSP3-7.0" = "#F21111",
										 "SSP4-7.0" = "#E88831",
										 "SSP5-8.5" = "#8036A8")

ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")

# Setting up the data
# - This do require installation of several packages and downloading data from various sources, and these might not run out of the box.
# source("R/foodbalancesheets.R") # Country-level covariates on food imports and exports, etc.
# source("R/cropland_conflict.R") # Cropland land area and its spatial interaction with conflict
# source("R/mder_estimates.R") # Parameters for MDER projections
# source("R/historical_mder.R") # Calculates historical MDER
# source("R/merge_data.R") # Puts together the main dataset from our different sources. Also see poldat::static_world.
# source("R/impute_data.R") # Imputes a few DES observations using a set of co-variates. Results in "data/main.csv"
# source("R/ssp_prepare.R") # Prepares the main SSP projection data and calculates MDER
# source("R/harmonize_ssp.R") # Harmonizes the SSP with the historical data

# source("R/simulation_SSP_dem.R") # Democracy projections
# source("R/simulation_bayes_SSP_brd.R") # Conflict projections
# source("R/prepare_projections.R") # Combines SSP projections with democracy and conflict projections. Results in "data/base_projections.parquet"


# DES projections
simulation_alternatives <- c("base", "constant_democracy", "constant_climate", "no_conflict_effect")

# no_conflict_effect is a counter-factual scenario that estimates what food security projections would look like (compared to base) if:
# 1. Conflict had no direct effect on dietary energy supply changes
# 2. Conflict had no effect on economic growth (GDP penalty removed)
# 3. Conflict had no effect on food supply variability (CV)

# constant_democracy is a counter-factual scenario that estimates what food security projections would look like (compared to base) if:
# 1. democracy stays constant at 2023-levels

# constant_climate is a counter-factual scenario that estimates what food security projections would look like (compared to base) if:
# 1. climate stays constant at 2023-levels

# CV projections
cv_approaches <- c("regression", "manual")

TIME_INTERVAL <- 1

# cv_approach <- "regression"
# simulation_alternative <- "base"

for(cv_approach in cv_approaches){
	for(simulation_alternative in simulation_alternatives){
		rm(list = setdiff(ls(), c("plotting_colors", "ragg_png", "TIME_INTERVAL", "simulation_alternatives", "simulation_alternative", "cv_approaches", "cv_approach")))
		dir.create(file.path("results", paste0("timeint", TIME_INTERVAL), simulation_alternative, cv_approach), recursive = T)
		dir.create(file.path("tables", paste0("timeint", TIME_INTERVAL), simulation_alternative, cv_approach), recursive = T)
		dir.create(file.path("figures", paste0("timeint", TIME_INTERVAL), simulation_alternative, cv_approach), recursive = T)

		dir.create(file.path("results", simulation_alternative, cv_approach), recursive = T)
		dir.create(file.path("tables", simulation_alternative, cv_approach), recursive = T)
		dir.create(file.path("figures", simulation_alternative, cv_approach), recursive = T)

		source("R/1_load_data.R")
		source("R/2_fit_des.R")
		source("R/3_fit_cv.R")

		if(simulation_alternative == "base" & cv_approach == "regression" & TIME_INTERVAL == 1){
			source("R/2_1_des_model_comparison.R")
			source("R/4_summary_tables.R")
		}

		source("R/5_projections.R")
		source("R/6_plot_projections.R")
		rm(list = setdiff(ls(), c("plotting_colors", "ragg_png", "TIME_INTERVAL", "simulation_alternatives", "simulation_alternative", "cv_approaches", "cv_approach")))
	}
}

source("R/7_comparison.R")
source("R/8_time_interval_comparison.R")


