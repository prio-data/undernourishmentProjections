# renv::install("../heterolm")
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

source("R/simulation_SSP_dem.R") # Democracy projections
source("R/simulation_bayes_SSP_brd.R") # Conflict projections
source("R/prepare_projections.R") # Combines SSP projections with democracy and conflict projections. Results in "data/base_projections.parquet"


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


for(simulation_alternative in simulation_alternatives){
	ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
	source("R/1_load_data.R")
	source("R/2_fit_des.R")
	source("R/3_summary_tables.R")
	source("R/4_projections.R")
	rm(list = ls())
}


