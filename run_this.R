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

ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")

source("R/1_load_data.R")


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
	source("R/2_fit_des.R")
	source("R/3_summary_tables.R")
	source("R/4_projections.R")
}


