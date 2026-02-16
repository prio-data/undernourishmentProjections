## Script for Buhaug et al., 2026. Projections of undernourishment. 

## This script reproduces the in-sample tests presented in the appendix, section 2.8.

rm(list = ls(all = TRUE))
gc()
# Load necessary libraries

library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)
library(tidyverse)
library(countrycode)
library(gratia)
library(statmod)
library(glmmTMB)
library(ggeffects)
library(Metrics)
library(furrr)
library(future)
library(future.apply)  
library(marginaleffects)
library(mgcv)
library(robustlmm)
library(performance)
library(ggeffects)


# Clean up temporary files
temp_files <- list.files(tempdir(), full.names = TRUE)
unlink(temp_files, recursive = TRUE)

set.seed(123)

# Set the working directory and get the list of NetCDF files
setwd("")


crop <- read.csv("Production_Crops_Livestock_E_All_Data/Production_Crops_Livestock_E_All_Data_NOFLAG.csv")

# First, attempt conversion
crop_final <- crop %>%
  filter(Element == "Production") %>% 
  rename_with(~str_remove(., "^Y"), starts_with("Y")) %>%
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "year",
               values_to = "crop") %>%
  mutate(year = as.integer(year)) %>%
  
  # Convert countries to GW code
  mutate(gwcode = countrycode(Area, "country.name", "gwn")) %>%
  # correction for unmatched cases:
  mutate(gwcode = case_when(
    Area == "Antigua and Barbuda" ~ 58,
    Area == "Dominica" ~ 54,
    Area == "Micronesia (Federated States of)" ~ 987,
    Area == "Grenada" ~ 55,
    Area == "Kiribati" ~ 970,
    Area == "Saint Kitts and Nevis" ~ 60,
    Area == "Saint Lucia" ~ 56,
    Area == "Marshall Islands" ~ 983,
    Area == "Nauru" ~ 971,
    Area == "Saint Vincent and the Grenadines" ~ 57,
    Area == "Vanuatu" ~ 935,
    Area == "Seychelles" ~ 591,
    Area == "Samoa" ~ 990,
    Area == "Tuvalu" ~ 973,
    Area == "Tonga" ~ 972,
    Area == "Sao Tome and Principe" ~ 403,
    Area == "Palestine" ~ 669,
    Area == "Yemen" ~ 678,
    Area == "Serbia and Montenegro" ~ 345,
    Area == "Yugoslav SFR" ~ 345,
    Area == "China" ~ 710,        
    TRUE ~ gwcode                 
  )) %>%
  
  # Remove rows without gwcode (optional but recommended):
  filter(!is.na(gwcode)) %>%
  
  # Summarize production per country-year
  group_by(gwcode, year) %>%
  summarise(crop = sum(crop, na.rm = TRUE), .groups = "drop")


climate <- read.csv("clim_growingseason_country_year_allssps.csv")

climate <- subset(climate, climate$year >= 1961)

climate <- climate %>% rename(gwcode = country_id)

# Exact SSP colors
plotting_colors <- c(
  "SSP1-2.6" = "#1E9620",
  "SSP2-4.5" = "#4576BF",
  "SSP3-7.0" = "#F21111",
  "SSP4-7.0" = "#E88831",
  "SSP5-8.5" = "#8036A8"
)

rx_long <- climate %>%
  dplyr::select(gwcode, year, starts_with("rx5day")) %>%
  pivot_longer(
    cols = starts_with("rx5day"),
    names_to = "scenario_raw",
    values_to = "rx5day"
  ) %>%
  mutate(
    scenario = case_when(
      grepl("126", scenario_raw) ~ "SSP1-2.6",
      grepl("245", scenario_raw) ~ "SSP2-4.5",
      grepl("370", scenario_raw) ~ "SSP3-7.0",
      grepl("470", scenario_raw) ~ "SSP4-7.0",
      grepl("585", scenario_raw) ~ "SSP5-8.5"
    )
  ) %>%
  filter(!is.na(scenario))

rx_trend <- rx_long %>%
  group_by(year, scenario) %>%
  summarise(rx5day = mean(rx5day, na.rm = TRUE), .groups = "drop")

#Plot historical series before 2014 where simulations start (data are same across all scenarios before 2015)
hist_series <- rx_trend %>%
  filter(year <= 2014, scenario == "SSP1-2.6")

ssp_series <- rx_trend %>%
  filter(year >= 2014)

rx_plot <- ggplot() +
  
  geom_line(
    data = hist_series,
    aes(x = year, y = rx5day),
    color = "black",
    linewidth = 1
  ) +
  
  geom_line(
    data = ssp_series,
    aes(x = year, y = rx5day, color = scenario),
    linewidth = 1
  ) +
  
  scale_color_manual(values = plotting_colors) +
  
  theme_bw(base_size = 24) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 18)
  ) +
  labs(
    subtitle = "Global mean Rx5day (growing season)",
    x = "Year",
    y = "Rx5day"
  )

rx_plot

ggsave(rx_plot, filename = "rx5day_trendlines.png", dpi = 350, width = 9,
       height = 5.5,
       units = "in")



#correlation plot crop and rx5day

climate <- subset(climate, climate$year <= 2023)
data <- left_join(crop_final, climate, by = c("gwcode", "year"))
data <- data %>%
  mutate(crop = round(crop / 1e6))  #  1M tonnes 

data <- data %>%
  mutate(crop = round(crop)) 


data$gwcode <- factor(data$gwcode)


#correlation plot crop and rx5day

#Test different models###
##Aggregate to 3 year 

library(zoo)

# Compute 3-year moving average of spei6
data_3y <- data %>%
  arrange(gwcode, year) %>%
  group_by(gwcode) %>%
  mutate(tx90_ma3 = rollmean(tx90p_245_gs, k = 3, fill = NA, align = "right"), 
         rx5day_ma3 = rollmean(rx5day_245_gs, k = 3, fill = NA, align = "right"))

# Compute 3-year difference in crop production
crop_final <- crop_final %>%
  arrange(gwcode, year) %>%
  group_by(gwcode) %>%
  mutate(crop_diff3 = crop - lag(crop, 3), 
         crop_tot = rollsum(crop, k = 3, fill = NA, align = "right"), 
         crop_diff_log = log1p(crop) - lag(log1p(crop), 3)
  )

crop_final$gwcode <- as.factor(as.character(crop_final$gwcode))

data_3y <- left_join(crop_final, data_3y, by = c("gwcode", "year")) %>%
  filter(year >= 1965 & year <= 2023) %>%  # to avoid edge effects from moving average and lag
  mutate(
    crop_diff3 = round(crop_diff3 / 1e6),  # scale to millions if needed
    crop_tot = round(crop_tot/1e6),
    gwcode = factor(gwcode)
  ) %>%
  filter(!is.na(rx5day_ma3), !is.na(crop_diff3))  # drop rows with missing due to lag/MA




##############In-sample models for tx90p and rx5day##############


plot_data <- data_3y

cor_test <- cor.test(plot_data$tx90_ma3, plot_data$crop_diff_log)

corr <- ggplot(plot_data, aes(x = tx90_ma3, y = crop_diff_log)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred", fill = "lightpink") +
  labs(title = paste0("Correlation between tx90_ma3 and crop_diff3: r = ",
                      round(cor_test$estimate, 3),
                      ", p-value = ",
                      signif(cor_test$p.value, 2)),
       x = "3-year MA of extreme heat",
       y = "3-year crop production change (million tonnes)") +
  theme_minimal()

corr
ggsave(corr, filename = "corr_tx90ma3_cropdiff_years.png", dpi = 300)



########RX5day#############

plot_data <- data_3y
cor_test <- cor.test(plot_data$rx5day_ma3, plot_data$crop_diff_log)

corr <- ggplot(plot_data, aes(x = rx5day_ma3, y = crop_diff_log)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred", fill = "lightpink") +
  labs(title = paste0("Correlation between rx5day_ma3 and crop_diff3: r = ",
                      round(cor_test$estimate, 3),
                      ", p-value = ",
                      signif(cor_test$p.value, 2)),
       x = "3-year Mov. Avg. Prec. Extreme (rx5day)",
       y = "3-year crop production change (million tonnes)") +
  theme_minimal()


corr
ggsave(corr, filename = "corr_rx5dayma3_cropdiff_years.png", dpi = 300)


###In-sample models tests


gam_lmm_clim <- gam(crop_diff_log ~ s(rx5day_ma3) + s(tx90_ma3) + s(gwcode, bs = "re") + year, data = data_3y)

#Reg tables
summary(gam_lmm_clim) 

draw(gam_lmm_clim)


#Plot marginal effects
plot_marginal_with_hist <- function(
    model,
    data,
    var,
    xlab,
    filename
) {
  
  # Marginal effects
  marg <- ggpredict(model, terms = paste0(var, " [all]"))
  
  # Histogram data
  plot_data <- na.omit(data[, c(var, "crop_diff3")])
  names(plot_data)[1] <- "x"
  
  hist_plot <- ggplot(plot_data, aes(x = x)) +
    geom_histogram(bins = 100)
  
  hist_data <- ggplot_build(hist_plot)$data[[1]]
  
  # Rescale histogram to response axis
  hist_data$y_scaled <-
    hist_data$count / max(hist_data$count) *
    abs(min(marg$predicted) * 15)
  
  # Plot
  p <- ggplot() +
    geom_col(
      data = hist_data,
      aes(
        x = xmin + (xmax - xmin) / 2,
        y = y_scaled
      ),
      fill = "grey80",
      color = "grey50",
      width = hist_data$xmax - hist_data$xmin
    ) +
    geom_line(
      data = marg,
      aes(x = x, y = predicted),
      color = "steelblue",
      size = 0.9
    ) +
    geom_ribbon(
      data = marg,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "steelblue",
      alpha = 0.4
    ) +
    labs(
      x = xlab,
      y = "Logged 3-year crop change"
    ) +
    scale_x_continuous(limits = range(marg$x)) +
    theme_minimal() +
    theme(
      axis.line = element_line(),
      axis.ticks = element_line(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      text = element_text(family = "Arial", size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    )
  
  ggsave(p, filename = filename, dpi = 300)
  return(p)
}


ins_rx5day <- plot_marginal_with_hist(
  model    = gam_lmm_clim,
  data     = data_3y,
  var      = "rx5day_ma3",
  xlab     = "3-year moving average of precipitation extreme (rx5day)", 
  filename = "marginal_effect_rx5day_prectemp_ma3_cropdiff3_gam.png"
)

ins_rx5day

ggsave(ins_rx5day, filename = "marginal_effect_rx5day_prectemp_ma3_cropdiff3_gam.png", dpi = 300)

plot_marginal_with_hist <- function(
    model,
    data,
    var,
    xlab,
    filename
) {
  
  # Marginal effects
  marg <- ggpredict(model, terms = paste0(var, " [all]"))
  
  # Histogram data
  plot_data <- na.omit(data[, c(var, "crop_diff3")])
  names(plot_data)[1] <- "x"
  
  hist_plot <- ggplot(plot_data, aes(x = x)) +
    geom_histogram(bins = 100)
  
  hist_data <- ggplot_build(hist_plot)$data[[1]]
  
  # Rescale histogram to response axis
  hist_data$y_scaled <-
    hist_data$count / max(hist_data$count) *
    abs(min(marg$predicted) * 4)
  
  # Plot
  p <- ggplot() +
    geom_col(
      data = hist_data,
      aes(
        x = xmin + (xmax - xmin) / 2,
        y = y_scaled
      ),
      fill = "grey80",
      color = "grey50",
      width = hist_data$xmax - hist_data$xmin
    ) +
    geom_line(
      data = marg,
      aes(x = x, y = predicted),
      color = "steelblue",
      size = 0.9
    ) +
    geom_ribbon(
      data = marg,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      fill = "steelblue",
      alpha = 0.4
    ) +
    labs(
      x = xlab,
      y = "Logged 3-year crop change"
    ) +
    scale_x_continuous(limits = range(marg$x)) +
    theme_minimal() +
    theme(
      axis.line = element_line(),
      axis.ticks = element_line(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      text = element_text(family = "Arial", size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    )
  
  ggsave(p, filename = filename, dpi = 300)
  return(p)
}
ins_tx90 <- plot_marginal_with_hist(
  model    = gam_lmm_clim,
  data     = data_3y,
  var      = "tx90_ma3",
  xlab     = "3-year moving average of extreme temperature (TX90)", 
  filename = "marginal_effect_tx90_prectemp_ma3_cropdiff3_gam.png"
)

ins_tx90
ggsave(ins_tx90, filename = "marginal_effect_tx90_prectemp_ma3_cropdiff3_gam.png", dpi = 300)

