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


#spei6 is the percentage of days when the daily maximum temperature exceeded the 90th percentile of maximum temperature from a baseline period (e.g., (baseline 1960-1990).

# Clean up temporary files
temp_files <- list.files(tempdir(), full.names = TRUE)
unlink(temp_files, recursive = TRUE)

set.seed(123)

# Set the working directory and get the list of NetCDF files
setwd("/Users/paola.vesco/ViEWS Dropbox/Paola Vesco/paola/projects/polimpact/undernourishment_paper/")

crop <- read.csv("data/crop/fao/Production_Crops_Livestock_E_All_Data/Production_Crops_Livestock_E_All_Data_NOFLAG.csv")

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
    Area == "Yugoslav SFR" ~ 345, # Serbia as successor
    Area == "China" ~ 710,        # Mainland China
    TRUE ~ gwcode                 # Retain successfully matched codes
  )) %>%
  
  # Remove rows without gwcode (optional but recommended):
  filter(!is.na(gwcode)) %>%
  
  # Summarize production per country-year
  group_by(gwcode, year) %>%
  summarise(crop = sum(crop, na.rm = TRUE), .groups = "drop")


#pou <- read.csv("data/imputed_static_world_0.4+climate+food_balance+mder+cereal_production.csv")

spei6 <- read.csv("data/final/clim_growingseason_country_year_allssps.csv")

spei6 <- subset(spei6, spei6$year <= 2023)
spei6 <- subset(spei6, spei6$year >= 1961)

spei6 <- spei6 %>% rename(gwcode = country_id)

names(spei6)

data <- left_join(crop_final, spei6, by = c("gwcode", "year"))
data <- data %>%
  mutate(crop = round(crop / 1e6))  #  1M tonnes 

data <- data %>%
  mutate(crop = round(crop))  # ensure it is integer


data$gwcode <- factor(data$gwcode)
#data$year <- factor(data$year)

data <- subset(data, !is.na(data$spei6_245_gs)) #remove NA, they are gwcode with no matches

#correlation plot crop and spei6

# Subset data removing NAs from relevant columns
plot_data <- data[!is.na(data$spei6_245_gs) & !is.na(data$crop), ]


# Correlation test
cor_test <- cor.test(plot_data$spei6_245_gs, plot_data$crop)

# Correlation test
#cor_test <- cor.test(plot_data$gwcode, plot_data$crop)

# Scatter plot with correlation coefficient and regression line
corr <- ggplot(plot_data, aes(x = spei6_245_gs, y = crop)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", fill = "lightpink") +
  labs(title = paste0("Correlation between spei6 and crop: r = ",
                      round(cor_test$estimate, 3),
                      ", p-value = ",
                      signif(cor_test$p.value, 2)),
       x = "Agricultural drought (SPEI6)",
       y = "Crop production (tonnes)") +
  theme_minimal()

corr

ggsave(corr, filename = "corr_spei6_crop.png",  dpi = 300)

corr <- ggplot(plot_data, aes(x = spi6_245_gs, y = crop)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", fill = "lightpink") +
  labs(title = paste0("Correlation between spi6 and crop: r = ",
                      round(cor_test$estimate, 3),
                      ", p-value = ",
                      signif(cor_test$p.value, 2)),
       x = "Agricultural drought (SPI6)",
       y = "Crop production (tonnes)") +
  theme_minimal()

corr

ggsave(corr, filename = "corr_spi_crop.png",  dpi = 300)


# Scatter plot with correlation coefficient and regression line
corr <- ggplot(plot_data, aes(x = rx5day_245_gs, y = crop)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", fill = "lightpink") +
  labs(title = paste0("Correlation between spei6 and crop: r = ",
                      round(cor_test$estimate, 3),
                      ", p-value = ",
                      signif(cor_test$p.value, 2)),
       x = "Precipitation extreme (rx5day)",
       y = "Crop production (tonnes)") +
  theme_minimal()

corr

ggsave(corr, filename = "corr_rx5day_crop.png",  dpi = 300)

#Test different models###


##Try 5 year difference models

library(zoo)

# Compute 5-year moving average of spei6
spei6 <- data %>%
  arrange(gwcode, year) %>%
  group_by(gwcode) %>%
  mutate(spei6_ma5 = rollmean(spei6_245_gs, k = 5, fill = NA, align = "right"), 
         spi6_ma5 = rollmean(spi6_245_gs, k = 5, fill = NA, align = "right"),
         tx90_ma5 = rollmean(tx90p_245_gs, k = 5, fill = NA, align = "right"), 
         rx5day_ma5 = rollmean(rx5day_245_gs, k = 5, fill = NA, align = "right"))

# Compute 5-year difference in crop production
crop_final <- crop_final %>%
  arrange(gwcode, year) %>%
  group_by(gwcode) %>%
  mutate(crop_diff5 = crop - lag(crop, 5), 
         crop_tot = rollsum(crop, k = 5, fill = NA, align = "right"), 
         crop_diff_log = log1p(crop) - lag(log1p(crop), 5)
  )

crop_final$gwcode <- as.factor(as.character(crop_final$gwcode))

data_5y <- left_join(crop_final, spei6, by = c("gwcode", "year")) %>%
  filter(year >= 1965 & year <= 2023) %>%  # to avoid edge effects from moving average and lag
  mutate(
    crop_diff5 = round(crop_diff5 / 1e6),  # scale to millions if needed
    crop_tot = round(crop_tot/1e6),
    gwcode = factor(gwcode)
  ) %>%
  filter(!is.na(spei6_ma5), !is.na(crop_diff5))  # drop rows with missing due to lag/MA

plot_data <- data_5y

cor_test <- cor.test(plot_data$spei6_ma5, plot_data$crop_diff_log)

corr <- ggplot(plot_data, aes(x = spei6_ma5, y = crop_diff_log)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred", fill = "lightpink") +
  labs(title = paste0("Correlation between spei6_ma5 and crop_diff5: r = ",
                      round(cor_test$estimate, 3),
                      ", p-value = ",
                      signif(cor_test$p.value, 2)),
       x = "5-year Mov. Avg. Drought Index (SPEI6)",
       y = "5-year crop production change (million tonnes)") +
  theme_minimal()


corr
ggsave(corr, filename = "corr_spei6ma5_cropdiff_years.png", dpi = 300)


cor_test <- cor.test(plot_data$spi6_ma5, plot_data$crop_diff_log)

corr <- ggplot(plot_data, aes(x = spi6_ma5, y = crop_diff_log)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred", fill = "lightpink") +
  labs(title = paste0("Correlation between spi6_ma5 and crop_diff5: r = ",
                      round(cor_test$estimate, 3),
                      ", p-value = ",
                      signif(cor_test$p.value, 2)),
       x = "5-year Mov. Avg. Drought Index (SPI6)",
       y = "5-year crop production change (million tonnes)") +
  theme_minimal()

corr
ggsave(corr, filename = "corr_spi6ma5_cropdiff_years.png", dpi = 300)


gam_model_spei <- gam(crop_diff_log ~ spei6_ma5 + gwcode + year, data = data_5y)

gam.check(gam_model_spei)
gam_model_noclim <- gam(crop_diff_log ~ gwcode + year, data = data_5y)
summary(gam_model_noclim)


gam_model_spi <- gam(crop_diff_log ~ spi6_ma5 + gwcode + year, data = data_5y)
summary(gam_model_spi)

gam.check(gam_model_spi)
summary(gam_model_noclim)


library(mgcv)

gam_lmm_spei <- gam(crop_diff_log ~ s(spei6_ma5) +  s(gwcode, bs = "re") +  year, data = data_5y)

gam_lmm_noclim <- gam(crop_diff_log ~   s(gwcode, bs = "re") +  year, data = data_5y)

summary(gam_lmm_noclim)
draw(gam_lmm_noclim)

summary(gam_lmm_spei)
draw(gam_lmm_spei)

library(robustlmm)

robust_model_spei <- rlmer(crop_diff_log ~ spei6_ma5 + (1 | gwcode) + (1 | year), data = data_5y)
summary(robust_model_spei)

library(performance)

compare_performance(gam_lmm_spei, gam_model_spei, robust_model_spei, rank = TRUE)  # AIC, R², RMSE, etc.

library(ggeffects)


#Chosen model:  GAM LLM SPEI We estimate a generalized additive mixed model, with country level random intercepts and a linear year effect to control for unobserved cross country heterogeneity and common global shocks, while allowing the effect of the five year SPEI moving average on log crop change to be nonlinear. 

marg_lmm_spei <- ggpredict(gam_lmm_spei, terms = "spei6_ma5 [all]")

plot_data_spei <- na.omit(data_5y[, c("spei6_ma5", "crop_diff5")])

# Histogram scaling
hist_plot_spei <- ggplot(plot_data_spei, aes(x = spei6_ma5)) + geom_histogram(bins = 100)
hist_data_spei <- ggplot_build(hist_plot_spei)$data[[1]]

# Check content
head(hist_data_spei)

hist_data_spei$y_scaled <- hist_data_spei$count / max(hist_data_spei$count) * abs(min(marg_lmm_spei$predicted)*5)

# Plot: Chosen model gam_lmm_spei
ins_spei <- ggplot() +
  geom_col(data = hist_data_spei, 
           aes(x = xmin + (xmax - xmin)/2, y = y_scaled),
           fill = "grey80", color = "grey50", width = hist_data_spei$xmax - hist_data_spei$xmin) +
  geom_line(data = marg_lmm_spei, aes(x = x, y = predicted), color = "steelblue", size = 0.9) +
  geom_ribbon(data = marg_lmm_spei, aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "steelblue", alpha = 0.4) +
  labs(x = "5-year moving average of agr. drought (SPEI6)", y = "5-year crop change (logged)") +
  scale_x_continuous(limits = range(marg_lmm_spei$x)) +
  theme_minimal() +
  theme(axis.line = element_line(), 
        axis.ticks = element_line(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        text = element_text(family = "Arial", size = 14), 
        #plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)
  ) 


ins_spei
ggsave(ins_spei, filename = "marginal_effect_spei6ma5_cropdiff5_gam.png", dpi = 300)

#Marginal effect of the five-year moving average of agricultural drought (SPEI6) on the logged five-year change in crop production. Estimates are obtained from a generalized additive mixed model with a smooth nonlinear effect of SPEI6, country-specific random intercepts implemented via a random-effect smooth, and a linear calendar year term capturing common temporal trends. The blue line shows the predicted average marginal effect across the observed range of SPEI6, and the shaded area indicates the 95 percent confidence interval. The histogram represents the distribution of the SPEI6 drought indicator in 1960-2023, scaled for visual reference only.

###SPI 


gam_lmm_spi <- gam(crop_diff_log ~ s(spi6_ma5) +  s(gwcode, bs = "re") +  year, data = data_5y)

summary(gam_lmm_spi)
draw(gam_lmm_spi)

library(robustlmm)

robust_model_spi <- rlmer(crop_diff_log ~ spi6_ma5 + (1 | gwcode) + (1 | year), data = data_5y)
summary(robust_model_spi)

library(performance)

compare_performance(gam_lmm_spi, gam_model_spi, robust_model_spi, rank = TRUE)  # AIC, R², RMSE, etc.

library(ggeffects)

marg_lmm_spi <- ggpredict(gam_lmm_spi, terms = "spi6_ma5 [all]")

plot_data_spi <- na.omit(data_5y[, c("spi6_ma5", "crop_diff5")])

# Histogram scaling
hist_plot_spi <- ggplot(plot_data_spi, aes(x = spi6_ma5)) + geom_histogram(bins = 100)
hist_data_spi <- ggplot_build(hist_plot_spi)$data[[1]]

# Check content
head(hist_data_spi)

hist_data_spi$y_scaled <- hist_data_spi$count / max(hist_data_spi$count) * abs(min(marg_lmm_spi$predicted)*5)

# Plot GAM LLM 
ins_spi <- ggplot() +
  geom_col(data = hist_data_spi, 
           aes(x = xmin + (xmax - xmin)/2, y = y_scaled),
           fill = "grey80", color = "grey50", width = hist_data_spi$xmax - hist_data_spi$xmin) +
  geom_line(data = marg_lmm_spi, aes(x = x, y = predicted), color = "steelblue", size = 0.9) +
  geom_ribbon(data = marg_lmm_spi, aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "steelblue", alpha = 0.4) +
  labs(x = "5-year moving average of agr. drought (SPI6)", y = "5-year crop change (logged)") +
  scale_x_continuous(limits = range(marg_lmm_spi$x)) +
  theme_minimal() +
  theme(axis.line = element_line(), 
        axis.ticks = element_line(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        text = element_text(family = "Arial", size = 14), 
        #plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)
  ) 

ins_spi
#Marginal effect of the five-year moving average of agricultural drought (SPI6) on the logged five-year change in crop production. Estimates are obtained from a generalized additive mixed model with a smooth nonlinear effect of SPI6, country-specific random intercepts implemented via a random-effect smooth, and a linear calendar year term capturing common temporal trends. The blue line shows the predicted average marginal effect across the observed range of SPI6, and the shaded area indicates the 95 percent confidence interval.  The histogram represents the distribution of the SPI6 drought indicator in 1960-2023, scaled for visual reference only.

ggsave(ins_spi, filename = "marginal_effect_spi6ma5_cropdiff5_gam.png", dpi = 300)



#TX90####
plot_data <- data_5y

cor_test <- cor.test(plot_data$tx90_ma5, plot_data$crop_diff_log)

corr <- ggplot(plot_data, aes(x = tx90_ma5, y = crop_diff_log)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred", fill = "lightpink") +
  labs(title = paste0("Correlation between tx90_ma5 and crop_diff5: r = ",
                      round(cor_test$estimate, 3),
                      ", p-value = ",
                      signif(cor_test$p.value, 2)),
       x = "5-year MA of extreme heat",
       y = "5-year crop production change (million tonnes)") +
  theme_minimal()

corr
ggsave(corr, filename = "corr_tx90ma5_cropdiff_years.png", dpi = 300)


gam_model <- gam(crop_diff_log ~ tx90_ma5 + gwcode + year, data = data_5y)
summary(gam_model)

gam.check(gam_model)
gam_model_noclim <- gam(crop_diff_log ~ gwcode + year, data = data_5y)
summary(gam_model)


library(mgcv)

gam_lmm <- gam(crop_diff_log ~ s(tx90_ma5) +  s(gwcode, bs = "re") +  year, data = data_5y)

summary(gam_lmm)
draw(gam_lmm)

library(robustlmm)

robust_model <- rlmer(crop_diff_log ~ tx90_ma5 + (1 | gwcode) + (1 | year), data = data_5y)
summary(robust_model)

library(performance)

compare_performance(gam_lmm, gam_model, robust_model, rank = TRUE)  # AIC, R², RMSE, etc.

library(ggeffects)

marg_lmm <- ggpredict(robust_model, terms = "tx90_ma5 [all]")

plot_data <- na.omit(data_5y[, c("tx90_ma5", "crop_diff5")])
# Histogram scaling
hist_plot <- ggplot(plot_data, aes(x = tx90_ma5)) + geom_histogram(bins = 100)
hist_data <- ggplot_build(hist_plot)$data[[1]]

# Check content
head(hist_data)

hist_data$y_scaled <- hist_data$count / max(hist_data$count) * abs(min(marg_lmm$predicted) * 4)

# Plot
ins <- ggplot() +
  geom_col(data = hist_data, 
           aes(x = xmin + (xmax - xmin)/2, y = y_scaled),
           fill = "grey80", color = "grey50", width = hist_data$xmax - hist_data$xmin) +
  geom_line(data = marg_lmm, aes(x = x, y = predicted), color = "steelblue", size = 0.9) +
  geom_ribbon(data = marg_lmm, aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "steelblue", alpha = 0.4) +
  labs(x = "5-year moving average of extreme temperature", y = "5-year crop change (logged)") +
  scale_x_continuous(limits = range(marg_lmm$x)) +
  theme_minimal() +
  theme(axis.line = element_line(), 
        axis.ticks = element_line(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        text = element_text(family = "Arial", size = 14), 
        #plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)
  ) 

ins
ggsave(ins, filename = "marginal_effect_tx90ma5_cropdiff5_robust.png", dpi = 300)



marg_lmm <- ggpredict(gam_lmm, terms = "tx90_ma5 [all]")

plot_data <- na.omit(data_5y[, c("tx90_ma5", "crop_diff5")])
# Histogram scaling
hist_plot <- ggplot(plot_data, aes(x = tx90_ma5)) + geom_histogram(bins = 100)
hist_data <- ggplot_build(hist_plot)$data[[1]]

# Check content
head(hist_data)

hist_data$y_scaled <- hist_data$count / max(hist_data$count) * abs(min(marg_lmm$predicted) * 4)

# Plot
ins <- ggplot() +
  geom_col(data = hist_data, 
           aes(x = xmin + (xmax - xmin)/2, y = y_scaled),
           fill = "grey80", color = "grey50", width = hist_data$xmax - hist_data$xmin) +
  geom_line(data = marg_lmm, aes(x = x, y = predicted), color = "steelblue", size = 0.9) +
  geom_ribbon(data = marg_lmm, aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "steelblue", alpha = 0.4) +
  labs(x = "5-year moving average of extreme temperature", y = "5-year crop change (logged)") +
  scale_x_continuous(limits = range(marg_lmm$x)) +
  theme_minimal() +
  theme(axis.line = element_line(), 
        axis.ticks = element_line(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        text = element_text(family = "Arial", size = 14), 
        #plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)
  ) 

ins
#Marginal effect of the five-year moving average of extreme heat (tx90p) on the logged five-year change in crop production. Estimates are obtained from a generalized additive mixed model with a smooth nonlinear effect of the extreme heat indicator, country-specific random intercepts implemented via a random-effect smooth, and a linear calendar year term capturing common temporal trends. The blue line shows the predicted average marginal effect across the observed range of extreme heat indicator, and the shaded area indicates the 95 percent confidence interval. The histogram represents the distribution of the extreme heat indicator in 1960-2023, scaled for visual reference only.

ggsave(ins, filename = "marginal_effect_tx90ma5_cropdiff5_gam.png", dpi = 300)



########RX5day#############

plot_data <- data_5y
cor_test <- cor.test(plot_data, plot_data$crop_diff_log)

corr <- ggplot(plot_data, aes(x = rx5day_ma5, y = crop_diff_log)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred", fill = "lightpink") +
  labs(title = paste0("Correlation between rx5day_ma5 and crop_diff5: r = ",
                      round(cor_test$estimate, 3),
                      ", p-value = ",
                      signif(cor_test$p.value, 2)),
       x = "5-year Mov. Avg. Prec. Extreme (rx5day)",
       y = "5-year crop production change (million tonnes)") +
  theme_minimal()


corr
ggsave(corr, filename = "corr_rx5dayma5_cropdiff_years.png", dpi = 300)



gam_model <- gam(crop_diff_log ~ rx5day_ma5 + gwcode + year, data = data_5y)
summary(gam_model)

gam.check(gam_model)
gam_model_noclim <- gam(crop_diff_log ~ gwcode + year, data = data_5y)
summary(gam_model)


library(mgcv)

gam_lmm <- gam(crop_diff_log ~ s(rx5day_ma5) +  s(gwcode, bs = "re") +  year, data = data_5y)

summary(gam_lmm)
draw(gam_lmm)

library(robustlmm)

robust_model <- rlmer(crop_diff_log ~ rx5day_ma5 + (1 | gwcode) + (1 | year), data = data_5y)
summary(robust_model)

library(performance)

compare_performance(gam_lmm, gam_model, robust_model, rank = TRUE)  # AIC, R², RMSE, etc.

library(ggeffects)

marg_lmm <- ggpredict(gam_lmm, terms = "rx5day_ma5 [all]")

plot_data <- na.omit(data_5y[, c("rx5day_ma5", "crop_diff5")])
# Histogram scaling
hist_plot <- ggplot(plot_data, aes(x = rx5day_ma5)) + geom_histogram(bins = 100)
hist_data <- ggplot_build(hist_plot)$data[[1]]

# Check content
head(hist_data)

hist_data$y_scaled <- hist_data$count / max(hist_data$count) * abs(min(marg_lmm$predicted) * 10)

# Plot
ins <- ggplot() +
  geom_col(data = hist_data, 
           aes(x = xmin + (xmax - xmin)/2, y = y_scaled),
           fill = "grey80", color = "grey50", width = hist_data$xmax - hist_data$xmin) +
  geom_line(data = marg_lmm, aes(x = x, y = predicted), color = "steelblue", size = 0.9) +
  geom_ribbon(data = marg_lmm, aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "steelblue", alpha = 0.4) +
  labs(x = "5-year moving average of extreme precipitation", y = "5-year crop change (logged)") +
  scale_x_continuous(limits = range(marg_lmm$x)) +
  theme_minimal() +
  theme(axis.line = element_line(), 
        axis.ticks = element_line(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        text = element_text(family = "Arial", size = 14), 
        #plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)
  ) 

ins
#Marginal effect of the five-year moving average of extreme precipitation (rx5day) on the logged five-year change in crop production. Estimates are obtained from a generalized additive mixed model with a smooth nonlinear effect of the extreme precipitation indicator, country-specific random intercepts implemented via a random-effect smooth, and a linear calendar year term capturing common temporal trends. The blue line shows the predicted average marginal effect across the observed range of extreme precipitation indicator, and the shaded area indicates the 95 percent confidence interval. The histogram represents the distribution of the extreme precipitation indicator in 1960-2023, scaled for visual reference only.
ggsave(ins, filename = "marginal_effect_rx5dayma5_cropdiff5_gam.png", dpi = 300)



marg_lmm <- ggpredict(gam_model, terms = "rx5day_ma5 [all]")

plot_data <- na.omit(data_5y[, c("rx5day_ma5", "crop_diff5")])
# Histogram scaling
hist_plot <- ggplot(plot_data, aes(x = rx5day_ma5)) + geom_histogram(bins = 100)
hist_data <- ggplot_build(hist_plot)$data[[1]]

# Check content
head(hist_data)

hist_data$y_scaled <- hist_data$count / max(hist_data$count) * abs(min(marg_lmm$predicted) * 10)

# Plot
ins <- ggplot() +
  geom_col(data = hist_data, 
           aes(x = xmin + (xmax - xmin)/2, y = y_scaled),
           fill = "grey80", color = "grey50", width = hist_data$xmax - hist_data$xmin) +
  geom_line(data = marg_lmm, aes(x = x, y = predicted), color = "steelblue", size = 0.9) +
  geom_ribbon(data = marg_lmm, aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "steelblue", alpha = 0.4) +
  labs(x = "5-year moving average of extreme precipitation", y = "5-year crop change (logged)") +
  scale_x_continuous(limits = range(marg_lmm$x)) +
  theme_minimal() +
  theme(axis.line = element_line(), 
        axis.ticks = element_line(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        text = element_text(family = "Arial", size = 14), 
        #plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)
  ) 

ins
ggsave(ins, filename = "marginal_effect_rx5dayma5_cropdiff5_robust.png", dpi = 300)


########ALL climate indicator

gam_lmm_clim <- gam(crop_diff_log ~ s(spi6_ma5) +  s(spei6_ma5) +s(tx90_ma5) + s(gwcode, bs = "re") +  year, data = data_5y)

summary(gam_lmm_clim)
draw(gam_lmm_clim)


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
  plot_data <- na.omit(data[, c(var, "crop_diff5")])
  names(plot_data)[1] <- "x"
  
  hist_plot <- ggplot(plot_data, aes(x = x)) +
    geom_histogram(bins = 100)
  
  hist_data <- ggplot_build(hist_plot)$data[[1]]
  
  # Rescale histogram to response axis
  hist_data$y_scaled <-
    hist_data$count / max(hist_data$count) *
    abs(min(marg$predicted) * 5)
  
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
      y = "Logged 5-year crop change"
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

ins_spi <- plot_marginal_with_hist(
  model    = gam_lmm_clim,
  data     = data_5y,
  var      = "spi6_ma5",
  xlab     = "5-year moving average of agricultural drought (SPI6)",
  filename = "marginal_effect_spi6ma5_cropdiff5_gam.png"
)

ins_spi

ggsave(ins_spi, filename = "marginal_effect_spi6_climall_ma5_cropdiff5_gam.png", dpi = 300)


ins_spei <- plot_marginal_with_hist(
  model    = gam_lmm_clim,
  data     = data_5y,
  var      = "spei6_ma5",
  xlab     = "5-year moving average of agricultural drought (SPEI6)",
  filename = "marginal_effect_spei6ma5_cropdiff5_gam.png"
)

ins_spei

ggsave(ins_spei, filename = "marginal_effect_spei6_climall_ma5_cropdiff5_gam.png", dpi = 300)


ins_tx90 <- plot_marginal_with_hist(
  model    = gam_lmm_clim,
  data     = data_5y,
  var      = "tx90_ma5",
  xlab     = "5-year moving average of extreme temperature (TX90)",
  filename = "marginal_effect_tx90ma5_cropdiff5_gam.png"
)

ins_tx90
ggsave(ins_tx90, filename = "marginal_effect_tx90_climall_ma5_cropdiff5_gam.png", dpi = 300)





###########TEMP EXT AND PREC EXT ################


########ALL climate indicator

gam_clim <- gam(crop_diff_log ~ rx5day_ma5 +tx90_ma5 + gwcode +  year, data = data_5y)

gam_lmm_clim <- gam(crop_diff_log ~ s(rx5day_ma5) + s(tx90_ma5) + s(gwcode, bs = "re") + year, data = data_5y)

summary(gam_clim)
draw(gam_clim)
summary(gam_lmm_clim)
draw(gam_lmm_clim)

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
  plot_data <- na.omit(data[, c(var, "crop_diff5")])
  names(plot_data)[1] <- "x"
  
  hist_plot <- ggplot(plot_data, aes(x = x)) +
    geom_histogram(bins = 100)
  
  hist_data <- ggplot_build(hist_plot)$data[[1]]
  
  # Rescale histogram to response axis
  hist_data$y_scaled <-
    hist_data$count / max(hist_data$count) *
    abs(min(marg$predicted) * 500)
  
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
      y = "Logged 5-year crop change"
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
  data     = data_5y,
  var      = "rx5day_ma5",
  xlab     = "5-year moving average of precipitation extreme (rx5day)", 
  filename = "marginal_effect_rx5day_prectemp_ma5_cropdiff5_gam.png"
)

ins_rx5day

ggsave(ins_rx5day, filename = "marginal_effect_rx5day_prectemp_ma5_cropdiff5_gam.png", dpi = 300)

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
  plot_data <- na.omit(data[, c(var, "crop_diff5")])
  names(plot_data)[1] <- "x"
  
  hist_plot <- ggplot(plot_data, aes(x = x)) +
    geom_histogram(bins = 100)
  
  hist_data <- ggplot_build(hist_plot)$data[[1]]
  
  # Rescale histogram to response axis
  hist_data$y_scaled <-
    hist_data$count / max(hist_data$count) *
    abs(min(marg$predicted) * 8)
  
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
      y = "Logged 5-year crop change"
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
  data     = data_5y,
  var      = "tx90_ma5",
  xlab     = "5-year moving average of extreme temperature (TX90)", 
  filename = "marginal_effect_tx90_prectemp_ma5_cropdiff5_gam.png"
)

ins_tx90
ggsave(ins_tx90, filename = "marginal_effect_tx90_prectemp_ma5_cropdiff5_gam.png", dpi = 300)




robust_clim <- rlmer(crop_diff_log ~ rx5day_ma5 +tx90_ma5 + (1 | gwcode) + (1 | year), data = data_5y)

summary(robust_clim)


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
  plot_data <- na.omit(data[, c(var, "crop_diff5")])
  names(plot_data)[1] <- "x"
  
  hist_plot <- ggplot(plot_data, aes(x = x)) +
    geom_histogram(bins = 100)
  
  hist_data <- ggplot_build(hist_plot)$data[[1]]
  
  # Rescale histogram to response axis
  hist_data$y_scaled <-
    hist_data$count / max(hist_data$count) *
    abs(min(marg$predicted) * 8)
  
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
      y = "Logged 5-year crop change"
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
  model    = robust_clim,
  data     = data_5y,
  var      = "rx5day_ma5",
  xlab     = "5-year moving average of precipitation extreme (rx5day)", 
  filename = "marginal_effect_rx5day_prectemp_ma5_cropdiff5_robust.png"
)

ins_rx5day

ggsave(ins_rx5day, filename = "marginal_effect_rx5day_prectemp_ma5_cropdiff5_robust.png", dpi = 300)


ins_tx90 <- plot_marginal_with_hist(
  model    = robust_clim,
  data     = data_5y,
  var      = "tx90_ma5",
  xlab     = "5-year moving average of extreme temperature (TX90)", 
  filename = "marginal_effect_tx90_prectemp_ma5_cropdiff5_robust.png"
)

ins_tx90
ggsave(ins_tx90, filename = "marginal_effect_tx90_prectemp_ma5_cropdiff5_robust.png", dpi = 300)


#########CROSS-VALIDATION##########

#Leave one year out cv SPEI6

# Get all unique years
years <- sort(unique(data_5y$year))

# Initialize results and prediction storage
cv_results <- tibble()
all_preds <- tibble()

# Loop over each year
for (yr in years) {
  cat("Leaving out year:", yr, "\n")
  
  # Train on all other years, test on the left-out year
  train_data <- filter(data_5y, year != yr)
  test_data  <- filter(data_5y, year == yr & !is.na(spei6_ma5))
  
  # Skip fold if no variation in outcome
  if (length(unique(test_data$crop_diff_log)) < 2) {
    cat("Skipping year", yr, "- no variation in crop\n")
    next
  }
  
  # Fit models
  model_full <- gam(crop_diff_log ~ s(spei6_ma5) +  s(gwcode, bs = "re") +  year, data = train_data)
  model_noclim <- gam(crop_diff_log ~   s(gwcode, bs = "re") +  year, data = train_data)
  #model_full <- rlmer(crop_diff_log ~ spei6_ma5 + (1 | gwcode) + (1 | year) , data = train_data)
  #model_noclim <- rlmer(crop_diff_log ~ spei6_ma5 + (1 | gwcode) + (1 | year), data = train_data) 
  # Predictions
  test_data$pred_crop <- predict(model_full, newdata = test_data, allow.new.levels = TRUE)
  test_data$pred_crop_noclim <- predict(model_noclim, newdata = test_data, allow.new.levels = TRUE)
  
  # Evaluation metrics
  rmse_full <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop)^2))
  mae_full  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop))
  r2_full   <- cor(test_data$crop_diff_log, test_data$pred_crop)^2
  
  rmse_noclim <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop_noclim)^2))
  mae_noclim  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop_noclim))
  r2_noclim   <- cor(test_data$crop_diff_log, test_data$pred_crop_noclim)^2
  # Store metrics
  cv_results <- bind_rows(
    cv_results,
    tibble(
      Year = yr,
      Model = c("Full model", "No climate"),
      RMSE = c(rmse_full, rmse_noclim),
      MAE  = c(mae_full, mae_noclim),
      R2   = c(r2_full, r2_noclim)
    )
  )
}

#Model       RMSE   MAE    R2

  #1 Full model  1.20 0.49  0.083
#2 No climate  1.20 0.475 0.088

cv_summary <- cv_results %>% group_by(Model) %>% summarize(RMSE = round(mean(RMSE), 5), 
                                                           MAE = round(mean(MAE), 5), 
                                                           R2 = round(mean(R2), 5))
print(cv_summary)

xtable(cv_summary, digits = c(0, 0, 4, 4, 4),
       caption = "Out-of-sample performance metrics by model using a leave-one-year out cross validation.",
       label = "tab:metrics_temp_crop")

########CV SPI###############


#Leave one year out cv SPI6

# Get all unique years
years <- sort(unique(data_5y$year))

# Initialize results and prediction storage
cv_results <- tibble()
all_preds <- tibble()

# Loop over each year
for (yr in years) {
  cat("Leaving out year:", yr, "\n")
  
  # Train on all other years, test on the left-out year
  train_data <- filter(data_5y, year != yr)
  test_data  <- filter(data_5y, year == yr & !is.na(spi6_ma5))
  
  # Skip fold if no variation in outcome
  if (length(unique(test_data$crop_diff_log)) < 2) {
    cat("Skipping year", yr, "- no variation in crop\n")
    next
  }
  
  # Fit models
  model_full <- gam(crop_diff_log ~ s(spi6_ma5) +  s(gwcode, bs = "re") +  year, data = train_data)
  model_noclim <- gam(crop_diff_log ~   s(gwcode, bs = "re") +  year, data = train_data)
  
  # Predictions
  test_data$pred_crop <- predict(model_full, newdata = test_data, allow.new.levels = TRUE)
  test_data$pred_crop_noclim <- predict(model_noclim, newdata = test_data, allow.new.levels = TRUE)
  
  # Evaluation metrics
  rmse_full <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop)^2))
  mae_full  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop))
  r2_full   <- cor(test_data$crop_diff_log, test_data$pred_crop)^2
  
  rmse_noclim <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop_noclim)^2))
  mae_noclim  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop_noclim))
  r2_noclim   <- cor(test_data$crop_diff_log, test_data$pred_crop_noclim)^2
  # Store metrics
  cv_results <- bind_rows(
    cv_results,
    tibble(
      Year = yr,
      Model = c("Full model", "No climate"),
      RMSE = c(rmse_full, rmse_noclim),
      MAE  = c(mae_full, mae_noclim),
      R2   = c(r2_full, r2_noclim)
    )
  )
}

cv_summary <- cv_results %>% group_by(Model) %>% summarize(RMSE = round(mean(RMSE), 3), 
                                                           MAE = round(mean(MAE), 3), 
                                                           R2 = round(mean(R2), 3))
print(cv_summary)

#Model       RMSE   MAE    R2

 # 1 Full model  1.22 0.528 0.083
#2 No climate  1.20 0.475 0.088
xtable(cv_summary, digits = c(0, 0, 4, 4, 4),
       caption = "Out-of-sample performance metrics by model using a leave-one-year out cross validation.",
       label = "tab:metrics_temp_crop")


#Leave one year out cv TX90

# Get all unique years
years <- sort(unique(data_5y$year))

# Initialize results and prediction storage
cv_results <- tibble()
all_preds <- tibble()

# Loop over each year
for (yr in years) {
  cat("Leaving out year:", yr, "\n")
  
  # Train on all other years, test on the left-out year
  train_data <- filter(data_5y, year != yr)
  test_data  <- filter(data_5y, year == yr & !is.na(tx90_ma5))
  
  # Skip fold if no variation in outcome
  if (length(unique(test_data$crop_diff_log)) < 2) {
    cat("Skipping year", yr, "- no variation in crop\n")
    next
  }
  
  # Fit models
  model_full <- rlmer(crop_diff_log ~ tx90_ma5 + (1 | gwcode) + (1 | year), data = train_data)
  model_noclim <- rlmer(crop_diff_log ~ (1 | gwcode) + (1 | year), data = train_data)
  
  # Predictions
  test_data$pred_crop <- predict(model_full, newdata = test_data, allow.new.levels = TRUE)
  test_data$pred_crop_noclim <- predict(model_noclim, newdata = test_data, allow.new.levels = TRUE)
  
  # Evaluation metrics
  rmse_full <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop)^2))
  mae_full  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop))
  r2_full   <- cor(test_data$crop_diff_log, test_data$pred_crop)^2
  
  rmse_noclim <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop_noclim)^2))
  mae_noclim  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop_noclim))
  r2_noclim   <- cor(test_data$crop_diff_log, test_data$pred_crop_noclim)^2
  # Store metrics
  cv_results <- bind_rows(
    cv_results,
    tibble(
      Year = yr,
      Model = c("Full model", "No climate"),
      RMSE = c(rmse_full, rmse_noclim),
      MAE  = c(mae_full, mae_noclim),
      R2   = c(r2_full, r2_noclim)
    )
  )
}

cv_summary <- cv_results %>% group_by(Model) %>% summarize(RMSE = round(mean(RMSE), 3), 
                                                           MAE = round(mean(MAE), 3), 
                                                           R2 = round(mean(R2), 3))
print(cv_summary)

xtable(cv_summary, digits = c(0, 0, 4, 4, 4),
       caption = "Out-of-sample performance metrics by model using a leave-one-year out cross validation.",
       label = "tab:metrics_temp_crop")




########RX5DAY############


#Leave one year out cv TX90

# Get all unique years
years <- sort(unique(data_5y$year))

# Initialize results and prediction storage
cv_results <- tibble()
all_preds <- tibble()

# Loop over each year
for (yr in years) {
  cat("Leaving out year:", yr, "\n")
  
  # Train on all other years, test on the left-out year
  train_data <- filter(data_5y, year != yr)
  test_data  <- filter(data_5y, year == yr & !is.na(rx5day_ma5))
  
  # Skip fold if no variation in outcome
  if (length(unique(test_data$crop_diff_log)) < 2) {
    cat("Skipping year", yr, "- no variation in crop\n")
    next
  }
  
  # Fit models
  model_full <- rlmer(crop_diff_log ~ rx5day_ma5 + (1 | gwcode) + (1 | year), data = train_data)
  model_noclim <- rlmer(crop_diff_log ~ (1 | gwcode) + (1 | year), data = train_data)
  
  # Predictions
  test_data$pred_crop <- predict(model_full, newdata = test_data, allow.new.levels = TRUE)
  test_data$pred_crop_noclim <- predict(model_noclim, newdata = test_data, allow.new.levels = TRUE)
  
  # Evaluation metrics
  rmse_full <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop)^2))
  mae_full  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop))
  r2_full   <- cor(test_data$crop_diff_log, test_data$pred_crop)^2
  
  rmse_noclim <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop_noclim)^2))
  mae_noclim  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop_noclim))
  r2_noclim   <- cor(test_data$crop_diff_log, test_data$pred_crop_noclim)^2
  # Store metrics
  cv_results <- bind_rows(
    cv_results,
    tibble(
      Year = yr,
      Model = c("Full model", "No climate"),
      RMSE = c(rmse_full, rmse_noclim),
      MAE  = c(mae_full, mae_noclim),
      R2   = c(r2_full, r2_noclim)
    )
  )
}

cv_summary <- cv_results %>% group_by(Model) %>% summarize(RMSE = round(mean(RMSE), 3), 
                                                           MAE = round(mean(MAE), 3), 
                                                           R2 = round(mean(R2), 3))
print(cv_summary)

xtable(cv_summary, digits = c(0, 0, 4, 4, 4),
       caption = "Out-of-sample performance metrics by model using a leave-one-year out cross validation.",
       label = "tab:metrics_rx5day_crop")


###LEAVE ONE OUT CV ALL CLIMATE INDICATORS

#Leave one year out cv all climate indicators

# Get all unique years
years <- sort(unique(data_5y$year))

# Initialize results and prediction storage
cv_results <- tibble()
all_preds <- tibble()

# Loop over each year
for (yr in years) {
  cat("Leaving out year:", yr, "\n")
  
  # Train on all other years, test on the left-out year
  train_data <- filter(data_5y, year != yr)
  test_data  <- filter(data_5y, year == yr & !is.na(spi6_ma5))
  
  # Skip fold if no variation in outcome
  if (length(unique(test_data$crop_diff_log)) < 2) {
    cat("Skipping year", yr, "- no variation in crop\n")
    next
  }
  
  # Fit models
  model_full <-  gam(crop_diff_log ~ s(spi6_ma5) + s(spei6_ma5) + s(tx90_ma5) + s(gwcode, bs = "re") + year, data = train_data)
  model_noclim <-  gam(crop_diff_log ~ s(gwcode, bs = "re") + year, data = train_data)
  
  # Predictions
  test_data$pred_crop <- predict(model_full, newdata = test_data, allow.new.levels = TRUE)
  test_data$pred_crop_noclim <- predict(model_noclim, newdata = test_data, allow.new.levels = TRUE)
  
  # Evaluation metrics
  rmse_full <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop)^2))
  mae_full  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop))
  r2_full   <- cor(test_data$crop_diff_log, test_data$pred_crop)^2
  
  rmse_noclim <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop_noclim)^2))
  mae_noclim  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop_noclim))
  r2_noclim   <- cor(test_data$crop_diff_log, test_data$pred_crop_noclim)^2
  # Store metrics
  cv_results <- bind_rows(
    cv_results,
    tibble(
      Year = yr,
      Model = c("Full model", "No climate"),
      RMSE = c(rmse_full, rmse_noclim),
      MAE  = c(mae_full, mae_noclim),
      R2   = c(r2_full, r2_noclim)
    )
  )
}

cv_summary <- cv_results %>% group_by(Model) %>% summarize(RMSE = round(mean(RMSE), 3), 
                                                           MAE = round(mean(MAE), 3), 
                                                           R2 = round(mean(R2), 3))
print(cv_summary)


#Model       RMSE   MAE    R2

 # 1 Full model  1.23 0.589 0.082
#2 No climate  1.20 0.475 0.088

xtable(cv_summary, digits = c(0, 0, 4, 4, 4),
       caption = "Out-of-sample performance metrics by model using a leave-one-year out cross validation.",
       label = "tab:metrics_temp_crop")



#Leave one year out cv all prec + temp

# Get all unique years
years <- sort(unique(data_5y$year))

# Initialize results and prediction storage
cv_results <- tibble()
all_preds <- tibble()

# Loop over each year
for (yr in years) {
  cat("Leaving out year:", yr, "\n")
  
  # Train on all other years, test on the left-out year
  train_data <- filter(data_5y, year != yr)
  test_data  <- filter(data_5y, year == yr & !is.na(spi6_ma5))
  
  # Skip fold if no variation in outcome
  if (length(unique(test_data$crop_diff_log)) < 2) {
    cat("Skipping year", yr, "- no variation in crop\n")
    next
  }
  
  # Fit models
  model_full <-  gam(crop_diff_log ~ s(rx5day_ma5) + s(tx90_ma5) + s(gwcode, bs = "re") + year, data = train_data)
  model_noclim <-  gam(crop_diff_log ~ s(gwcode, bs = "re") + year, data = train_data)
  #model_full <-  rlmer(crop_diff_log ~ rx5day_ma5  + tx90_ma5 + (1 | gwcode) + (1 | year), data = train_data)
  #model_noclim <-  rlmer(crop_diff_log ~  (1 | gwcode) + (1 | year), data = train_data)
  
  # Predictions
  test_data$pred_crop <- predict(model_full, newdata = test_data, allow.new.levels = TRUE)
  test_data$pred_crop_noclim <- predict(model_noclim, newdata = test_data, allow.new.levels = TRUE)
  
  # Evaluation metrics
  rmse_full <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop)^2))
  mae_full  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop))
  r2_full   <- cor(test_data$crop_diff_log, test_data$pred_crop)^2
  
  rmse_noclim <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop_noclim)^2))
  mae_noclim  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop_noclim))
  r2_noclim   <- cor(test_data$crop_diff_log, test_data$pred_crop_noclim)^2
  # Store metrics
  cv_results <- bind_rows(
    cv_results,
    tibble(
      Year = yr,
      Model = c("Full model", "No climate"),
      RMSE = c(rmse_full, rmse_noclim),
      MAE  = c(mae_full, mae_noclim),
      R2   = c(r2_full, r2_noclim)
    )
  )
}

cv_summary <- cv_results %>% group_by(Model) %>% summarize(RMSE = round(mean(RMSE), 3), 
                                                           MAE = round(mean(MAE), 3), 
                                                           R2 = round(mean(R2), 3))
print(cv_summary)

# A tibble: 2 × 4
#Model       RMSE   MAE    R2
# 1 Full model  1.22 0.575 0.083
#2 No climate  1.20 0.475 0.088


xtable(cv_summary, digits = c(0, 0, 4, 4, 4),
       caption = "Out-of-sample performance metrics by model using a leave-one-year out cross validation.",
       label = "tab:metrics_temp_crop")




#Leave one year out cv all prec + temp with ROBUST MIXED model

# Get all unique years
years <- sort(unique(data_5y$year))

# Initialize results and prediction storage
cv_results <- tibble()
all_preds <- tibble()

# Loop over each year
for (yr in years) {
  cat("Leaving out year:", yr, "\n")
  
  # Train on all other years, test on the left-out year
  train_data <- filter(data_5y, year != yr)
  test_data  <- filter(data_5y, year == yr & !is.na(spi6_ma5))
  
  # Skip fold if no variation in outcome
  if (length(unique(test_data$crop_diff_log)) < 2) {
    cat("Skipping year", yr, "- no variation in crop\n")
    next
  }
  
  # Fit models
  #model_full <-  gam(crop_diff_log ~ s(rx5day_ma5) + s(tx90_ma5) + s(gwcode, bs = "re") + year, data = train_data)
  #model_noclim <-  gam(crop_diff_log ~ s(gwcode, bs = "re") + year, data = train_data)
  model_full <-  rlmer(crop_diff_log ~ rx5day_ma5  + tx90_ma5 + (1 | gwcode) + (1 | year), data = train_data)
  model_noclim <-  rlmer(crop_diff_log ~  (1 | gwcode) + (1 | year), data = train_data)
  
  # Predictions
  test_data$pred_crop <- predict(model_full, newdata = test_data, allow.new.levels = TRUE)
  test_data$pred_crop_noclim <- predict(model_noclim, newdata = test_data, allow.new.levels = TRUE)
  
  # Evaluation metrics
  rmse_full <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop)^2))
  mae_full  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop))
  r2_full   <- cor(test_data$crop_diff_log, test_data$pred_crop)^2
  
  rmse_noclim <- sqrt(mean((test_data$crop_diff_log - test_data$pred_crop_noclim)^2))
  mae_noclim  <- mean(abs(test_data$crop_diff_log - test_data$pred_crop_noclim))
  r2_noclim   <- cor(test_data$crop_diff_log, test_data$pred_crop_noclim)^2
  # Store metrics
  cv_results <- bind_rows(
    cv_results,
    tibble(
      Year = yr,
      Model = c("Full model", "No climate"),
      RMSE = c(rmse_full, rmse_noclim),
      MAE  = c(mae_full, mae_noclim),
      R2   = c(r2_full, r2_noclim)
    )
  )
}

cv_summary <- cv_results %>% group_by(Model) %>% summarize(RMSE = round(mean(RMSE), 4), 
                                                           MAE = round(mean(MAE), 4), 
                                                           R2 = round(mean(R2), 4))
print(cv_summary)

#Model       RMSE   MAE    R2
#  1 Full model  1.02 0.327 0.127
# 2 No climate  1.02 0.328 0.122

xtable(cv_summary, digits = c(0, 0, 4, 4, 4),
       caption = "Out-of-sample performance metrics by model using a leave-one-year out cross validation.",
       label = "tab:metrics_temp_crop")


#####Leave one out cv by country

# Get all unique countries
countries <- sort(unique(data_5y$gwcode))

# Initialize results
cv_results <- tibble()

for (ctry in countries) {
  cat("Leaving out country:", ctry, "\n")
  
  # Train / test split
  train_data <- filter(data_5y, gwcode != ctry)
  test_data  <- filter(data_5y, gwcode == ctry & !is.na(spei6_ma5))
  
  # Skip if no outcome variation
  if (length(unique(test_data$crop_diff_log)) < 2) {
    cat("Skipping country", ctry, "- no variation in outcome\n")
    next
  }
  
 # model_full <-  gam(crop_diff_log ~   s(tx90_ma5)  + s(spei6_ma5) + s(spi6_ma5) + s(gwcode, bs = "re") + year, data = train_data)
 # model_noclim <-  gam(crop_diff_log ~ s(gwcode, bs = "re") + year, data = train_data)
  model_full <-  rlmer(crop_diff_log ~ spi6_ma5 + spei6_ma5 + tx90_ma5 + (1 | gwcode) + (1 | year), data = train_data)
  model_noclim <-  rlmer(crop_diff_log ~  (1 | gwcode) + (1 | year), data = train_data)
  
  
  # Predictions
  test_data$pred_full   <- predict(model_full, newdata = test_data, allow.new.levels = TRUE)
  test_data$pred_noclim <- predict(model_noclim, newdata = test_data, allow.new.levels = TRUE)
  
  # Metrics
  rmse_full   <- sqrt(mean((test_data$crop_diff_log - test_data$pred_full)^2))
  mae_full    <- mean(abs(test_data$crop_diff_log - test_data$pred_full))
  r2_full     <- cor(test_data$crop_diff_log, test_data$pred_full)^2
  
  rmse_noclim <- sqrt(mean((test_data$crop_diff_log - test_data$pred_noclim)^2))
  mae_noclim  <- mean(abs(test_data$crop_diff_log - test_data$pred_noclim))
  r2_noclim   <- cor(test_data$crop_diff_log, test_data$pred_noclim)^2
  
  # Store
  cv_results <- bind_rows(
    cv_results,
    tibble(
      Country = ctry,
      Model   = c("Full model", "No climate"),
      RMSE    = c(rmse_full, rmse_noclim),
      MAE     = c(mae_full, mae_noclim),
      R2      = c(r2_full, r2_noclim)
    )
  )
}

# Summary table
cv_summary <- cv_results %>%
  group_by(Model) %>%
  summarize(
    RMSE = round(mean(RMSE), 3),
    MAE  = round(mean(MAE), 3),
    R2   = round(mean(R2), 3)
  )

print(cv_summary)

#ALL INDICATORS 
#Model       RMSE   MAE    R2
  #1 Full model 1.05  0.528 0.091
#2 No climate 0.946 0.447 0.12

#TX90 + SPEI

#  1 Full model 1.03  0.462 0.105
#2 No climate 0.966 0.416 0.12 

xtable(
  cv_summary,
  digits  = c(0, 0, 4, 4, 4),
  caption = "Out-of-sample performance using leave-one-country-out cross-validation (All climate indicators vs no climate).",
  label   = "tab:cv_country"
)



#####Leave one out cv by country

# Get all unique countries
countries <- sort(unique(data_5y$gwcode))

# Initialize results
cv_results <- tibble()

for (ctry in countries) {
  cat("Leaving out country:", ctry, "\n")
  
  # Train / test split
  train_data <- filter(data_5y, gwcode != ctry)
  test_data  <- filter(data_5y, gwcode == ctry & !is.na(spei6_ma5))
  
  # Skip if no outcome variation
  if (length(unique(test_data$crop_diff_log)) < 2) {
    cat("Skipping country", ctry, "- no variation in outcome\n")
    next
  }
  
   model_full <-  gam(crop_diff_log ~   s(tx90_ma5)  + s(rx5day_ma5) + s(gwcode, bs = "re") + year, data = train_data)
   model_noclim <-  gam(crop_diff_log ~ s(gwcode, bs = "re") + year, data = train_data)
  #model_full <-  rlmer(crop_diff_log ~ rx5day_ma5  + tx90_ma5 + (1 | gwcode) + (1 | year), data = train_data)
  #model_noclim <-  rlmer(crop_diff_log ~  (1 | gwcode) + (1 | year), data = train_data)
  
  
  # Predictions
  test_data$pred_full   <- predict(model_full, newdata = test_data, allow.new.levels = TRUE)
  test_data$pred_noclim <- predict(model_noclim, newdata = test_data, allow.new.levels = TRUE)
  
  # Metrics
  rmse_full   <- sqrt(mean((test_data$crop_diff_log - test_data$pred_full)^2))
  mae_full    <- mean(abs(test_data$crop_diff_log - test_data$pred_full))
  r2_full     <- cor(test_data$crop_diff_log, test_data$pred_full)^2
  
  rmse_noclim <- sqrt(mean((test_data$crop_diff_log - test_data$pred_noclim)^2))
  mae_noclim  <- mean(abs(test_data$crop_diff_log - test_data$pred_noclim))
  r2_noclim   <- cor(test_data$crop_diff_log, test_data$pred_noclim)^2
  
  # Store
  cv_results <- bind_rows(
    cv_results,
    tibble(
      Country = ctry,
      Model   = c("Full model", "No climate"),
      RMSE    = c(rmse_full, rmse_noclim),
      MAE     = c(mae_full, mae_noclim),
      R2      = c(r2_full, r2_noclim)
    )
  )
}

# Summary table
cv_summary <- cv_results %>%
  group_by(Model) %>%
  summarize(
    RMSE = round(mean(RMSE), 3),
    MAE  = round(mean(MAE), 3),
    R2   = round(mean(R2), 3)
  )

print(cv_summary)




