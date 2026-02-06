# Load and prepare main data
main_df <- data.table::fread("data/imputed_main.csv")
data.table::setnames(main_df, "calorie_var", "cv")

unit_var <- "gwcode"
time_var <- "year"
outcomes <- c("des", "pou", "mder", "cv")
explanatory_variables <- c("best", "gdppc", "population",
													 "v2x_polyarchy", "secprop", "tx90pgs", "spei6gs", "spi6gs", "rx5daygs", "tas")

main_df <- main_df[, c(unit_var, time_var, outcomes, explanatory_variables), with = FALSE]

if(TIME_INTERVAL > 1){
	mean_vars <- setdiff(names(main_df), c("gwcode", "year", "best"))
	main_df[, period := (year - min(year)) %/% TIME_INTERVAL, by = gwcode]
	main_df <- main_df[, c(
		list(year = max(year), best = sum(best, na.rm = TRUE)),
		lapply(.SD, mean, na.rm = TRUE)
	), by = .(gwcode, period), .SDcols = mean_vars]
} else{
	# This just makes it easier to code later.
	main_df$period <- main_df$year
}


