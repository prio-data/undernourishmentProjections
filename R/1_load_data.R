# Load and prepare main data
main_df <- data.table::fread("data/imputed_main.csv")
data.table::setnames(main_df, "calorie_var", "cv")

unit_var <- "gwcode"
time_var <- "year"
outcomes <- c("des", "pou", "mder", "cv")
explanatory_variables <- c("best", "gdppc", "population",
													 "v2x_polyarchy", "secprop", "tx90pgs", "spei6gs", "spi6gs", "rx5daygs", "tas")

main_df <- main_df[, c(unit_var, time_var, outcomes, explanatory_variables), with = FALSE]


