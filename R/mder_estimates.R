library(dplyr)
library(lubridate)
library(tidyr)

bmi <- function(weight, height){
  # weight in kg, height in m
  weight / height^2
}

weight_from_bmi <- function(bmi, height){
  # weight in kg, height in m
  bmi * height^2
}


weight_for_length_girls <- readxl::read_xlsx("data_raw/wfl-girls-percentiles-expanded-tables.xlsx")
weight_for_length_boys <- readxl::read_xlsx("data_raw/wfl-girls-percentiles-expanded-tables.xlsx")
weight_for_height_girls <- readxl::read_xlsx("data_raw/wfh-girls-percentiles-expanded-tables.xlsx")
weight_for_height_boys <- readxl::read_xlsx("data_raw/wfh-boys-percentiles-expanded-tables.xlsx")
bmi_girls <- readxl::read_xlsx("data_raw/bmi-girls-perc-who2007-exp.xlsx")
bmi_boys <- readxl::read_xlsx("data_raw/bmi-boys-perc-who2007-exp.xlsx")

height_for_age_girls <- readxl::read_xlsx("data_raw/hfa-girls-perc-who2007-exp.xlsx")
height_for_age_boys <- readxl::read_xlsx("data_raw/hfa-boys-perc-who2007-exp.xlsx")
length_height_for_age_girls <- readxl::read_xlsx("data_raw/lhfa-girls-percentiles-expanded-tables.xlsx")
length_height_for_age_boys <- readxl::read_xlsx("data_raw/lhfa-boys-percentiles-expanded-tables.xlsx")


lhg <- length_height_for_age_girls |> mutate(
  year = ddays(Day) %/% as.duration(years(1))
) |> group_by(year) |>
  summarize(median_height = mean(P50) |> round(digits = 1)) |> mutate(sex = "female") |>
  left_join(weight_for_length_girls |> dplyr::select(Length, "median_weight" = P50), by = c("median_height" = "Length"))

lhb <- length_height_for_age_boys |> mutate(
  year = ddays(Day) %/% as.duration(years(1))
) |> group_by(year) |>
  summarize(median_height = mean(P50) |> round(digits = 1)) |> mutate(sex = "male") |>
  left_join(weight_for_length_boys |> dplyr::select(Length, "median_weight" = P50), by = c("median_height" = "Length"))
lhb$median_weight[lhb$year == 5] <- weight_for_length_boys |> filter(Length == max(Length)) |> pull(P50) # impute this


hg <- height_for_age_girls |> mutate(
  year = dmonths(Month) %/% as.duration(years(1)) + 1
) |> group_by(year) |>
  summarize(median_height = mean(P50) |> round(digits = 1)) |> mutate(sex = "female") |>
  left_join(weight_for_height_girls |> dplyr::select(Height, "median_weight" = P50), by = c("median_height" = "Height"))

hb <- height_for_age_boys |> mutate(
  year = dmonths(Month) %/% as.duration(years(1)) + 1
) |> group_by(year) |>
  summarize(median_height = mean(P50) |> round(digits = 1)) |> mutate(sex = "male") |>
  left_join(weight_for_height_boys |> dplyr::select(Height, "median_weight" = P50), by = c("median_height" = "Height"))

bmib <- bmi_boys |> mutate(
  year = dmonths(Month) %/% as.duration(years(1))+ 1)   |>
  group_by(year) |>
  summarize(median_bmi = mean(P50), p5_bmi = mean(P5)) |> mutate(sex = "male")

bmig <- bmi_girls |> mutate(
  year = dmonths(Month) %/% as.duration(years(1))+ 1)   |>
  group_by(year) |>
  summarize(median_bmi = mean(P50), p5_bmi = mean(P5)) |> mutate(sex = "female")

df <- left_join(bind_rows(hb, hg), bind_rows(bmib, bmig), by = c("year", "sex")) |> mutate(
  bmi_weight_p50 = weight_from_bmi(median_bmi, median_height/100),
  bmi_weight_p5 = weight_from_bmi(p5_bmi, median_height/100)
)

df <- bind_rows(df, bind_rows(lhg, lhb)) |> arrange(year, sex) |>
  mutate(weight_est =
           case_when(
            !is.na(median_weight) & year < 10 ~ median_weight,
            is.na(median_weight) & year < 10 ~ bmi_weight_p50,
            year >= 10 ~ bmi_weight_p5)
           )
df <- df |> dplyr::select(year, sex, "mder_height" = median_height, "mder_weight" = weight_est)
age_sequence <- seq(-1, 19, 5)
age_sequence <- c(age_sequence, 20)
df_ag <- df |> mutate(age_group = cut(year, breaks = age_sequence, include.lowest = F)) |>
  group_by(age_group, sex) |>
  summarize(mder_height = mean(mder_height),
            mder_weight = mean(mder_weight))

df_ag <- df_ag |> mutate(
  age_group = case_when(
    age_group == "(-1,4]" ~ "0-4",
    age_group == "(4,9]" ~ "5-9",
    age_group == "(9,14]" ~ "10-14",
    age_group == "(14,19]" ~ "15-19",
    age_group == "(19,20]" ~ "20-24|25-29|30-34|35-39|40-44|45-49|50-54|55-59|60-64|65-69|70-74|75-79|80-84|85-89|90-94|95-99|100+",
  )
) |>
  mutate(
    age_group = stringr::str_split(age_group, "\\|")
  ) |>
  unnest(cols = c("age_group"))


schofield_equations <- function(kg, sex, age_group){
  case_when(
    age_group == "<3" & sex == "male"  ~ (59.512 * kg) - 30.4,
    age_group == "3-10" & sex == "male"  ~ (22.706 * kg) + 504.3,
    age_group == "10-18" & sex == "male"  ~ (17.686 * kg) + 658.2,
    age_group == "18-30" & sex == "male"  ~ (15.057 * kg) + 692.2,
    age_group == "30-60" & sex == "male"  ~ (11.472 * kg) + 873.1,
    age_group == ">60" & sex == "male"  ~ (11.711 * kg) + 587.7,

    age_group == "<3" & sex == "female"  ~ (58.317 * kg) - 31.1,
    age_group == "3-10" & sex == "female"  ~ (20.315 * kg) + 485.9,
    age_group == "10-18" & sex == "female"  ~ (13.384 * kg) + 692.6,
    age_group == "18-30" & sex == "female"  ~ (14.818 * kg) + 486.6,
    age_group == "30-60" & sex == "female"  ~ (8.126 * kg) + 845.6,
    age_group == ">60" & sex == "female"  ~ (9.082 * kg) + 658.5)
}

schofield_equations_adj <- function(kg, sex, age_group){
  case_when(
    age_group == "0-4" & sex == "male"  ~ (59.512 * kg) - 30.4,
    age_group == "5-9" & sex == "male"  ~ (22.706 * kg) + 504.3,
    age_group %in% c("10-14", "15-19") & sex == "male"  ~ (17.686 * kg) + 658.2,
    age_group %in% c("20-24", "25-29") & sex == "male"  ~ (15.057 * kg) + 692.2,
    age_group %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59") & sex == "male"  ~ (11.472 * kg) + 873.1,
    age_group %in% c("60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+") & sex == "male"  ~ (11.711 * kg) + 587.7,

    age_group == "0-4" & sex == "female"  ~ (58.317 * kg) - 31.1,
    age_group == "5-9" & sex == "female"  ~ (20.315 * kg) + 485.9,
    age_group %in% c("10-14", "15-19") & sex == "female"  ~ (13.384 * kg) + 692.6,
    age_group %in% c("20-24", "25-29") & sex == "female"  ~ (14.818 * kg) + 486.6,
    age_group %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59") & sex == "female"  ~ (8.126 * kg) + 845.6,
    age_group %in% c("60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+") & sex == "female"  ~ (9.082 * kg) + 658.5)
}

physical_activity_level_factor <- function(sex, activity){
  if(sex == "male"){
    x <- case_when(
      activity == "sedentary" ~ 1.3,
      activity == "lightly active" ~ 1.6,
      activity == "moderate activity" ~ 1.7,
      activity == "very active" ~ 2.1,
      activity == "extremely active" ~ 2.4,
    )
    return(x)
  }
  if(sex == "female"){
    x <- case_when(
      activity == "sedentary" ~ 1.3,
      activity == "lightly active" ~ 1.5,
      activity == "moderate activity" ~ 1.6,
      activity == "very active" ~ 1.9,
      activity == "extremely active" ~ 2.2,
    )
    return(x)
  }
}

df_ag <- df_ag |>
  mutate(pal = sapply(sex, function(x) physical_activity_level_factor(x, "lightly active"))) |>
  mutate(bmr = schofield_equations_adj(mder_weight, sex, age_group)) |>
  mutate(mer = bmr * pal)

write_csv(df_ag, "data/mder_estimates.csv")
