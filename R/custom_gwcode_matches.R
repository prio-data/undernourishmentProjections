french_overseas_regions <- c(
  "French Guiana" = 220,
  "Guadeloupe" = 220,
  "Martinique" = 220,
  "Mayotte" = 220,
  "Reunion" = 220,
  "Réunion" = 220
)
french_overseas_collectivities <- c(
  "French Polynesia" = 960,
  "New Caledonia" = 930,
  # Saint Barthélemy
  "Saint Martin (French part)" = 220
  # Saint Pierre and Miquelon
  # Wallis and Futuna
)

us_overseas_territories <- c(
  # "Guam" = 7,
  "Puerto Rico" = 6,
  # "United States Virgin Islands" = 8,
  # "U.S. Virgin Islands" = 8,
  "American Samoa" = 2,
  "Northern Mariana Islands" = 2
)

dutch_caribbean <- c(
  # "Bonaire" = 210,
  # "Sint Eustatius" = 210,
  # "Saba" = 210
  "Aruba" = 102,
  "Curaçao" = 103,
  "Sint Maarten" = 210
)

china_sar <- c(
  "Hong Kong SAR China" = 710,
  "Hong Kong Special Administrative Region of China" = 710,
  "Macao Special Administrative Region of China" = 710,
  "Macao SAR China" = 710
)

uk_overseas_territories <- c(
  "Anguilla" = 200,
  "Bermuda" = 200,
  "British Virgin Islands" = 200,
  "Cayman Islands" = 200,
  "Montserrat" = 200,
  "Turks & Caicos Islands" = 200,
  "Gibraltar" = 200,
  "Isle of Man" = 200,
  "Channel Islands" = 200
)

denmark_overseas_territories <- c(
  "Greenland" = 390,
  "Faroe Islands" = 390
)

palestine <- c("Palestine" = 665,
               "Palestine/British Mandate" = 665,
               "Palestine/West Bank" = 6631,
               "Palestine/Gaza" = 6511,
               "Occupied Palestinian Territories" = 699,
               "Occupied Palestinian Territory" = 699,
               "Palestinian Territories" = 699,
               "West Bank" = 6631,
               "Gaza" = 6511)
western_sahara <- c("Western Sahara" = 609)
somaliland <- c("Somaliland" = 520) # We code Somaliland as Somalia.
vietnam <- c("Vietnam" = 816) # This is often coded as 817
yemen <- c("Yemen" = 678)
kosovo <- c("Kosovo" = 347)


tmp <- tempfile()
"http://ksgleditsch.com/data/microstates.txt" |>
  httr2::request() |>
  httr2::req_perform(path = tmp)
microstates <- read.csv(tmp, sep = "\t")
unlink(tmp)

microstates$countryname <- iconv(microstates$countryname, from = "latin1")
microstates <- microstates |> dplyr::select(countryname, gwcode = statenumber)

ms <- microstates$gwcode
names(ms) <- microstates$countryname


# Other ways to write...
sao_tome <- c("São Tomé & Príncipe" = 403)
st_kitts <- c("St. Kitts & Nevis" = 60)
st_lucia <- c("St. Lucia" = 56)
st_vincent <- c("St. Vincent & Grenadines" = 57)


custom_gwcode_matches <- c(french_overseas_regions,
                            french_overseas_collectivities,
                            us_overseas_territories,
                            uk_overseas_territories,
                            dutch_caribbean,
                            denmark_overseas_territories,
                            china_sar,
                            palestine,
                            western_sahara,
                            yemen,
                            somaliland,
                            vietnam,
                            kosovo,
                            ms,
                            sao_tome,
                           st_kitts,
                           st_lucia,
                           st_vincent)
