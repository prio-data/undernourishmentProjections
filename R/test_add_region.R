
# data <- data.table(country_name = c("United States of America", "Brazil", "Russia (Soviet Union)", "Germany", "India", "Kenya", "Mexico"))


# Mapping av country_name til region (inkluderer alternative navn fra din liste)
region_map <- list(
	"OECD" = c(
		"United States of America", "Puerto Rico", "Canada", "United Kingdom", "Ireland", "Netherlands",
		"Belgium", "France", "Switzerland", "Spain", "Portugal", "German Federal Republic", "Germany",
		"Poland", "Austria", "Hungary", "Czech Republic", "Slovakia", "Italy/Sardinia", "Malta",
		"Albania", "Serbia", "Montenegro", "Macedonia (FYROM/North Macedonia)", "Croatia",
		"Bosnia-Herzegovina", "Slovenia", "Greece", "Cyprus", "Bulgaria", "Estonia", "Latvia",
		"Lithuania", "Norway", "Sweden", "Finland", "Denmark", "Iceland", "Australia",
		"New Zealand", "Japan", "Turkey (Ottoman Empire)", "Rumania"
	),

	"REF" = c(
		"Russia (Soviet Union)", "Belarus (Byelorussia)", "Ukraine", "Moldova", "Armenia", "Georgia",
		"Azerbaijan", "Turkmenistan", "Tajikistan", "Kyrgyz Republic", "Uzbekistan", "Kazakhstan"
	),

	"ASIA" = c(
		"Afghanistan", "Bangladesh", "Bhutan", "Brunei", "Cambodia (Kampuchea)", "China", "Korea, People's Republic of",
		"India", "Indonesia", "Laos", "Malaysia", "Maldives", "Mongolia", "Myanmar (Burma)", "Nepal",
		"Pakistan", "Philippines", "Republic of Korea", "Korea, Republic of", "Fiji", "French Polynesia",
		"Singapore", "Solomon Islands", "Sri Lanka (Ceylon)", "Taiwan", "Thailand", "East Timor",
		"Vietnam, Democratic Republic of", "Papua New Guinea", "New Caledonia and Dependencies"
	),

	"MAF" = c(
		"Algeria", "Angola", "Bahrain", "Benin", "Botswana", "Burkina Faso (Upper Volta)", "Burundi",
		"Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo",
		"Congo, Democratic Republic of (Zaire)", "Cote D'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea",
		"Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Iran (Persia)",
		"Iraq", "Israel", "Jordan", "Kenya", "Kuwait", "Lebanon", "Lesotho", "Liberia", "Libya",
		"Madagascar (Malagasy)", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique",
		"Namibia", "Niger", "Nigeria", "Oman", "Occupied Palestinian Territory", "Qatar", "Rwanda",
		"Réunion", "Saudi Arabia", "Senegal", "Sierra Leone", "Somalia", "South Africa", "South Sudan",
		"Sudan", "Swaziland (Eswatini)", "Syrian Arab Republic", "Togo", "Tunisia", "Uganda",
		"United Arab Emirates", "United Republic of Tanzania", "Western Sahara", "Yemen (Arab Republic of Yemen)",
		"Zambia", "Zimbabwe (Rhodesia)", "Syria", "Tanzania (Tanganyika)"
	),

	"LAM" = c(
		"Argentina", "Aruba", "Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", "Chile",
		"Colombia", "Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "El Salvador",
		"French Guiana", "Grenada", "Guadeloupe", "Guatemala", "Guyana", "Haiti", "Honduras",
		"Jamaica", "Martinique", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Surinam",
		"Trinidad and Tobago", "United States Virgin Islands", "Uruguay", "Venezuela"
	)
)

region_dt <- rbindlist(lapply(names(region_map), function(region) {
	data.table(country_name = region_map[[region]], region = region)
}))

data <- merge(data, region_dt, by = "country_name", all.x = TRUE)

