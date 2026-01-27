library(data.table)
library(ggplot2)

# Probabilistic Democracy and Conflict projections
ssp1_brd <- fread("data/simbayes/ssp1_brd.csv")[,-"region"]
ssp2_brd <- fread("data/simbayes/ssp2_brd.csv")[,-"region"]
ssp3_brd <- fread("data/simbayes/ssp3_brd.csv")[,-"region"]
ssp4_brd <- fread("data/simbayes/ssp4_brd.csv")[,-"region"]
ssp5_brd <- fread("data/simbayes/ssp5_brd.csv")[,-"region"]

ssp1_dem <- fread("data/sim/ssp1_dem.csv")[,-c("region", "country_name")]
ssp2_dem <- fread("data/sim/ssp2_dem.csv")[,-c("region", "country_name")]
ssp3_dem <- fread("data/sim/ssp3_dem.csv")[,-c("region", "country_name")]
ssp4_dem <- fread("data/sim/ssp4_dem.csv")[,-c("region", "country_name")]
ssp5_dem <- fread("data/sim/ssp5_dem.csv")[,-c("region", "country_name")]

dem_obs <- ssp1_dem[sim == 0] # sim 0 is historical data, add to each simulation.
for(s in 1:500){
	add_this <- dem_obs
	add_this$sim <- add_this$sim + s
	ssp1_dem <- rbindlist(list(ssp1_dem, add_this))
	ssp2_dem <- rbindlist(list(ssp2_dem, add_this))
	ssp3_dem <- rbindlist(list(ssp3_dem, add_this))
	ssp4_dem <- rbindlist(list(ssp4_dem, add_this))
	ssp5_dem <- rbindlist(list(ssp5_dem, add_this))
}

ssp_1_dem <- ssp1_dem[sim != 0]
ssp_2_dem <- ssp2_dem[sim != 0]
ssp_3_dem <- ssp3_dem[sim != 0]
ssp_4_dem <- ssp4_dem[sim != 0]
ssp_5_dem <- ssp5_dem[sim != 0]

ssp1 <- merge(ssp1_brd, ssp1_dem, by = c("gwcode", "year", "sim"))
ssp2 <- merge(ssp2_brd, ssp2_dem, by = c("gwcode", "year", "sim"))
ssp3 <- merge(ssp3_brd, ssp3_dem, by = c("gwcode", "year", "sim"))
ssp4 <- merge(ssp4_brd, ssp4_dem, by = c("gwcode", "year", "sim"))
ssp5 <- merge(ssp5_brd, ssp5_dem, by = c("gwcode", "year", "sim"))

ssp1$scenario = "SSP1"
ssp2$scenario = "SSP2"
ssp3$scenario = "SSP3"
ssp4$scenario = "SSP4"
ssp5$scenario = "SSP5"

base_projections <- rbindlist(list(ssp1, ssp2, ssp3, ssp4, ssp5))
data.table::setnames(base_projections, old = "pol", new = "v2x_polyarchy")
base_projections <- base_projections[year >= 2001 & year <= 2050]

projected_countries <- base_projections$gwcode |> unique()

harmonized_ssp <- fread("data/harmonized_ssp.csv")[,-"crop_country_share"]
projdf <- harmonized_ssp[gwcode %in% projected_countries & year >= 2001]

countries_with_missing_data <- projdf[!complete.cases(projdf),]$gwcode |> unique()
projdf <- projdf[!gwcode %in% countries_with_missing_data, -c("best", "v2x_polyarchy")]

base_projections <- base_projections[!gwcode %in% countries_with_missing_data]
base_projections <- merge(base_projections, projdf, by = c("scenario", "gwcode", "year"))

base_projections[year >= 2010, .(gdppc = median(v2x_polyarchy, na.rm = TRUE)), by = .(scenario, year)] |>
	ggplot(aes(x = year, y = gdppc, color = scenario)) + geom_line()


arrow::write_parquet(base_projections, "data/base_projections.parquet")
