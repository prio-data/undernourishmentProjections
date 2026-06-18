# Replication data for "Global hunger risk in alternative climate change and socio-political scenarios"

## How to setup

1. Download and unzip the data from Harvard Dataverse: https://doi.org/10.7910/DVN/VMO2Y7. Note that the zip-file is 562.4 MB and 7.6GB unzipped.
```R
# Assuming you have made a new R-project in a new folder
install.packages("renv")
renv::install("dataverse")

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
f <- get_file("undernourishmentProjectionsHarvard.zip", "doi:10.7910/DVN/VMO2Y7")
writeBin(f, "undernourishmentProjectionsHarvard.zip")
unzip("undernourishmentProjectionsHarvard.zip")
# You might here want to move the extracted content into the project, or setwd("undernourishmentProjectionsHarvard/")
```

2. Install required packages (including the https://github.com/prio-data/heterolm package, and possibly also https://github.com/kvelleby/poldat)
```R
renv::install()
```

4. Run "run_this.R"
```R
source("run_this.R")
```
