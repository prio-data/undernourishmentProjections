# Replication data for "Global hunger risk in alternative climate change and socio-political scenarios"

## How to setup

1. Clone this repository in the folder you would want it.
```bash
git clone https://github.com/prio-data/undernourishmentProjections.git
```
2. Install required packages (including the "https://github.com/prio-data/heterolm" package, and possibly also "https://github.com/kvelleby/poldat")
```R
renv::install()
```
3. Download and unzip the data
```R
data_url <- "TBA"
data_file <- file.path("data_raw", "data.zip")
download.file(data_url, destfile = data_file)
unzip(data_file)
```
4. Run "run_this.R"
```R
source("run_this.R")
```
