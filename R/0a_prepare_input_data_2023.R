library(readr)

source("R/functions/fetch_mye.R")
source("R/functions/process_mye_data.R")

fpath <- list(dir_raw = "data/raw/",
              dir_intermediate = "data/intermediate/",
              dir_processed = "data/processed/",
              mye_coc = "data/intermediate/mye_coc.rds",
              od_series = "data/intermediate/od_series.rds"
              )

if(!dir.exists(fpath$dir_raw)) {dir.create(fpath$dir_raw)}
if(!dir.exists(fpath$dir_intermediate)) {dir.create(fpath$dir_intermediate)}
if(!dir.exists(fpath$dir_processed)) {dir.create(fpath$dir_processed)}

urls <- list(mye_coc = "https://data.london.gov.uk/download/modelled-population-backseries/2b07a39b-ba63-403a-a3fc-5456518ca785/full_modelled_estimates_series_EW%282023_geog%29.rds",
             detailed_od_series = "https://data.london.gov.uk/download/modelled-population-backseries/fd5d7b6a-02a4-4087-822a-1cfda209a6c3/full_series_lad.rds")

#fetch component data from London Datastore
download.file(url = urls$mye_coc, destfile = fpath$mye_coc)

#fetch origin destination data from London Datastore
download.file(url = urls$detailed_od_series, destfile = fpath$od_series)
