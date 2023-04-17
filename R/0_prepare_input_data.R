library(readr)

source("R/functions/fetch_mye.R")
source("R/functions/process_mye_data.R")

fpath <- list(dir_raw = "data/raw/",
              dir_intermediate = "data/intermediate/",
              dir_processed = "data/processed/",
              raw_original_mye = "data/raw/original_mye_coc.csv",
              mye_coc = "data/intermediate/mye_coc.rds",
              od_series = "data/intermediate/od_series.rds"
              )

if(!dir.exists(fpath$dir_raw)) {dir.create(fpath$dir_raw)}
if(!dir.exists(fpath$dir_intermediate)) {dir.create(fpath$dir_intermediate)}
if(!dir.exists(fpath$dir_processed)) {dir.create(fpath$dir_processed)}

urls <- list(original_mye = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2021/dataforreconciliation.zip",
             detailed_od_series = "https://data.london.gov.uk/download/modelled-population-backseries/6b9d6296-db41-4b7f-901c-2a5e5c5b44d5/origin_destination_2002_to_2020_%282021_geog%29.rds")

#fetch MYE data
fetch_mye(fpath$raw_original_mye, urls$original_mye)

#clean data and save full component files
process_mye_data(fpath$raw_original_mye,
                 fpath$mye_coc,
                 src_name = "ONS MYE original")

#fetch origin destination data
download.file(url = urls$detailed_od_series, destfile = fpath$od_series)
