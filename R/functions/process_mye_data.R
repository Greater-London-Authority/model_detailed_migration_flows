library(readr)
library(dplyr)
library(tidyr)
library(stringr)

process_mye_data <- function(fp_raw, fp_save,
                             src_name = "") {

  name_lookup <- c("gss_code" = "ladcode21",
                   "gss_name" = "ladname21")

  out_df <- read_csv(fp_raw) %>%
    rename(any_of(name_lookup)) %>%
    pivot_longer(cols = -any_of(c("gss_code", "gss_name", "geography", "country", "sex", "age")),
                 values_to = "value",
                 names_to = "component_year") %>%
    mutate(year = as.numeric(str_sub(component_year, -4, -1)),
           component = str_sub(component_year, 1, -6),
           sex = as.character(sex),
           sex = recode(sex,
                        "1" = "female",
                        "2" = "male")) %>%
    select(-component_year) %>%
    mutate(source = src_name)

  saveRDS(out_df, fp_save)
}


