library(dplyr)
library(tidyr)
library(ipfp)
library(mipfp)
library(reshape2)

source("R/functions/optimise_gross_flows.R")

fit_od_flows <- function(tgt_inflow_df, tgt_outflow_df, seed_df, number_iterations = 40000){

  tgt_inflow_list <- tgt_inflow_df %>%
    split(~age + sex)

  tgt_outflow_list <- tgt_outflow_df %>%
    split(~age + sex)

  seed_list <- seed_df %>%
    split(~age + sex)

  c_age_sex <- names(tgt_inflow_list)

  output_list <- vector(mode = "list", length = length(c_age_sex))
  names(output_list) <- c_age_sex

  ####### Fit origin-destination flows

  # formulae used to cast data frames into arrays
  tgt_inflow_formula <- "gss_in ~ age ~ sex"
  tgt_outflow_formula <- "gss_out ~ age ~ sex"
  seed_formula <- "gss_out ~ gss_in ~ age ~ sex"

  # flows are fit for single combination of age and sex at a time (this is more efficient that fitting for all at once)
  # the process loops over age and sex

  for(sel_age_sex in c_age_sex) {

    message(format(Sys.time(), "%X"), " - ", sel_age_sex)

    ### filter target and seed data frames by age/sex and cast into arrays
    target_inflow <- acast(data = tgt_inflow_list[[sel_age_sex]],
                           formula = as.formula(tgt_inflow_formula),
                           value.var = "value",
                           fill = 0)

    target_outflow <- acast(data = tgt_outflow_list[[sel_age_sex]],
                            formula = as.formula(tgt_outflow_formula),
                            value.var = "value",
                            fill = 0)

    seed_tbl <- acast(data = seed_list[[sel_age_sex]],
                      formula = as.formula(seed_formula),
                      value.var = "value",
                      fill = 0.01)

    #create list of target data and also list defining relationships to contingency table dimensions
    target_data <- list("outflows" = target_outflow,
                        "inflows" = target_inflow)

    target_dimension_list <- list("outflows" = c(1, 3, 4),
                                  "inflows" = c(2, 3, 4))

    total_count <- sum(target_outflow)

    #### fit table and convert output to data frame
    results.ipfp <- Estimate(seed = seed_tbl,
                             target.list = target_dimension_list,
                             target.data = target_data,
                             method = "ipfp",
                             iter = number_iterations)

    output <- melt(results.ipfp$p.hat, value.name = "value") %>%
      mutate_at(vars(-value), funs(as.character)) %>%
      mutate(value = as.numeric(value) * total_count) %>%
      filter(value > 0)

    colnames(output) <- c("gss_out", "gss_in", "age", "sex", "value")

    output_list[[sel_age_sex]] <- output

  }

  full_output <- bind_rows(output_list)

  return(full_output)
}
