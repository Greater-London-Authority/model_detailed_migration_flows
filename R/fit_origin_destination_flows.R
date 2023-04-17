library(dplyr)
library(tidyr)
library(ipfp)
library(mipfp)
library(reshape2)

source("R/functions/optimise_gross_flows.R")
source("R/functions/integerise_values.R")

fpath <- list(mye_coc = "data/intermediate/mye_coc.rds",
              modelled_od_flows = "data/processed/modelled_od_flows.rds",
              modelled_od_flows_csv = "data/processed/modelled_od_flows.csv",
              unintegerised_flows = "data/processed/unintegerised_flows.rds",
              od_series = "data/intermediate/od_series.rds",
              saved_seed = "data/intermediate/saved_seed.rds"
              )

yrs_past_od <- c(2018, 2020) # start/end years of period past detailed data used for starting distribution
yr_to_model <- 2021 # the year we are fitting for

# We want to match the domestic gross in- and out-flows from the ONS reconciliation data
target_flows <- readRDS(fpath$mye_coc) %>%
  filter(component %in% c("internal_in", "internal_out")) %>%
  filter(year == yr_to_model)

# Detailed migration estimates from previous years used as basis for new distribution
od_data <- readRDS(fpath$od_series) %>%
  filter(between(year, yrs_past_od[1], yrs_past_od[2]))

##### Create target flows for Scotland and Northern Ireland

# Process is complicated by the lack of flow data for Scotland and Northern Ireland in the reconciliation data
# We will model gross flows for each area by:
#   inferring their combined net balance with E&W from the difference in sum of inflows and outflows for all districts in the reconciliation set
#   calculating typical in- and out-flows for S&NI (combined) from past detailed OD data
#   fitting gross flows for S&NI (combined) consistent with the calculated net
#   splitting these flows between Scotland and Northern Ireland based upon the past relative sizes of their gross flows

# calculate net flow for S & NI (combined)
target_S_NI_net <- readRDS(fpath$mye_coc) %>%
  filter(component %in% c("internal_net")) %>%
  filter(year == yr_to_model) %>%
  group_by(age, sex, year) %>%
  summarise(netflow = sum(-value), .groups = "drop")

# calculate typical in- and out-flows for S&NI (combined)
base_S_NI_inflow <- od_data%>%
  filter(gss_in %in% c("N92000002", "S92000003"))%>%
  group_by(age, sex, year)%>%
  summarise(value = sum(value), .groups = "drop") %>%
  group_by(age, sex)%>%
  summarise(m_in = mean(value), .groups = "drop")

base_S_NI_outflow <- od_data%>%
  filter(gss_out %in% c("N92000002", "S92000003"))%>%
  group_by(age, sex, year)%>%
  summarise(value = sum(value), .groups = "drop") %>%
  group_by(age, sex)%>%
  summarise(m_out = mean(value), .groups = "drop")

# calculate the typical relative sizes of gross flows to and from Scotland and Northern Ireland
split_S_NI_inflow <- od_data%>%
  filter(gss_in %in% c("N92000002", "S92000003"))%>%
  group_by(gss_in, age, sex)%>%
  summarise(value = sum(value), .groups = "drop") %>%
  pivot_wider(names_from = gss_in, values_from = value) %>%
  mutate(total = N92000002 + S92000003,
         N92000002 = N92000002/total,
         S92000003 = S92000003/total) %>%
  select(-total) %>%
  pivot_longer(cols = c("S92000003", "N92000002"),
               values_to = "inflow_prop",
               names_to = "gss_code")

split_S_NI_outflow <- od_data%>%
  filter(gss_out %in% c("N92000002", "S92000003"))%>%
  group_by(gss_out, age, sex)%>%
  summarise(value = sum(value), .groups = "drop") %>%
  pivot_wider(names_from = gss_out, values_from = value) %>%
  mutate(total = N92000002 + S92000003,
         N92000002 = N92000002/total,
         S92000003 = S92000003/total) %>%
  select(-total) %>%
  pivot_longer(cols = c("S92000003", "N92000002"),
               values_to = "outflow_prop",
               names_to = "gss_code")

# Fit gross flows for S&NI (combined) consistent with net
# disaggregate gross flows between Scotland and Northern Ireland
modelled_S_NI_flows <- target_S_NI_net %>%
  left_join(base_S_NI_inflow, by = c("age", "sex")) %>%
  left_join(base_S_NI_outflow, by = c("age", "sex")) %>%
  mutate(model_flows = optimise_gross_flows(m_in, m_out, netflow)) %>%
  unnest_wider(col = model_flows) %>%
  select(-c(m_in, m_out, netflow)) %>%
  left_join(split_S_NI_inflow, by = c("age", "sex")) %>%
  left_join(split_S_NI_outflow, by = c("age", "sex", "gss_code")) %>%
  mutate(internal_in = round(inflow * inflow_prop, 0),
         internal_out = round(outflow * outflow_prop, 0),
         internal_net = internal_in - internal_out) %>%
  select(-c(inflow, outflow, inflow_prop, outflow_prop)) %>%
  pivot_longer(cols = c("internal_in", "internal_out", "internal_net"),
               names_to = "component", values_to = "value")

rm(split_S_NI_outflow, split_S_NI_inflow, base_S_NI_inflow, base_S_NI_outflow, target_S_NI_net)


####### Prepare target and seed data frames ready for fitting process

# prepare seed table #

### comment/uncomment the following statements as necessary ###

#### A. ## Generate and save seed data frame. This is slow - once generated, consider loading from file instead

seed_df <- od_data %>%
  group_by(gss_out, gss_in, sex, age) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  mutate(age = as.character(age)) %>%
  complete(gss_out, gss_in, age, sex, fill = list(value = 0.01)) %>%
  arrange(sex, age, gss_in, gss_out)

saveRDS(seed_df, fpath$saved_seed)

#### B. ## Read previously saved seed

#seed_df <- readRDS(fpath$saved_seed)

#######

# prepare target constraints #
# fitting process is simpler if sums of in and outflows match (by age and sex)

#target marginal flows
tgt_inflow_df <- target_flows %>%
  bind_rows(modelled_S_NI_flows) %>%
  filter(component %in% c("internal_in")) %>%
  rename(gss_in = gss_code) %>%
  select(-c(year, component, gss_name)) %>%
  mutate(value = as.integer(value)) %>%
  mutate(age = as.character(age)) %>%
  arrange(sex, age, gss_in)

tgt_outflow_unadjusted <- target_flows %>%
  bind_rows(modelled_S_NI_flows) %>%
  filter(component %in% c("internal_out")) %>%
  rename(gss_out = gss_code) %>%
  select(-c(year, component, gss_name)) %>%
  mutate(value = as.integer(value)) %>%
  mutate(age = as.character(age)) %>%
  arrange(sex, age, gss_out)

#ensure target totals match adjusting largest value in the outflows
total_inflows <- tgt_inflow_df %>%
  group_by(age, sex) %>%
  summarise(inflow = sum(value), .groups = "drop")

total_outflows <- tgt_outflow_unadjusted %>%
  group_by(age, sex) %>%
  summarise(outflow = sum(value), .groups = "drop")

adjust_la <- tgt_outflow_unadjusted %>%
  group_by(age, sex) %>%
  top_n(1, value) %>%
  ungroup() %>%
  select(-value)

tgt_adjustment <- total_inflows %>%
  left_join(total_outflows, by = c("age", "sex")) %>%
  mutate(value = inflow - outflow) %>%
  select(-inflow, -outflow) %>%
  left_join(adjust_la, by = c("age", "sex"))

tgt_outflow_df <- tgt_outflow_unadjusted %>%
  bind_rows(tgt_adjustment) %>%
  group_by(gss_out, age, sex) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  arrange(sex, age, gss_out)

rm(tgt_outflow_unadjusted, tgt_adjustment, total_inflows, total_outflows, adjust_la)

####### Fit origin-destination flows

number_iterations <- 40000

# formulae used to cast data frames into arrays
tgt_inflow_formula <- "gss_in ~ age ~ sex"
tgt_outflow_formula <- "gss_out ~ age ~ sex"
seed_formula <- "gss_out ~ gss_in ~ age ~ sex"

# flows are fit for single combination of age and sex at a time (this is more efficient that fitting for all at once)
# the process loops over age and sex
# TODO normally we'd prepare a list to store the results and not bind_rows within it [we also might avoid filtering within]

i = 0 #flag for first loop

for(sel_age in unique(seed_df$age)) {

  for(sel_sex in unique(seed_df$sex)) {

    message(format(Sys.time(), "%X"), " - ", sel_age, " - ", sel_sex)

    ### filter target and seed data frames by age/sex and cast into arrays
    sel_tgt_inflow_df <- tgt_inflow_df %>%
      filter(age == sel_age,
             sex == sel_sex)

    sel_tgt_outflow_df <- tgt_outflow_df %>%
      filter(age == sel_age,
             sex == sel_sex)

    target_inflow <- acast(data = sel_tgt_inflow_df,
                           formula = as.formula(tgt_inflow_formula),
                           value.var = "value",
                           fill = 0)

    target_outflow <- acast(data = sel_tgt_outflow_df,
                            formula = as.formula(tgt_outflow_formula),
                            value.var = "value",
                            fill = 0)

    sel_seed_df <- seed_df %>%
      filter(age == sel_age,
             sex == sel_sex)

    seed_tbl <- acast(data = sel_seed_df,
                      formula = as.formula(seed_formula),
                      value.var = "value",
                      fill = 0.01)

    #create list of target data and also list defining relationships to contingency table dimensions
    target_data <- list("outflows" = target_outflow,
                        "inflows" = target_inflow)

    target_dimension_list <- list("outflows" = c(1, 3, 4),
                                  "inflows" = c(2, 3, 4))

    #### fit table and convert output to data frame
    results.ipfp <- Estimate(seed = seed_tbl,
                             target.list = target_dimension_list,
                             target.data = target_data,
                             method = "ipfp",
                             iter = number_iterations)

    output <- melt(results.ipfp$x.hat, value.name = "value") %>%
      mutate_at(vars(-value), funs(as.character)) %>%
      filter(value > 0)

    colnames(output) <- c("gss_out", "gss_in", "age", "sex", "value")

    # combine output from each loop into single data frame
    if(i == 0) {
      full_output <- output
    } else {
      full_output <- bind_rows(full_output, output)
    }

    i = 1
  }
}

saveRDS(full_output, fpath$unintegerised_flows)

#### prepare and save outputs

# fitted values likely to include large number of small fractional flows
# we often convert results of IPF into integers, but with OD data this can change totals too much
# not integerising at all makes file very big
# compromise here is to allow limited range of fractional values
# x/min_fraction - i.e. setting min_fraction of 10 allows values to be multiples of 1/10 = 0.1

# integerise and add year
min_fraction <- 10

modelled_od_flows <- full_output %>%

  group_by(age, sex) %>%
  mutate(value = min_fraction * value) %>%
  mutate(value = integerise_values(value)) %>%
  mutate(value = value/min_fraction) %>%
  ungroup() %>%
  filter(value > 0) %>%
  mutate(year = yr_to_model,
         age = as.integer(age))

saveRDS(modelled_od_flows, fpath$modelled_od_flows)

write_csv(modelled_od_flows, fpath$modelled_od_flows_csv)
