library(dplyr)
library(tidyr)
library(ipfp)
library(mipfp)
library(reshape2)
library(readr)

source("R/functions/fit_od_flows.R")
source("R/functions/integerise_values.R")

fpath <- list(mye_coc = "data/intermediate/mye_coc.rds",
              modelled_od_flows = "data/processed/modelled_od_flows.rds",
              modelled_od_flows_csv = "data/processed/modelled_od_flows.csv",
              unintegerised_flows = "data/processed/unintegerised_flows.rds",
              od_series = "data/intermediate/od_series.rds",
              saved_seed = "data/intermediate/saved_seed.rds"
)

yrs_past_od <- c(2014, 2022) # start/end years of period past detailed data used for starting distribution
yr_to_model <- 2023 # the year we are fitting for

# fitted values likely to include large number of small fractional flows
# we often convert results of IPF into integers, but with OD data this can change totals too much
# not integerising at all makes file very big
# compromise here is to allow limited range of fractional values
# x/min_fraction - i.e. setting min_fraction of 10 allows values to be multiples of 1/10 = 0.1

min_fraction <- 5

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
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(age = as.character(age)) %>%
  complete(gss_out, gss_in, age, sex, fill = list(value = 0)) %>%
  arrange(sex, age, gss_in, gss_out)

saveRDS(seed_df, fpath$saved_seed)

#### B. ## Read previously saved seed

#seed_df <- readRDS(fpath$saved_seed)

#######

# prepare target constraints #

#target marginal flows
tgt_inflow_df <- target_flows %>%
  bind_rows(modelled_S_NI_flows) %>%
  filter(component %in% c("internal_in")) %>%
  rename(gss_in = gss_code) %>%
  select(-c(year, component, gss_name)) %>%
  mutate(value = as.integer(value)) %>%
  mutate(age = as.character(age)) %>%
  arrange(sex, age, gss_in)

tgt_outflow_df <- target_flows %>%
  bind_rows(modelled_S_NI_flows) %>%
  filter(component %in% c("internal_out")) %>%
  rename(gss_out = gss_code) %>%
  select(-c(year, component, gss_name)) %>%
  mutate(value = as.integer(value)) %>%
  mutate(age = as.character(age)) %>%
  arrange(sex, age, gss_out)


full_output <- fit_od_flows(tgt_inflow_df, tgt_outflow_df, seed_df, number_iterations = 150000)



saveRDS(full_output, fpath$unintegerised_flows)

#### prepare and save outputs

# integerise and add year

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
