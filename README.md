
<!-- README.md is generated from README.Rmd. Please edit that file -->

# model detailed migration flows

This repository contains the code used by the GLA to model detailed 2021
domestic migration flows by origin and destination for use in the
2021-based interim population projections.

- The 2021-based interim projections are available from the London
  Datastore:
  <https://data.london.gov.uk/demography/population-and-household-projections/>
- Modelled input data, including the detailed migration flows, are
  published here:
  <https://data.london.gov.uk/dataset/modelled-population-backseries>

[Annual origin-destination
flows](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset)
for internal moves between local authority districts by age and sex are
usually published by ONS alongside their [Mid-Year Population
Estimates](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland)
(MYE). This data is a key input to the GLA’s population projection
models. At the time of production (January 2023), ONS had not published
detailed flow data for the year ending mid-2021. However, it had
published estimates of gross internal flows to and from each local
authority district in England and Wales as part of the ‘data for
reconciliation’ that accompanied the published 2021 MYE. We used this
data in combination with detailed flow data from previous years to
create modelled OD flows for 2021 using iterative proportional fitting
(IPF).

The problem lends itself well to an IPF approach. The main complication
in implementation is the lack of flow data for Scotland and Northern
Ireland in the reconciliation data (the published detailed flow data
includes the countries of Scotland and Northern Ireland within the flow
matrix with English and Welsh local authority districts).

An additional step is necessary to fit gross in- and out-flows for both
Scotland and Northern Ireland before fitting the origin-destination
flows. This is done by:

- inferring the combined S&NI net balance with E&W from the difference
  in the sum of inflows and outflows for all districts in the
  reconciliation data
- calculating typical in- and out-flows for S&NI (combined) from past
  detailed OD data
- use these typical flows as a basis for fitting modelled gross flows
  for S&NI consistent with the calculated net
- splitting the combined in- and out-flows into separate flows for
  Scotland and Northern Ireland based upon the past relative sizes of
  their gross flows

Outputs from iterative proportional fitting processes can include large
numbers of small fractional values and these are often converted to
integers in a manner that preserves the overall total. For our purposes,
integer outputs would be desirable as it leads to smaller file sizes and
faster run times when used as an input to the population projection
models.

However, in the case of origin-destination data, where values
simultaneously represent flows from one area and to another, simple
approaches to integerising outputs risk introducing significant
distortions into the total flows to or from a given area. The approach
taken here is a compromise between compactness of output and consistency
with the original gross flows. Values are converted to multiples of 0.1
(chosen through trial and error) rather than integers. This allows a
large proportion of small flows to be removed, reducing the size of the
output by a factor of approximately 15, while having a limited impact on
overall gross flows.

An alternative approach - not tested due to the time constraints under
which the work was conducted - would be to allow explicit zero values in
the seed table, rather than allocating combinations not present in the
original data a small nominal value.

## Setup

The inputs to the process are:

- The components of change from the ‘data for reconciliation’ published
  by ONS alongside the 2021 MYE
- Detailed annual internal migration flow estimates for years to
  mid-2020 (originally published by ONS, the file used here has been
  processed by the GLA to consolidate the annual series into a single
  file on a consistent geography)

The script *0_prepare_input_data.R* will download and prepare these
inputs ready for use.

## Use

The script *fit_origin_destination_flows.R* will generate and save
modelled origin destination flows for 2021.

There are several parameters in this script that can be readily adjusted
to modify the output of the process:

- The vector *yrs_past_od* (default *c(2018, 2020)*) defines the period
  of past detailed flow data to use as the basis for the seed
  distribution
- *min_fraction* (default *10*) determines the extent to which
  fractional values in the modelled outputs are consolidated. Lower
  values give fewer small fractional flows and smaller file sizes, at
  the cost of some distortion of the results. A value of 1 gives integer
  outputs.
- *number_iterations* (default *40000*) determines the maximum number of
  iterations for the IPF process. Higher values potentially give a
  better fit at the expense of longer run times.

Further adjustments can be made to the configuration of the IPF
function. More information can be found in the documentation that
accompanies the [MIPFP](https://www.jstatsoft.org/article/view/v086c02)
package.
