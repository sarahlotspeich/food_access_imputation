############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load library
library(tidycensus) ## to get ACS data

## Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("set_api_keys.R")

############################################################################################
## DEFINE PIEDMONT TRIAD COUNTY NAMES //////////////////////////////////////////////////////
## To align with CDC Places dataset, use 2015 census geometry. /////////////////////////////
############################################################################################
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")

############################################################################################
## MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS ///////////////////////////////////////////
############################################################################################
income = get_acs(state = "NC",
                 geography = "tract",
                 county = piedmont_triad,
                 variables = "B19113_001", ## median family income (past 12 mo) 
                 geometry = FALSE,
                 year = 2015) |> 
  dplyr::rename(INCOME = estimate) |> 
  dplyr::select(GEOID, NAME, INCOME) 

############################################################################################
## % POPULATION WITH INCOME BELOW POVERTY LEVEL IN THE PAST 12 MONTHS //////////////////////
############################################################################################
poverty = get_acs(state = "NC",
                  geography = "tract",
                  county = piedmont_triad,
                  variables = c("B17020_001", ## population for whom poverty status is determined
                                "B17020_002"), ## number of people with income in the past 12 months below poverty level
                  geometry = FALSE,
                  year = 2015) |> 
  dplyr::mutate(variable = factor(x = variable, 
                                  levels = c("B17020_002", "B17020_001"), 
                                  labels = c("POVERTY", "POVERTY_TOTAL"))) |> 
  dplyr::select(-moe) |>
  tidyr::spread(key = variable, value = estimate) |> 
  dplyr::mutate(PERC_POVERTY = POVERTY / POVERTY_TOTAL) |> 
  dplyr::select(-POVERTY, -POVERTY_TOTAL)
############################################################################################
## % HOUSEHOLDS RECEIVING FOOD STAMPS/SNAP IN THE PAST 12 MONTHS ///////////////////////////
############################################################################################
snap = get_acs(state = "NC",
               geography = "tract",
               county = piedmont_triad,
               variables = c(SNAP = "B22001_002", ## number of households received food stamps/SNAP (past 12 mo)
                             SNAP_TOTAL = "B22001_001"), ## total number of households
               geometry = FALSE,
               year = 2015) |> 
  dplyr::select(-moe) |>
  tidyr::spread(key = variable, value = estimate) |> 
  dplyr::mutate(PERC_SNAP = SNAP / SNAP_TOTAL) |> 
  dplyr::select(-SNAP, -SNAP_TOTAL)

############################################################################################
## % WORKFORCE (>16 YO) GETTING TO WORK DRIVING ALONE VS PUBLIC TRANSIT ////////////////////
############################################################################################
work_transport = get_acs(state = "NC",
                         geography = "tract",
                         county = piedmont_triad,
                         variables = c("B08006_001", ## total for transportation to work variables
                                       "B08006_003", ## drive alone in car, truck, or van to work 
                                       "B08006_008" ## public transportation (excluding taxicab) to work 
                         ),
                         geometry = FALSE,
                         year = 2015) |> 
  dplyr::mutate(variable = factor(x = variable, 
                                  levels = c("B08006_003", "B08006_008", "B08006_001"), 
                                  labels = c("CAR", "PUBLIC_TRANSIT", "TRANSPORT_TOTAL"))) |> 
  dplyr::select(-moe) |>
  tidyr::spread(key = variable, value = estimate) |> 
  dplyr::mutate(PERC_CAR = CAR / TRANSPORT_TOTAL, 
                PERC_PUBLIC_TRANSIT = PUBLIC_TRANSIT / TRANSPORT_TOTAL) |> 
  dplyr::select(GEOID, NAME, PERC_CAR, PERC_PUBLIC_TRANSIT) 

############################################################################################
## % CIVILIAN NONINSTITUTIONALIZED POPULATION WITH HEALTH INSURANCE COVERAGE ///////////////
## These data are available by sex x age group, ////////////////////////////////////////////
## so they have to be aggregated by census tract ///////////////////////////////////////////
############################################################################################
insured = get_acs(state = "NC",
                       geography = "tract",
                       county = piedmont_triad,
                       variables = c("B27001_004", "B27001_007", "B27001_010", 
                                     "B27001_013", "B27001_016", "B27001_019", 
                                     "B27001_022", "B27001_025", "B27001_028", 
                                     "B27001_032", "B27001_035", "B27001_038", 
                                     "B27001_041", "B27001_044", "B27001_047", 
                                     "B27001_050", "B27001_053", "B27001_056"),
                       geometry = FALSE,
                       year = 2015) |> 
  dplyr::group_by(GEOID, NAME) |> 
  dplyr::summarize(INSURED = sum(estimate)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(
    get_acs(state = "NC",
            geography = "tract",
            county = piedmont_triad,
            variables = "B27001_001",
            geometry = FALSE,
            year = 2015) |> 
      dplyr::rename(INSURED_TOTAL = estimate) |> 
      dplyr::select(GEOID, NAME, INSURED_TOTAL) 
  ) |> 
  dplyr::mutate(PERC_INSURED = INSURED / INSURED_TOTAL) |> 
  dplyr::select(GEOID, NAME, PERC_INSURED) 
############################################################################################
## % POPULATION (>= 25 YO) COMPLETING AT LEAST SOME COLLEGE ////////////////////////////////
############################################################################################
college = get_acs(state = "NC",
                  geography = "tract",
                  county = piedmont_triad,
                  variables = c("B06009_004", ## Some college or associate's degree
                                "B06009_005", ## Bachelor's degree
                                "B06009_006" ## Graduate or professional degree
                                ),
                  geometry = FALSE,
                  year = 2015) |> 
  dplyr::group_by(GEOID, NAME) |> 
  dplyr::summarize(COLLEGE = sum(estimate)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(
    get_acs(state = "NC",
            geography = "tract",
            county = piedmont_triad,
            variables = "B06009_001",
            geometry = FALSE,
            year = 2015) |> 
      dplyr::rename(EDUCATION_TOTAL = estimate) |> 
      dplyr::select(GEOID, NAME, EDUCATION_TOTAL) 
  ) |> 
  dplyr::mutate(PERC_COLLEGE = COLLEGE / EDUCATION_TOTAL) |> 
  dplyr::select(GEOID, NAME, PERC_COLLEGE) 

############################################################################################
## % CHILDREN (< 18 YO) IN FEMALE-HEADED HOUSEHOLDS (NO SPOUSE) ////////////////////////////
############################################################################################
fem_house = get_acs(state = "NC",
                    geography = "tract",
                    county = piedmont_triad,
                    variables = c(FEM_HEAD = "B09005_005", ## children in female-headed household, no husband present, family
                                  FEM_HEAD_TOTAL = "B09005_001"), ## total number of children
                    geometry = FALSE,
                    year = 2015) |> 
  dplyr::select(-moe) |>
  tidyr::spread(key = variable, value = estimate) |> 
  dplyr::mutate(PERC_FEM_HEAD = FEM_HEAD / FEM_HEAD_TOTAL) |> 
  dplyr::select(-FEM_HEAD, -FEM_HEAD_TOTAL)

############################################################################################
## MERGE AND SAVE //////////////////////////////////////////////////////////////////////////
############################################################################################
piedmont_triad_acs_data = income |> 
  dplyr::left_join(poverty) |> 
  dplyr::left_join(snap) |> 
  dplyr::left_join(work_transport) |> 
  dplyr::left_join(insured) |> 
  dplyr::left_join(college) |> 
  dplyr::left_join(fem_house)
piedmont_triad_acs_data |> 
  write.csv("~/Documents/food/forsyth-data/piedmont_triad_acs_data.csv", 
            row.names = FALSE)