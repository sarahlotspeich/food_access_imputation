############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(tidycensus) ## for shapefiles
library(possum) ## for imputation 
library(spdep) ## for adjacency matrix
library(spaMM) ## for spatial mixed-effects model

############################################################################################
## LOAD FOOD ACCESS DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////
############################################################################################
## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/analysis_data.csv")
nrow(food_access) ## N = 387 neighborhoods (exclude the tract with population = 0)

## Source a script to build adjacency matrix for census tracts in the Piedmont Triad as 
### --> ptW
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/piedmont_adjacency_matrix.R?raw=TRUE")

## Source a function to extract model coefficients from spaMM
### --> get_sp_mod_summ()
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/get_sp_mod_summ.R?raw=TRUE")

## Naive Analysis 
### --> naive_res
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/naive_analysis.R?raw=TRUE")

## Gold Standard Analysis 
### --> gs_res
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/gold_standard_analysis.R?raw=TRUE")

## Complete Case Analysis 
### --> cc_res
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/complete_case_analysis.R?raw=TRUE")

## Imputation Analysis 
### --> imp_res
devtools::source_url("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/imputation_analysis.R?raw=TRUE")

## Combine All 
all_res = naive_res |> 
  dplyr::bind_rows(gs_res) |> 
  dplyr::bind_rows(cc_res) |> 
  dplyr::bind_rows(imp_res)

### And save 
all_res |> 
  write.csv("~/Documents/food_access_imputation/piedmont-triad-data/forest_plot_data.csv", 
            row.names = FALSE)
