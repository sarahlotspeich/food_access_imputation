############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(tidycensus) ## for shapefiles
library(spdep) ## for adjacency matrix

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
# source("set_api_keys.R")

############################################################################################
## LOAD FOOD ACCESS DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////
############################################################################################
## Load the PLACES data for all of NC to get IDs for census tracts with nonzero population
nonzero_pop = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/nc_disease_prevalences_2022.csv")
nrow(nonzero_pop) ## N = 2169 neighborhoods (excluding any tracts with population = 0)

# Load map data (census tracts)
tracts = get_acs(state = "NC", 
                 geography = "tract",
                 variables = "B19013_001",
                 geometry = TRUE, 
                 year = 2015) |> 
  dplyr::filter(GEOID %in% nonzero_pop$TractFIPS)

# Define the adjacency matrix 
neighbors = poly2nb(tracts, 
                    queen = TRUE) ## contiguous neighbors
ncW = nb2mat(neighbors, 
                       style = "B", 
                       zero.policy = TRUE)
rownames(ncW) = colnames(ncW) = nonzero_pop$TractFIPS

# Save it 
ncW |> 
  saveRDS(file = "piedmont-triad-data/nc_adjacency_matrix.Rds")