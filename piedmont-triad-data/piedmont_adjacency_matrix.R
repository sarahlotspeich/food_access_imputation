############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(tidycensus) ## for shapefiles
library(spdep) ## for adjacency matrix

############################################################################################
## LOAD FOOD ACCESS DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////
############################################################################################
## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/analysis_data.csv")
nrow(food_access) ## N = 387 neighborhoods (exclude the tract with population = 0)

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("set_api_keys.R")

# Define Piedmont Triad counties
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")

# Load map data (census tracts)
tracts = get_acs(state = "NC", 
                 geography = "tract", 
                 county = piedmont_triad,
                 variables = "B19013_001",
                 geometry = TRUE, 
                 year = 2015) |> 
  dplyr::filter(GEOID %in% food_access$LocationID)

# Load map data (counties)
counties = get_acs(state = "NC", 
                   geography = "county", 
                   county = piedmont_triad,
                   variables = "B19013_001",
                   geometry = TRUE, 
                   year = 2015) |> 
  dplyr::mutate(NAME = sub(pattern = " County, North Carolina", 
                           replacement = "", 
                           x = NAME))

# Define the adjacency matrix 
neighbors = poly2nb(tracts, 
                    queen = TRUE) ## contiguous neighbors
adj_matrix = nb2mat(neighbors, 
                    style = "B")
rownames(adj_matrix) = colnames(adj_matrix) = food_access$LocationID