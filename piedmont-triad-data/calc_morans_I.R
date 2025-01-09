# This code was heavily (and gratefully) adapted from the link below
# REF: https://mgimond.github.io/simple_moransI_example/

# Load packages 
library(tidycensus) ## for shapefiles
library(spdep) ## for adjacency matrix
library(dplyr) ## for data wrangling

# Load data from GitHub
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/analysis_data.csv")

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
  filter(GEOID %in% food_access$GEOID)

# Define neighbors / adjacency matrix from the tract shapes 
neighbors = poly2nb(tracts, 
                    queen = TRUE) ## contiguous neighbors
adj_matrix = nb2mat(neighbors, 
                    style = "B")
rownames(adj_matrix) = colnames(adj_matrix) = food_access$GEOID

# Naive Analysis 
## Outcome Y = Diagnosed diabetes among adults aged >=18 years 
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_diab = glm(formula = Y_DIABETES ~ METRO * Xstar, 
               family = poisson(link = "log"), 
               offset = log(O_POP),
               data = food_access)

## Outcome Y = Obesity among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_obes = glm(formula = Y_OBESITY ~ METRO * Xstar,
               family = poisson(link = "log"), 
               offset = log(O_POP),
               data = food_access)

# Compute residuals 
food_access$RESID_DIABETES_PRED = exp(predict(object = mod_diab)) - food_access$Y_DIABETES
food_access$RESID_OBESITY_PRED = exp(predict(object = mod_obes)) - food_access$Y_OBESITY

# Calculate Moran's I on the residuals
## Assign weights to neighboring tracts 
lw = nb2listw(neighbours = neighbors, 
              style = "W", 
              zero.policy = TRUE)

## Moran's I and test for diagnosed diabetes
moran.test(x = food_access$RESID_DIABETES_PRED, 
           listw = lw, 
           alternative = "greater")
# Moran I statistic       Expectation          Variance 
# 0.0568403495            -0.0025906736      0.0008747714 

## Moran's I and test for obesity
moran.test(x = food_access$RESID_OBESITY_PRED, 
           listw = lw, 
           alternative = "greater")
# Moran I statistic       Expectation          Variance 
# 0.0538842833            -0.0025906736      0.0008752417 
