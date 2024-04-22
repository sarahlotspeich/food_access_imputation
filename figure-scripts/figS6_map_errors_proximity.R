############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("set_api_keys.R")

############################################################################################
## LOAD MAP DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////////////
## To align with CDC Places dataset, use 2015 census geometry. /////////////////////////////
############################################################################################
# Census tract outlines
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")
piedmont_triad_ct = get_acs(state = "NC",
                            geography = "tract",
                            county = piedmont_triad,
                            variables = "B01003_001", ## total population,
                            geometry = TRUE,
                            year = 2015) |>
  dplyr::mutate(GEOID = as.numeric(GEOID)) ### character --> numeric to merge into food access data
nrow(piedmont_triad_ct) ## N = 388 neighborhoods

## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/analysis_data.csv")
nrow(food_access) ## N = 387 neighborhoods (exclude the tract with population = 0)

## Merge food_access with imputed data and geometry
map_data = food_access |> 
  dplyr::mutate(error = dist_closest_map - dist_closest_straight) |> 
  dplyr::select(LocationID, 
                dist_closest_straight, 
                dist_closest_map, 
                error) |> 
  dplyr::right_join(piedmont_triad_ct, 
                   by = dplyr::join_by(LocationID == GEOID)) |> 
  dplyr::select(-variable, -estimate, -moe)

############################################################################################
## MAKE MAP OF OBSERVED/PREDICTED PROXIMITY ////////////////////////////////////////////////
############################################################################################
map_data |> 
  ggplot(aes(x = error)) + 
  geom_histogram(fill = '#709AE1') + 
  theme_minimal(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "bottom", 
        strip.text = element_text(face = "bold", hjust = 0.5)) + 
  labs(x = "Magnitude of Error in Straight-Line Proximity (X*)", 
       y = "Number of Census Tracts")

ggsave(filename = "~/Documents/food/figures/figS6_histogram_errors_piedmont.png", 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in")
