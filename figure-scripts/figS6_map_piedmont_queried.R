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
piedmont_triad_count = get_acs(state = "NC",
                            geography = "county",
                            county = piedmont_triad,
                            variables = "B01003_001", ## total population,
                            geometry = TRUE,
                            year = 2015) |>
  dplyr::mutate(GEOID = as.numeric(GEOID)) ### character --> numeric to merge into food access data
nrow(piedmont_triad_count) ## 12 counties

## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/analysis_data.csv")
nrow(food_access) ## N = 387 neighborhoods (exclude the tract with population = 0)

# Merge food access and map data 
map_data = food_access |> 
  dplyr::right_join(piedmont_triad_ct, 
                   by = dplyr::join_by(LocationID == GEOID))

############################################################################################
## MAKE MAP OF OBSERVED/PREDICTED PROXIMITY ////////////////////////////////////////////////
############################################################################################
map_data |> 
  dplyr::mutate(queried = factor(x = !is.na(X_partial), 
                                 levels = c(FALSE, TRUE), 
                                 labels = c("Unqueried", "Queried"))) |> 
  ggplot() + 
  geom_sf(aes(fill = queried, 
              geometry = geometry)) + 
  geom_sf(data = piedmont_triad_count, 
          aes(geometry = geometry), 
          color = "black", 
          fill = NA,
          linewidth = 0.7) + 
  scale_fill_manual(
    values = c('#FD7446','#709AE1', '#FFFFFF'),
    name = "") +
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "bottom") 

ggsave(filename = "~/Documents/food/figures/figS7_map_piedmont_queried.png", 
       device = "png", 
       width = 5, 
       height = 5, 
       units = "in")
ggsave(filename = "~/Documents/food/figures/figS7_map_piedmont_queried.pdf", 
       device = "pdf", 
       width = 5, 
       height = 5, 
       units = "in")
