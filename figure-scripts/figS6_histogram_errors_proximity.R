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
## LOAD FOOD ACCESS DATA FOR PIEDMONT TRIAD ////////////////////////////////////////////////
############################################################################################
## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/analysis_data.csv")
nrow(food_access) ## N = 387 neighborhoods (exclude the tract with population = 0)

## Merge food_access with imputed data and geometry
map_data = food_access |> 
  dplyr::mutate(add_error = X_full - Xstar, 
                mult_error = Xstar / X_full) 

############################################################################################
## MAKE HISTOGRAM OF ADDITIVE ERROR MAGNITUDES /////////////////////////////////////////////
############################################################################################
plot_add_err = map_data |> 
  ggplot(aes(x = add_error)) + 
  geom_histogram(fill = '#709AE1') + 
  theme_minimal(base_size = 10) + 
  labs(x = "Magnitude of Additive Error\nin Straight-Line Proximity", 
       y = "Number of Census Tracts")
ggsave(plot = plot_add_err, 
       filename = "~/Documents/food/figures/figS6a_histogram_add_errors_piedmont.png", 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in")

############################################################################################
## MAKE HISTOGRAM OF MULTIPLICATIVE ERROR MAGNITUDES ///////////////////////////////////////
############################################################################################
plot_mult_err = map_data |> 
  ggplot(aes(x = mult_error)) + 
  geom_histogram(fill = "#BACBEE") + 
  theme_minimal(base_size = 10) + 
  labs(x = "Magnitude of Multiplicative Error\nin Straight-Line Proximity", 
       y = "Number of Census Tracts")
ggsave(plot = plot_mult_err, 
       filename = "~/Documents/food/figures/figS6b_histogram_mult_errors_piedmont.png", 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in")

############################################################################################
## COMBINE PLOTS ///////////////////////////////////////////////////////////////////////////
############################################################################################
ggpubr::ggarrange(plot_add_err, plot_mult_err, nrow = 1)
ggsave(filename = "~/Documents/food/figures/figS6_histogram_errors_piedmont.png", 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in")
