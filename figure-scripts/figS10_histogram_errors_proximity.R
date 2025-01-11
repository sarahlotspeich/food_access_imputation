############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes
library(dplyr) ## to wrangle data
library(ggpubr) ## to combine plots

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("food_access_imputation/set_api_keys.R")

############################################################################################
## LOAD FOOD ACCESS DATA FOR PIEDMONT TRIAD ////////////////////////////////////////////////
############################################################################################
## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/analysis_data.csv")
nrow(food_access) ## N = 387 neighborhoods (exclude the tract with population = 0)

## Merge food_access with imputed data and geometry
map_data = food_access |> 
  mutate(add_error = Xstar - X_full, 
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

############################################################################################
## MAKE HISTOGRAM OF MULTIPLICATIVE ERROR MAGNITUDES ///////////////////////////////////////
############################################################################################
plot_mult_err = map_data |> 
  ggplot(aes(x = mult_error)) + 
  geom_histogram(fill = "#BACBEE") + 
  theme_minimal(base_size = 10) + 
  labs(x = "Magnitude of Multiplicative Error\nin Straight-Line Proximity", 
       y = "Number of Census Tracts")

############################################################################################
## COMBINE PLOTS ///////////////////////////////////////////////////////////////////////////
############################################################################################
ggarrange(plot_add_err, plot_mult_err, nrow = 1)
ggsave(filename = "figures/figS10_histogram_errors_piedmont.png", 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in")
