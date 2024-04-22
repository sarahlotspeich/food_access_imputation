############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps

############################################################################################
## LOAD FOOD ACCESS DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////
############################################################################################
## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/analysis_data.csv")
nrow(food_access) ## N = 387 neighborhoods (exclude the tract with population = 0)

############################################################################################
## MAKE SCATTER PLOT OF STRAIGHT-LINE VS MAP-BASED PROXIMITY ///////////////////////////////
############################################################################################
food_access |> 
  ggplot(aes(x = dist_closest_straight, y = dist_closest_map)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              col = '#FD7446', 
              size = 1, 
              fullrange = TRUE) + 
  geom_abline(slope = 1, 
              intercept = 0, 
              col = '#709AE1', 
              size = 1, 
              linetype = 2) + 
  theme_minimal(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25)) + 
  labs(x = "Straight-Line Proximity to Healthy Foods (X*)",
       y = "Map-Based Proximity to Healthy Foods (X)") + 
  xlim(c(0, 16)) +
  ylim(c(0, 16)) +
  coord_equal()

ggsave(filename = "~/Documents/food/figures/fig6_scatterplot_proximity_piedmont.png", 
       device = "png", 
       width = 5, 
       height = 5, 
       units = "in")
