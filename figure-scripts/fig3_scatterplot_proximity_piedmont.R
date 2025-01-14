############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps

############################################################################################
## LOAD FOOD ACCESS DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////
############################################################################################
## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/analysis_data.csv")
nrow(food_access) ## N = 387 neighborhoods (exclude the tract with population = 0)

############################################################################################
## MAKE LONG VERSION OF THE DATA TO PLOT ///////////////////////////////////////////////////
############################################################################################
plot_dat = food_access |> 
  dplyr::select(GEOID, CountyName, METRO, Xstar, X_full, X_partial) |> 
  tidyr::gather(key = "Dataset", value = "X", -c(1:4)) |> 
  dplyr::mutate(Dataset = factor(x = Dataset, 
                                 levels = c("X_full", "X_partial"), 
                                 labels = c("Fully Queried", "Partially Queried")))

############################################################################################
## MAKE SCATTER PLOT OF STRAIGHT-LINE VS MAP-BASED PROXIMITY ///////////////////////////////
############################################################################################
plot_dat |> 
  dplyr::mutate(METRO = factor(x = METRO, 
                               levels = c(TRUE, FALSE))) |> 
  ggplot(aes(x = Xstar, 
             y = X, 
             color = METRO)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              #col = '#FD7446', 
              size = 1, 
              fullrange = TRUE, 
              alpha = 0.5) + 
  geom_abline(slope = 1, 
              intercept = 0, 
              col = "grey", # col = '#709AE1', 
              size = 1, 
              linetype = 2) + 
  theme_minimal(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25), 
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white"), 
        legend.box = "vertical", 
        legend.position = "top") + 
  scale_color_manual(name = "Metropolitan Area:", 
                     values = c("#6F98DF", "#EB7B51")) + 
  labs(x = "Straight-Line Proximity to Healthy Foods (X*)",
       y = "Map-Based Proximity to Healthy Foods (X)") + 
  xlim(c(0, 16)) +
  ylim(c(0, 16)) +
  coord_equal() + 
  facet_wrap(~Dataset)

# //////////////////////////////////////////////////////////////////////
# Save as 10" wide x 5" tall ///////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
ggsave(filename = "figures/fig3_scatterplot_proximity_piedmont.png", 
       device = "png", 
       width = 10, 
       height = 5, 
       units = "in")
ggsave(filename = "figures/fig3_scatterplot_proximity_piedmont.pdf", 
       device = "pdf", 
       width = 10, 
       height = 5, 
       units = "in")
