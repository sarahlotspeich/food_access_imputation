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
                               levels = c(TRUE, FALSE), 
                               labels = c("Metropolitan Tracts", 
                                          "Non-Metropolitan Tracts"))) |> 
  ggplot(aes(x = Xstar, 
             y = X)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(aes(col = "Fitted Line"), 
              method = "lm", 
              se = FALSE, 
              size = 1, 
              fullrange = TRUE, 
              alpha = 0.5) + 
  geom_abline(aes(slope = 1, 
                  intercept = 0, 
                  col = "Line of Equality"), 
              size = 1, 
              linetype = 2, 
              key_glyph = "path") + 
  scale_color_manual(values = c("Fitted Line" = '#FD7446',  
                                "Line of Equality" = '#709AE1'), 
                     name = "") + 
  theme_minimal(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25), 
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white"), 
        legend.box = "vertical", 
        legend.position = "top", 
        legend.key.width = unit(2, "line")) + 
  labs(x = "Straight-Line Proximity to Healthy Foods (X*)",
       y = "Map-Based Proximity to Healthy Foods (X)") + 
  xlim(c(0, 16)) +
  ylim(c(0, 16)) +
  coord_equal() + 
  facet_grid(cols = vars(Dataset), 
             rows = vars(METRO))

ggsave(filename = "figures/figS11_scatterplot_proximity_piedmont_metro.png", 
       device = "png", 
       width = 6, 
       height = 6, 
       units = "in")
ggsave(filename = "figures/figS11_scatterplot_proximity_piedmont_metro.pdf", 
       device = "pdf", 
       width = 6, 
       height = 6, 
       units = "in")
