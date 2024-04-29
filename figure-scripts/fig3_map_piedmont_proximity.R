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

############################################################################################
## GET PREDICTIONS FROM IMPUTATION /////////////////////////////////////////////////////////
############################################################################################
## Deterministic imputation
imp_data = possum::impPossum_data(imputation_formula = X_partial ~ Xstar, 
                                  data = food_access, 
                                  B = 0) |> 
  dplyr::rename(dist_closest_map_imp = X_partial) |> 
  dplyr::select(LocationID, dist_closest_map_imp)

## Merge food_access with imputed data and geometry
map_data = food_access |> 
  dplyr::left_join(imp_data) |> 
  dplyr::select(LocationID, 
                Xstar, 
                X_full, 
                X_partial, 
                dist_closest_map_imp) |> 
  dplyr::right_join(piedmont_triad_ct, 
                   by = dplyr::join_by(LocationID == GEOID)) |> 
  dplyr::select(-variable, -estimate, -moe)

############################################################################################
## MAKE MAP OF OBSERVED/PREDICTED PROXIMITY ////////////////////////////////////////////////
############################################################################################
med_map_based = median(map_data$X_full, na.rm = TRUE) ## 1.514352 miles
med_straight_line = median(map_data$Xstar, na.rm = TRUE) ## 1.006827 miles

## Save as Figure 3
map_data |> 
  tidyr::gather("dist_method", "proximity", -c(1, 6:7)) |> 
  dplyr::mutate(dist_method = factor(x = dist_method, 
                                     levels = c("Xstar", 
                                                "X_full", 
                                                "dist_closest_map_imp", 
                                                "X_partial"), 
                                     labels = c("Naive", 
                                                "Gold Standard", 
                                                "Imputation", 
                                                "Complete Case"))) |> 
  ggplot() + 
  geom_sf(aes(fill = proximity, 
              geometry = geometry)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
    rescaler = ~ scales::rescale_mid(.x, mid = 2), #mid = med_map_based),
    guide = guide_colourbar(direction = "horizontal",
                            barwidth = 8,
                            barheight = 1),
    name = "Proximity to Healthy Foods (in Miles):") +
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "bottom", 
        strip.text = element_text(face = "bold", hjust = 0.5)) + 
  facet_wrap(~ dist_method, nrow = 2, ncol = 2)

ggsave(filename = "~/Documents/food/figures/fig3_map_proximity_piedmont.png", 
       device = "png", 
       width = 5, 
       height = 5, 
       units = "in")

## Save as Figure 3
map_data |> 
  tidyr::gather("dist_method", "proximity", -c(1, 6:7)) |> 
  dplyr::mutate(dist_method = factor(x = dist_method, 
                                     levels = c("Xstar", 
                                                "X_full", 
                                                "dist_closest_map_imp", 
                                                "X_partial"), 
                                     labels = c("Naive", 
                                                "Gold Standard", 
                                                "Imputation", 
                                                "Complete Case"))) |> 
  ggplot() + 
  geom_sf(aes(fill = proximity, 
              geometry = geometry)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
    rescaler = ~ scales::rescale_mid(.x, mid = 2),
    guide = guide_colourbar(direction = "horizontal",
                            barwidth = 8,
                            barheight = 1),
    name = "Proximity to Healthy Foods (in Miles):") +
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "bottom", 
        strip.text = element_text(face = "bold", hjust = 0.5)) + 
  facet_wrap(~ dist_method, nrow = 1, ncol = 4)

ggsave(filename = "~/Documents/food/figures/fig3_map_proximity_piedmont_wide.png", 
       device = "png", 
       width = 10, 
       height = 5, 
       units = "in")
