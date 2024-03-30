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
imp_data = possum::impPossum_data(imputation_formula = dist_closest_map_cc ~ dist_closest_straight, 
                                  data = food_access, 
                                  B = 0) |> 
  dplyr::rename(dist_closest_map_imp = dist_closest_map_cc) |> 
  dplyr::select(LocationID, dist_closest_map_imp)

## Merge food_access with imputed data and geometry
map_data = food_access |> 
  dplyr::left_join(imp_data) |> 
  dplyr::select(LocationID, 
                dist_closest_straight, 
                dist_closest_map, 
                dist_closest_map_cc, 
                dist_closest_map_imp) |> 
  dplyr::left_join(piedmont_triad_ct, 
                   by = dplyr::join_by(LocationID == GEOID)) |> 
  dplyr::select(-variable, -estimate, -moe)

############################################################################################
## MAKE MAP OF OBSERVED/PREDICTED PROXIMITY ////////////////////////////////////////////////
############################################################################################
med_map_based = median(map_data$dist_closest_map) ## 1.514352 miles

### Gold Standard
fig3a = piedmont_triad_ct |> 
  ggplot() + 
  geom_sf(aes(fill = dist_closest_straight, 
              geometry = geometry)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
    rescaler = ~ scales::rescale_mid(.x, mid = 5),
    guide = guide_colourbar(direction = "horizontal",
                            barwidth = 8,
                            barheight = 1),
    name = "Proximity to Healthy Foods (in Miles):") +
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "bottom", 
        plot.title = element_text(face = "bold", hjust = 0.5)) + 
  ggtitle(label = "Naive")
### Gold Standard
fig3b = piedmont_triad_ct |> 
  ggplot() + 
  geom_sf(aes(fill = dist_closest_map, 
              geometry = geometry)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
    rescaler = ~ scales::rescale_mid(.x, mid = 5),
    guide = guide_colourbar(direction = "horizontal",
                            barwidth = 8,
                            barheight = 1),
    name = "Proximity to Healthy Foods (in Miles):") +
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "bottom", 
        plot.title = element_text(face = "bold", hjust = 0.5)) + 
  ggtitle(label = "Gold Standard")
### Complete Case
fig3c = piedmont_triad_ct |> 
  ggplot() + 
  geom_sf(aes(fill = dist_closest_map_cc, 
              geometry = geometry)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
    rescaler = ~ scales::rescale_mid(.x, mid = 5),
    guide = guide_colourbar(direction = "horizontal",
                            barwidth = 8,
                            barheight = 1),
    name = "Proximity to Healthy Foods (in Miles):") +
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "bottom", 
        plot.title = element_text(face = "bold", hjust = 0.5)) + 
  ggtitle(label = "Complete Case")
### Imputation
fig3d = piedmont_triad_ct |> 
  ggplot() + 
  geom_sf(aes(fill = dist_closest_imp, 
              geometry = geometry)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
    rescaler = ~ scales::rescale_mid(.x, mid = 5),
    guide = guide_colourbar(direction = "horizontal",
                            barwidth = 8,
                            barheight = 1),
    name = "Proximity to Healthy Foods (in Miles):") +
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "bottom", 
        plot.title = element_text(face = "bold", hjust = 0.5)) + 
  ggtitle(label = "Imputation")
ggpubr::ggarrange(fig3a, fig3b, fig3c, fig3d, 
                  common.legend = TRUE, 
                  legend = "bottom")
ggsave(filename = "~/Downloads/fig3_map_proximity_piedmont.png", 
       device = "png", 
       width = 5, 
       height = 5, 
       units = "in")
