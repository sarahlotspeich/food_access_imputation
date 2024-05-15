# Load library
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("set_api_keys.R")

# Read in final grocery store data for analyses
snap_dat = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/healthy_foods_stores_2022.csv")

# Load map data for census tracts
## To align with CDC Places dataset, use 2015 census geometry. 
tracts = get_acs(state = "NC",
                 geography = "tract",
                 county = tolower(unique(snap_dat$County)),
                 variables = "B19013_001",
                 geometry = TRUE,
                 year = 2015)

# Create labels for each store type that includes sample size 
store_type_levels = data.frame(table(snap_dat$Store.Type)) |> 
  dplyr::pull(Var1)
store_type_labels = data.frame(table(snap_dat$Store.Type)) |> 
  dplyr::mutate(label = paste0(Var1, "\n(n = ", Freq, ")")) |> 
  dplyr::pull(label)
snap_dat = snap_dat |> 
  dplyr::mutate(Store.Type_with_n = factor(x = Store.Type, 
                                           levels = store_type_levels, 
                                           labels = store_type_labels)) 

# Make map (faceted by store type)
tracts |> 
  ggplot() + 
  geom_sf(aes(geometry = geometry), 
          fill = "white") + 
  geom_point(data = snap_dat, 
             aes(x = Longitude, 
                 y = Latitude), 
             color = "#FD7446", 
             alpha = 0.5) +
  theme_void() + 
  theme(plot.margin = margin(l=20, r=20, t=20, b=25),
        strip.text = element_text(size = 10)) + 
  facet_wrap(~Store.Type_with_n, nrow = 4) 

## Save map 
ggsave(filename = "figures/figS1_map_piedmont_triad_SNAP_long.png", 
       device = "png", 
       width = 8, 
       height = 10, 
       units = "in")
ggsave(filename = "figures/figS1_map_piedmont_triad_SNAP_long.pdf", 
       device = "pdf", 
       width = 8, 
       height = 10, 
       units = "in")

# Make map (faceted by store type)
tracts |> 
  ggplot() + 
  geom_sf(aes(geometry = geometry), 
          fill = "white") + 
  geom_point(data = snap_dat, 
             aes(x = Longitude, 
                 y = Latitude), 
             color = "#FD7446", 
             alpha = 0.5) +
  theme_void() + 
  theme(plot.margin = margin(l=20, r=20, t=20, b=25),
        strip.text = element_text(size = 10)) + 
  facet_wrap(~Store.Type_with_n, nrow = 3) 

## Save map 
ggsave(filename = "figures/figS1_map_piedmont_triad_SNAP_wide.png", 
       device = "png", 
       width = 10, 
       height = 8, 
       units = "in")
ggsave(filename = "figures/figS1_map_piedmont_triad_SNAP_wide.pdf", 
       device = "pdf", 
       width = 10, 
       height = 8, 
       units = "in")
