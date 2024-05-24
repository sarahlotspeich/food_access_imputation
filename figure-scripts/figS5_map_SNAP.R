# Load library
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes
library(dplyr) ## to wrangle data

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("food_access_imputation/set_api_keys.R")

# Read in final grocery store data for analyses
snap_dat = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/healthy_foods_stores_2022.csv")

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
  pull(Var1)
store_type_labels = data.frame(table(snap_dat$Store.Type)) |> 
  mutate(label = paste0(Var1, "\n(n = ", Freq, ")")) |> 
  pull(label)
snap_dat = snap_dat |> 
  mutate(Store.Type_with_n = factor(x = Store.Type, 
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
  facet_wrap(~Store.Type_with_n, nrow = 3) 

## Save map 
ggsave(filename = "figures/figS5_map_piedmont_triad_SNAP_wide.png", 
       device = "png", 
       width = 10, 
       height = 8, 
       units = "in")
ggsave(filename = "figures/figS5_map_piedmont_triad_SNAP_wide.pdf", 
       device = "pdf", 
       width = 10, 
       height = 8, 
       units = "in")
