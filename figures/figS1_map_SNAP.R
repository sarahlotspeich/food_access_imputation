# Load library
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("set_api_keys.R")

# Read in final grocery store data for analyses
forsyth_border_snap = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/forsyth-data/healthy_foods_stores_2022.csv")

# Load map data for Forsyth and bordering counties' census tracts
## To align with CDC Places dataset, use 2015 census geometry. 
border_counties = c("DAVIDSON", "DAVIE",  "FORSYTH", "GUILFORD", "RANDOLPH", "ROCKINGHAM",
                    "STOKES", "SURRY", "YADKIN")
border_border_counties = c("ALLEGHANY", "WILKES", "IREDELL", "ROWAN", "ALAMANCE", "CASWELL")
forsyth_ct = get_acs(state = "NC",
                     geography = "tract",
                     county = tolower(c("forsyth", border_counties, border_border_counties)),
                     variables = "B19013_001",
                     geometry = TRUE,
                     year = 2015)

# Create labels for each store type that includes sample size 
store_type_levels = data.frame(table(forsyth_border_snap$Store.Type)) |> 
  dplyr::pull(Var1)
store_type_labels = data.frame(table(forsyth_border_snap$Store.Type)) |> 
  dplyr::mutate(label = paste0(Var1, "\n(n = ", Freq, ")")) |> 
  dplyr::pull(label)
forsyth_border_snap = forsyth_border_snap |> 
  dplyr::mutate(Store.Type_with_n = factor(x = Store.Type, 
                                           levels = store_type_levels, 
                                           labels = store_type_labels)) 

# Make map (faceted by store type)
forsyth_ct |> 
  ggplot() + 
  geom_sf(aes(geometry = geometry), 
          fill = "black") + 
  geom_point(data = forsyth_border_snap, 
             aes(x = Longitude, 
                 y = Latitude), 
             color = "#721F81FF") +
  theme_void() + 
  theme(plot.margin = margin(l=20, r=20, t=20, b=25),
        strip.text = element_text(size = 10)) + 
  facet_wrap(~Store.Type_with_n, nrow = 3) 

## Save map 
ggsave(filename = "figures/figS1_map_SNAP.png", 
       device = "png", 
       width = 10, 
       height = 8, 
       units = "in")
