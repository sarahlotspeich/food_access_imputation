# Load library
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("set_api_keys.R")

# Read in final grocery store data for analyses
snap_dat = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/healthy_foods_stores_2022.csv")

# Load map data for census tracts
## All counties 
counties = get_acs(state = "NC",
                   geography = "county",
                   county = unique(snap_dat$County),
                   variables = "B19013_001",
                   geometry = TRUE,
                   year = 2015)

## Example census tract
example_tract = get_acs(state = "NC",
                        geography = "tract",
                        county = "forsyth",
                        variables = "B19013_001",
                        geometry = TRUE,
                        year = 2015) |> 
  dplyr::filter(GEOID == 37067002601)

tract_center = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/forsyth-data/nc_ct_pop_centers_2010.csv") |> ## Read in all NC population centers
  dplyr::mutate(LocationID = paste0(STATEFP, sprintf("%03s", COUNTYFP), sprintf("%06s", TRACTCE))) |> ## Construct LocationID by combining state, county, and tract FIPS codes
  dplyr::filter(LocationID == 37067002601)

straight_dist = geosphere::distHaversine(p1 = tract_center[, c("LONGITUDE", "LATITUDE")], 
                                         p2 = snap_dat[, c("Longitude", "Latitude")], 
                                         r = 3958.8)

quantile(x = straight_dist, probs = 0.2) ## 18.699 miles 
snap_dat$query = straight_dist <= quantile(x = straight_dist, probs = 0.2)
snap_dat = snap_dat |> 
  dplyr::mutate(query = factor(x = query, 
                               levels = c(FALSE, TRUE), 
                               labels = c("Unqueried", 
                                          "Queried")))

# Make map (faceted by store type)
counties |> 
  ggplot() + 
  geom_sf(aes(geometry = geometry), 
          fill = "white") +
  geom_point(data = snap_dat, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = query), 
             size = 3, 
             alpha = 0.7) +
  geom_point(data = tract_center, 
             aes(x = LONGITUDE, 
                 y = LATITUDE), 
             color = "black", 
             shape = 17, 
             size = 3) +
  scale_color_manual(values = c("#6F98DF", 
                                "#EB7B51")) + 
  theme_void() + 
  theme(plot.margin = margin(l=20, r=20, t=20, b=25), 
        legend.position = "top")

## Save map 
ggsave(filename = "figures/figS_map_query_per_tract.png", 
       device = "png", 
       width = 8, 
       height = 8, 
       units = "in")
