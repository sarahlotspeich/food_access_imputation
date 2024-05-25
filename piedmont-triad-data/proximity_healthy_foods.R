################################################################################
# SETUP ########################################################################
################################################################################
## Load libraries
library(dplyr) ## to wrangle data
library(geosphere) ## to compute Haversine distances
library(ggmap) ## to compute Google Maps driving distances

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("food_access_imputation/set_api_keys.R")

# Source function to get map-aligned census tract population centers
source("food_access_imputation/map_align_coord.R")

# Load census tract population centers for Piedmont Triad from the 2010 US Census
## FIPS codes 
piedmont_triad = c(171, 169, 157, 033, 197, 067, 081, 001, 059, 057, 151, 123, 81, 151, 59, 67, 57, 169, 157, 171, 197) 
## Load and subset population center data
neighborhoods = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/nc_ct_pop_centers_2010.csv") |> ## Read in all NC population centers
  filter(COUNTYFP %in% piedmont_triad) |> ## Subset to counties in study area
  mutate(LocationID = paste0(STATEFP, sprintf("%03s", COUNTYFP), sprintf("%06s", TRACTCE))) |> ## Construct LocationID by combining state, county, and tract FIPS codes
  filter(LocationID != 37081980100) ## Exclude census tract with population = 0 that is just PTI Airport
nrow(neighborhoods) ## N = 387 neighborhoods

################################################################################
# CALCULATE PROXIMITY TO HEALTHY STORES ########################################
################################################################################
# Load healthy foods stores for Piedmont Triad and the surrounding counties from the historic SNAP retailers data
grocery = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/healthy_foods_stores_2022.csv")
nrow(grocery) ## M = 701 healthy foods stores

# Create data frame to fill with food access variables
food_access = neighborhoods |> 
  select(LocationID, LONGITUDE, LATITUDE) |> 
  mutate(dist_closest_straight = NA, ## Straight-line distance to nearest healthy foods store
         dist_closest_map = NA, ## Map-based distance to nearest healthy foods store
         time_closest_map = NA, ## Map-based driving time to nearest healthy foods store
         comp_time_straight = NA, ## Total computing time for all straight-line calculations 
         dist_straight_q20 = NA, ## 20th percentile of all straight-line distances to healthy food stores
         comp_time_map = NA ## Total computing time for all map-based calculations 
         )

# Loop over neighborhoods
for (i in 1:nrow(food_access)) {
  ## Get map-aligned neighborhood centroid (lat/long --> address --> lat/long)
  temp = map_align_coord(longitude = neighborhoods$LONGITUDE[i], 
                         latitude = neighborhoods$LATITUDE[i])
  coord = temp$long_lat ### extract lat/long coordinates for straight-line distances
  address = temp$address ### extract address for map-based distances
  
  ## If map-aligned centroids can't be found, skip and manually review. 
  if (!any(is.na(temp))) {
    ## Calculate straight-line distances between neighborhood and all stores 
    t = system.time(
      straight_dist <- distHaversine(p1 = coord, 
                                     p2 = grocery[, c("Longitude", "Latitude")], 
                                     r = 3958.8)
    )
    food_access$comp_time_straight[i] = as.numeric(t["elapsed"])

    ## Save minimum straight-line distance to grocery store
    food_access$dist_closest_straight[i] = min(straight_dist)
    
    ## Subset grocery stores to only those within 10 straight-line miles 
    straight_dist_q20 = as.numeric(quantile(x = straight_dist, 
                                            probs = 0.2)) ### Compute 20th percentile straight-line distance
    food_access$dist_straight_q20[i] = straight_dist_q20 ### Save 20th percentile straight-line distance
    I_within_q20 = straight_dist <= straight_dist_q20 ### TRUE/FALSE indicator of distance <= 20th percentile
    grocery_within_q20 = grocery[I_within_q20, "Address"] 
 
    ## Calculate map-based distances between neighborhood and stores within 10 miles
    t = system.time(
      map_dist <- mapdist(from = grocery_within_q20,
                          to = rep(x = address, 
                                   times = length(grocery_within_q20)))
    )
    food_access$comp_time_map[i] = as.numeric(t["elapsed"])

    ## Save minimum map-based distance to grocery store
    ## ** do not save which store **
    food_access$dist_closest_map[i] = min(map_dist$miles)
    
    ## Save minimum map-based travel time (driving) to grocery store
    ## ** do not save which store **
    food_access$time_closest_map[i] = min(map_dist$minutes)
    
    ## Print message (to track progress)
    print(paste("Tract", i, "complete."))
    
    ## Save data
    food_access |> 
      write.csv(file = "food_access_imputation/piedmont-triad-data/proximity_healthy_foods.csv", 
                row.names = FALSE)
  }
}
