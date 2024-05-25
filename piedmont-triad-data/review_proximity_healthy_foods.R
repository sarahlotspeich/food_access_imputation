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

################################################################################
# HEALTHY STORE LOCATIONS ######################################################
################################################################################
# Load healthy foods stores for Piedmont Triad and the surrounding counties from the historic SNAP retailers data
grocery = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/healthy_foods_stores_2022.csv")
nrow(grocery) ## M = 701 healthy foods stores

# Start with Piedmont Triad food access variables (don't need to re-query all)
food_access = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/")
nrow(food_access) ## N = 387 neighborhoods

# Function to redo all distance calcs based on manually reviewed coordinates (coord)
# and fill in row i in original food_access data
revisit_neighborhood = function(coord, i) {
  ## Get map-aligned neighborhood centroid (lat/long --> address --> lat/long)
  temp = map_align_coord(longitude = coord[1], 
                         latitude = coord[2])
  coord = temp$long_lat ### extract lat/long coordinates for straight-line distances
  address = temp$address ### extract address for map-based distances
  
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
  return(food_access)
}

################################################################################
# MANUAL REVIEW NEIGHBORHOODS WITHOUT MAP-ALIGNED POPULATION CENTERS ###########
################################################################################
## Manual entry of LocationID 37033930100
## Get map-aligned neighborhood centroid (address --> lat/long)
address = "3866 Yarboroughs Mill Rd, Milton, NC 27305" ### address
geo = geocode(location = address) ### geocode address
coord = with(geo, c(lon, lat)) ### vector of long/lat coordinates
## Recalculate distances
food_access = revisit_neighborhood(coord = coord,
                                   i = which(food_access$LocationID == 37033930100))

## Manual entry of LocationID 37123960300
## Get map-aligned neighborhood centroid (address --> lat/long)
address = "1513 Moccasin Creek Rd, Troy, NC 27371" ### address
geo = geocode(location = address) ### geocode address
coord = with(geo, c(lon, lat)) ### vector of long/lat coordinates
## Recalculate distances
food_access = revisit_neighborhood(coord = coord,
                                   i = which(food_access$LocationID == 37123960300))

## Manual entry of LocationID 37123960402
## Get map-aligned neighborhood centroid (address --> lat/long)
address = "118 Hudson Ln, Mt Gilead, NC 27306" ### address
geo = geocode(location = address) ### geocode address
coord = with(geo, c(lon, lat)) ### vector of long/lat coordinates
## Recalculate distances
food_access = revisit_neighborhood(coord = coord,
                                   i = which(food_access$LocationID == 37123960402))

## Manual entry of LocationID 37033930100
## Get map-aligned neighborhood centroid (address --> lat/long)
address = "3866 Yarboroughs Mill Rd, Milton, NC 27305" ### address
geo = geocode(location = address) ### geocode address
coord = with(geo, c(lon, lat)) ### vector of long/lat coordinates
## Recalculate distances
food_access = revisit_neighborhood(coord = coord,
                                   i = which(food_access$LocationID == 37033930100))

## Manual entry of LocationID 37123960300
## Get map-aligned neighborhood centroid (address --> lat/long)
address = "1513 Moccasin Creek Rd, Troy, NC 27371" ### address
geo = geocode(location = address) ### geocode address
coord = with(geo, c(lon, lat)) ### vector of long/lat coordinates
## Recalculate distances
food_access = revisit_neighborhood(coord = coord,
                                   i = which(food_access$LocationID == 37123960300))

## Manual entry of LocationID 37123960402
## Get map-aligned neighborhood centroid (address --> lat/long)
address = "118 Hudson Ln, Mt Gilead, NC 27306" ### address
geo = geocode(location = address) ### geocode address
coord = with(geo, c(lon, lat)) ### vector of long/lat coordinates
## Recalculate distances
food_access = revisit_neighborhood(coord = coord,
                                   i = which(food_access$LocationID == 37123960402))

## Save data
food_access |> 
  write.csv(file = "food_access_imputation/piedmont-triad-data/review_proximity_healthy_foods.csv", 
            row.names = FALSE)