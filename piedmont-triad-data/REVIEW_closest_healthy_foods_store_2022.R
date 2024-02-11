################################################################################
# SETUP ########################################################################
################################################################################

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("~/Documents/food/set_api_keys.R")

# Source function to get map-aligned neighborhood centroids
source("~/Documents/food/map_align_coord.R")

################################################################################
# FOOD ACCESS (HEALTHY STORES) #################################################
################################################################################
# Load healthy foods stores for Piedmont Triad and the surrounding counties from the historic SNAP retailers data
grocery = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/healthy_foods_stores_2022.csv")
nrow(grocery) ## M = 701 healthy foods stores

# Start with Forsyth county & border food access variables (don't need to re-query)
food_access = read.csv("~/Documents/food/piedmont-triad-data/addl_closest_healthy_foods_store_2022.csv")
nrow(food_access) ## N = 48 additional neighborhoods

# View summary of data
summary(food_access) ## 3 NAs 

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
    straight_dist <- geosphere::distHaversine(p1 = coord, 
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
    map_dist <- ggmap::mapdist(from = grocery_within_q20,
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

# Review seven neighborhood that didn't have map-aligned centroids
## So they didn't have proximity calculations the first time 
food_access |> 
  dplyr::filter(is.na(dist_closest_straight))

## Manual entry of LocationID 37033930100
## Get map-aligned neighborhood centroid (address --> lat/long)
address = "3866 Yarboroughs Mill Rd, Milton, NC 27305" ### address
geo = ggmap::geocode(location = address) ### geocode address
coord = with(geo, c(lon, lat)) ### vector of long/lat coordinates
## Recalculate distances
  food_access = revisit_neighborhood(coord = coord,
                                     i = which(food_access$LocationID == 37033930100))
summary(food_access) ### 2 NA's remaining
  
## Manual entry of LocationID 37123960300
## Get map-aligned neighborhood centroid (address --> lat/long)
address = "1513 Moccasin Creek Rd, Troy, NC 27371" ### address
geo = ggmap::geocode(location = address) ### geocode address
coord = with(geo, c(lon, lat)) ### vector of long/lat coordinates
## Recalculate distances
food_access = revisit_neighborhood(coord = coord,
                                   i = which(food_access$LocationID == 37123960300))
summary(food_access) ### 1 NA's remaining

## Manual entry of LocationID 37123960402
## Get map-aligned neighborhood centroid (address --> lat/long)
address = "118 Hudson Ln, Mt Gilead, NC 27306" ### address
geo = ggmap::geocode(location = address) ### geocode address
coord = with(geo, c(lon, lat)) ### vector of long/lat coordinates
## Recalculate distances
food_access = revisit_neighborhood(coord = coord,
                                   i = which(food_access$LocationID == 37123960402))
summary(food_access) ### 0 NA's remaining

################################################################################
# CHECK FOR INVALID DISTANCE VALUES ############################################
################################################################################
# Check that straight-line distance <= map-based distance
food_access |> 
  dplyr::filter(dist_closest_straight > dist_closest_map)
## No neighborhoods have straight-line distance > map-based distance (not possible)

################################################################################
# RE-SAVE DATA #################################################################
################################################################################
# View summary of data
summary(food_access)

## Save data
food_access |> 
  write.csv(file = "~/Documents/food/piedmont-triad-data/REVIEW_closest_healthy_foods_store_2022.csv", 
            row.names = FALSE)
