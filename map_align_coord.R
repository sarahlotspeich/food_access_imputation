map_align_coord = function(longitude, latitude) {
  ## Get all reverse-geocoding results 
  all_rev = suppressWarnings(
    suppressMessages(
      ggmap::revgeocode(location = c(longitude, latitude), 
                        output = "all")
    )
  )
  
  ## Extract location types and lat/longs 
  location_type = sapply(X = 1:length(all_rev$results), 
                         FUN = function(x) all_rev$results[[x]]$geometry$location_type)
  lat_lon = t(sapply(X = 1:length(all_rev$results), 
                     FUN = function(x) all_rev$results[[x]]$geometry$location[c(2, 1)]))
  
  ## Calculate distance between original centroid and each rooftop-level match
  dist_between = as.vector(rep(NA, length(location_type)))
  for (l in which(location_type == "ROOFTOP")) { 
    dist_between[l] = geosphere::distHaversine(p1 = c(longitude, latitude), 
                                               p2 = as.numeric(unlist(all_rev$results[[l]]$geometry$location[c(2, 1)])), 
                                               r = 3958.8)
  }
  
  if (any(location_type == "ROOFTOP")) {
    ## If there are any rooftop matches, let the nearest rooftop-level match 
    ## be the centroid address 
    address = all_rev$results[[which.min(dist_between)]]$formatted_address  
    lat = all_rev$results[[which.min(dist_between)]]$geometry$location$lat
    lon = all_rev$results[[which.min(dist_between)]]$geometry$location$lng
  } else {
    ## If there are no rooftop matches, return NA and review manually
    address = NA 
    lat = NA
    lon = NA
  }
  
  ## Return matched address and its lat/lon coordinates
  return(list("address" = address, 
              "long_lat" = c(lon, lat)))
}