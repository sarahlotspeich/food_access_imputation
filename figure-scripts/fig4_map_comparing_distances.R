# Load packages
library(ggplot2)

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("food/set_api_keys.R")

# Get a map of the area around Wake Forest
wake_map = ggmap::qmap(location = c(lon = -80.291648, 
                                    lat = 36.142265), 
                       zoom = 14)

# Get driving route from Reynolda House to Food Lion using the Google Maps API
drive_route = ggmap::route(from = "2250 Reynolda Rd, Winston-Salem, NC 27106", 
                           to = "7760 North Point Blvd, Winston-Salem, NC 27106", 
                           mode = "driving", 
                           structure = "route")

# Add rows to the driving route to visually complete the route 
add_rows = data.frame(lon = c(-80.292714, -80.290715, -80.291648, -80.287652), 
                      lat = c(36.133942, 36.137491, 36.142265, 36.148390)) 
drive_route = drive_route |> 
  dplyr::bind_rows(add_rows) 
drive_route = drive_route[c(1:6, 11:14, 7:10), ] ## reorder to plot correctly
drive_dist = sum(drive_route$miles, na.rm = TRUE) ## sum over the legs of the route to get total driving distance

# Calculate the straight-line distance between the start/end of the driving route (Reynolda House and Food Lion)
straight_dist = geosphere::distHaversine(p1 = drive_route[1, c("lon", "lat")], 
                                         p2 = drive_route[nrow(drive_route), c("lon", "lat")], 
                                         r = 3958.8
                                         )

# Create the map with both driving routes
wake_map + 
  geom_path(data = drive_route, 
            aes(x = lon, y = lat, color = "Map-Based Distance"), 
            linewidth = 3) + 
  geom_path(data = drive_route[c(1, nrow(drive_route)), ], 
            aes(x = lon, y = lat, color = "Straight-Line Distance"), 
            linewidth = 3) + 
  geom_point(data = drive_route[1, c("lon", "lat")], 
             aes(x = lon, y = lat),
             shape = 15, size = 6) +
  geom_point(data = drive_route[nrow(drive_route), c("lon", "lat")], 
             aes(x = lon, y = lat),
             shape = 17, size = 6) +
  annotate(geom = "label", 
           x = drive_route$lon[nrow(drive_route)] + 0.0055, 
           y = mean(drive_route$lat[c(1, nrow(drive_route))]) + 0.003,
           label = paste("Straight-line\ndistance:\n", 
                         round(straight_dist, 2), 
                         "miles", sep = " "),
           size = 12, color = "#721F81FF",
           fill = "white", label.size = NA) +
  annotate(geom = "label", 
           x = min(drive_route$lon) - 0.001, 
           y = mean(drive_route$lat[c(1, nrow(drive_route))]) + 0.0095,
           label = paste("Map-based\ndistance:\n", 
                         round(drive_dist, 2), 
                         "miles", sep = " "),
           size = 12, color = "#F1605DFF",
           fill = "white", label.size = NA) +
  theme(legend.position = c(0.3, 0.7), 
        legend.title = element_blank(),
        legend.background = element_rect(color = "black")) + 
  scale_color_manual(values = c("#F1605DFF", "#78247a"),
                     guide = "none") + 
  coord_map(ylim=c(36.120, 36.16), 
            xlim = c(-80.31, -80.27))

# Save Figure 1
ggsave(filename = "figures/fig4_map_comparing_distances.png", 
       device = "png", units = "in", width = 14, height = 16)
