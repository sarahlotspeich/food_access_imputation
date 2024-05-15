############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps

############################################################################################
## LOAD FOOD ACCESS DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////
############################################################################################
## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv(file = "~/Documents/food/forsyth-data/REVIEW_closest_healthy_foods_store_2022.csv") |> 
  dplyr::bind_rows(
    read.csv(file = "~/Documents/food/piedmont-triad-data/REVIEW_closest_healthy_foods_store_2022.csv")
  ) |> 
  dplyr::filter(LocationID != 37081980100) ## exclude PTI
nrow(food_access) ## N = 387 neighborhoods (exclude the tract with population = 0)

############################################################################################
## MAKE LINE GRAPH OF CUMULATIVE COMPUTING TIME ////////////////////////////////////////////
############################################################################################
food_access |> 
  dplyr::select(LocationID, dplyr::starts_with("comp_")) |> 
  dplyr::mutate(row_id = 1:dplyr::n()) |> 
  tidyr::gather(key = "dist_calc", value = "comp_time", -c(1, 4)) |> 
  dplyr::group_by(dist_calc) |> 
  dplyr::mutate(cum_comp_time = cumsum(comp_time), 
                dist_calc = factor(x = dist_calc, 
                                   levels = c("comp_time_map", "comp_time_straight"), 
                                   labels = c("Map-Based", "Straight-Line"))) |> 
  ggplot(aes(x = row_id, 
             y = cum_comp_time, 
             color = dist_calc)) + 
  geom_line(size = 1) + 
  scale_color_manual(values = c("#6F98DF", 
                                "#EB7B51"), 
                     name = "Distance Calculation:") + 
  theme_minimal(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25), 
        legend.position = "top") + 
  labs(x = "Number of Distances Calculated",
       y = "Cumulative Computing Time (in Seconds)") 
ggsave(filename = "~/Documents/food/figures/figS3_cum_comp_time_line.png", 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in")
ggsave(filename = "~/Documents/food/figures/figS3_cum_comp_time_line.pdf", 
       device = "pdf", 
       width = 7, 
       height = 5, 
       units = "in")