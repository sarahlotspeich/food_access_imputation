############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes

## Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("~/Documents/food/set_api_keys.R")
############################################################################################
## LOAD MAP DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////////////
## To align with CDC Places dataset, use 2015 census geometry. /////////////////////////////
############################################################################################
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")
piedmont_triad_ct = get_acs(state = "NC",
                     geography = "tract",
                     county = piedmont_triad,
                     variables = "B01003_001", ## total population,
                     geometry = TRUE,
                     year = 2015)
############################################################################################
## LOAD ACS DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////////////
############################################################################################
acs = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/piedmont_triad_acs_data.csv")
## Merge ACS data into map data 
piedmont_triad_ct = piedmont_triad_ct |> 
  dplyr::mutate(GEOID = as.numeric(GEOID)) |> 
  dplyr::left_join(acs)
############################################################################################
## CREATE FUNCTION TO MAP VARIABLES FROM ACS DATA //////////////////////////////////////////
############################################################################################
plot_tract_acs = function(fill_var, title, legend_title = "", label_scale = scales::number) {
  piedmont_triad_ct |> 
    ggplot() + 
    geom_sf(aes(fill = {{ fill_var }}, geometry = geometry)) + 
    scale_fill_viridis_c(option = "magma", 
                         name = legend_title, 
                         labels = label_scale,
                         guide = guide_colourbar(direction = "horizontal",
                                                 barwidth = 8, 
                                                 barheight = 1)) +
    theme_void(base_size = 10) + 
    theme(plot.margin = margin(l=25,r=20,t=20,b=25),
          legend.position = "bottom", 
          plot.title = element_text(face = "bold", hjust = 0.5)) + 
    ggtitle(label = title)
}
############################################################################################
## MAPS FROM EACH ACS VARIABLE /////////////////////////////////////////////////////////////
############################################################################################
## 1. Median family income (past 12 months) per census tract
map_income = plot_tract_acs(fill_var = INCOME, 
                            title = "Median Family Income", 
                            label_scale = scales::dollar)
## 2. Percent population with income below poverty line (past 12 months)
map_poverty = plot_tract_acs(fill_var = PERC_POVERTY, 
                             title = "Income Below Poverty Line", 
                             label_scale = scales::percent)
## 3. Percent households receiving food stamps / SNAP (past 12 months)
map_snap = plot_tract_acs(fill_var = PERC_SNAP, 
                          title = "Households Receiving SNAP", 
                          label_scale = scales::percent)
## 4. Percent workers (>= 16 yo) driving alone in car, truck, or van to work
map_cars = plot_tract_acs(fill_var = PERC_CAR, 
                          title = "Workers Driving To Work", 
                          label_scale = scales::percent)
## 5. Percent population with health insurance (public or private)
map_insured = plot_tract_acs(fill_var = PERC_INSURED, 
                             title = "With Health Insurance", 
                             label_scale = scales::percent)
## 6. Percent population (>= 25 yo) completing at least some college
map_college = plot_tract_acs(fill_var = PERC_COLLEGE, 
                             title = "At Least Some College", 
                             label_scale = scales::percent)
############################################################################################
## COMBINE MAPS AND SAVE ///////////////////////////////////////////////////////////////////
############################################################################################
ggpubr::ggarrange(map_income, map_poverty, 
                  map_snap, map_cars, 
                  map_insured, map_college,
                  ncol = 3, nrow = 2)
 
ggsave(filename = "~/Documents/food/figures/fig1_map_piedmont_triad_acs_data.png",
       device = "png",
       width = 10,
       height = 8,
       units = "in")
