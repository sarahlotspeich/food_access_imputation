############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes

## Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("set_api_keys.R")
############################################################################################
## LOAD MAP DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////////////
## To align with CDC Places dataset, use 2015 census geometry. /////////////////////////////
############################################################################################
border_counties = c("DAVIDSON", "DAVIE",  "FORSYTH", "GUILFORD", "RANDOLPH", "ROCKINGHAM",
                    "STOKES", "SURRY", "YADKIN")
border_border_counties = c("ALLEGHANY", "WILKES", "IREDELL", "ROWAN", "ALAMANCE", "CASWELL")
forsyth_ct = get_acs(state = "NC",
                     geography = "tract",
                     county = tolower(c("forsyth", border_counties, border_border_counties)),
                     variables = "B01003_001", ## total population,
                     geometry = TRUE,
                     year = 2015)
############################################################################################
## LOAD ACS DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////////////
############################################################################################
acs = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/forsyth-data/forsyth_acs_data.csv")
## Merge ACS data into map data 
forsyth_ct = forsyth_ct |> 
  dplyr::mutate(GEOID = as.numeric(GEOID)) |> 
  dplyr::left_join(acs)
############################################################################################
## CREATE FUNCTION TO MAP VARIABLES FROM ACS DATA //////////////////////////////////////////
############################################################################################
plot_tract_acs = function(fill_var, legend_title, label_scale = scales::number) {
  forsyth_ct |> 
    ggplot() + 
    geom_sf(aes(fill = {{ fill_var }}, geometry = geometry)) + 
    scale_fill_viridis_c(option = "magma", 
                         name = legend_title, 
                         labels = label_scale,
                         guide = guide_colourbar(direction = "vertical",
                                                 barwidth = 1, barheight = 8)) +
    theme_void() + 
    theme(plot.margin = margin(l=25,r=20,t=20,b=25),
          legend.position = "right")
}
############################################################################################
## MAPS FROM EACH ACS VARIABLE /////////////////////////////////////////////////////////////
############################################################################################
## 1. Median family income (past 12 months) per census tract
map_income = plot_tract_acs(fill_var = INCOME, 
                            legend_title = "Median\nFamily\nIncome", 
                            label_scale = scales::dollar)
## 2. Percent population with income below poverty line (past 12 months)
map_poverty = plot_tract_acs(fill_var = PERC_POVERTY, 
                          legend_title = "Income\nBelow\nPoverty\nLine", 
                          label_scale = scales::percent)
## 3. Percent households receiving food stamps / SNAP (past 12 months)
map_snap = plot_tract_acs(fill_var = PERC_SNAP, 
                          legend_title = "Households\nReceiving\nSNAP", 
                          label_scale = scales::percent)
## 4. Percent workers (>= 16 yo) driving alone in car, truck, or van to work
map_cars = plot_tract_acs(fill_var = PERC_CAR, 
                          legend_title = "Workers\nDriving\nTo Work", 
                          label_scale = scales::percent)
## 5. Percent population with health insurance (public or private)
map_insured = plot_tract_acs(fill_var = PERC_INSURED, 
                             legend_title = "With\nHealth\nInsurance", 
                             label_scale = scales::percent)
## 6. Percent population (>= 25 yo) completing at least some college
map_college = plot_tract_acs(fill_var = PERC_COLLEGE, 
                             legend_title = "Completed\nAt Least\nSome\nCollege", 
                             label_scale = scales::percent)
############################################################################################
## COMBINE MAPS AND SAVE ///////////////////////////////////////////////////////////////////
############################################################################################
ggpubr::ggarrange(map_income, map_poverty, 
                  map_snap, map_cars, 
                  map_insured, map_college,
                  ncol = 2, nrow = 3)
 
ggsave(filename = "fig1_map_forsyth_acs_data.png",
       device = "png",
       width = 8,
       height = 8,
       units = "in")
