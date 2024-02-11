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
# Census tract outlines
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")
piedmont_triad_ct = get_acs(state = "NC",
                            geography = "tract",
                            county = piedmont_triad,
                            variables = "B01003_001", ## total population,
                            geometry = TRUE,
                            year = 2015)
# County outlines
counties = get_acs(state = "NC", 
                   geography = "county", 
                   county = piedmont_triad,
                   variables = "B19013_001",
                   geometry = TRUE, 
                   year = 2015) |> 
  dplyr::mutate(NAME = sub(pattern = " County, North Carolina", 
                           replacement = "", 
                           x = NAME))
############################################################################################
## LOAD ACS and RUCA DATA FOR PIEDMONT TRIAD CENSUS TRACTS /////////////////////////////////
############################################################################################
acs = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/piedmont_triad_acs_data.csv")

## Merge ACS data into map data 
piedmont_triad_ct = piedmont_triad_ct |> 
  dplyr::mutate(GEOID = as.numeric(GEOID)) |> 
  dplyr::left_join(acs)

## 2010 rural/urban continuum codes (merge into health outcomes and food access)
ruca = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/forsyth-data/ruca2010revised.csv")
piedmont_triad_ct = piedmont_triad_ct |> 
  dplyr::left_join(y = ruca, 
                   by = dplyr::join_by(GEOID == StateCountyTract))
############################################################################################
## CREATE FUNCTION TO MAP VARIABLES FROM ACS DATA //////////////////////////////////////////
############################################################################################
plot_tract_acs = function(fill_var, title, legend_title = "", label_scale = scales::number, label_counties = FALSE) {
  if (label_counties) {
    ggplot() + 
      geom_sf(data = piedmont_triad_ct, 
              aes(fill = {{ fill_var }}, 
                  geometry = geometry)) + 
      geom_sf(data = counties, 
              aes(geometry = geometry), 
              color = "white", 
              fill = NA, 
              size = 25) +   
      geom_sf_label(data = counties, 
                    aes(geometry = geometry, label = NAME)) + 
      scale_fill_viridis_c(option = "magma", 
                           name = legend_title, 
                           labels = label_scale,
                           guide = guide_colourbar(direction = "horizontal",
                                                   barwidth = 8, 
                                                   barheight = 0.5)) +
      theme_void(base_size = 8) + 
      theme(plot.margin = margin(0, 0, 0, 0), #margin(l = 25, r = 20, t = 20, b = 25),
            legend.position = "top", 
            plot.title = element_text(face = "bold", 
                                      hjust = 0.5, 
                                      margin = margin(t = 0, r = 0, b = 10, l = 0))) + 
      ggtitle(label = title)
  } else {
    ggplot() + 
      geom_sf(data = piedmont_triad_ct, 
              aes(fill = {{ fill_var }}, 
                  geometry = geometry)) + 
      geom_sf(data = counties, 
              aes(geometry = geometry), 
              color = "white", 
              fill = NA, 
              size = 25) +   
      scale_fill_viridis_c(option = "magma", 
                           name = legend_title, 
                           labels = label_scale,
                           guide = guide_colourbar(direction = "horizontal",
                                                   barwidth = 8, 
                                                   barheight = 0.5)) +
      theme_void(base_size = 8) + 
      theme(plot.margin = margin(0, 0, 0, 0), #margin(l = 25, r = 20, t = 20, b = 25),
            legend.position = "top", 
            plot.title = element_text(face = "bold", 
                                      hjust = 0.5, 
                                      margin = margin(t = 0, r = 0, b = 10, l = 0))) + 
      ggtitle(label = title)
  }
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
## 7. Population density
map_density = plot_tract_acs(fill_var = PopulationDensity, 
                             title = "Population Density", 
                             label_scale = scales::label_comma())

## 8. Percent children in female-headed households 
map_fem = plot_tract_acs(fill_var = PERC_FEM_HEAD, 
                         title = "Female-Headed Households", 
                         label_scale = scales::label_comma())
############################################################################################
## COMBINE MAPS AND SAVE ///////////////////////////////////////////////////////////////////
############################################################################################
ggpubr::ggarrange(map_density, map_income, 
                  map_poverty, map_snap, 
                  map_cars, map_insured, 
                  map_college, map_fem, 
                  ncol = 3, nrow = 3, 
                  labels = "AUTO")
 
ggsave(filename = "~/Documents/food/figures/fig1_map_piedmont_triad_acs_data.png",
       device = "png",
       width = 8,
       height = 8,
       units = "in")
