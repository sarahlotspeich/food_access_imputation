############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes
library(dplyr) ## to wrangle data

## Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("food_access_imputation/set_api_keys.R")

############################################################################################
## LOAD MAP DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////////////
## To align with CDC Places dataset, use 2015 census geometry. /////////////////////////////
############################################################################################
## Define counties in the Piedmont Triad
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")

## Census tract outlines
piedmont_triad_ct = get_acs(state = "NC",
                            geography = "tract",
                            county = piedmont_triad,
                            variables = "B01003_001", ## total population,
                            geometry = TRUE,
                            year = 2015)

############################################################################################
## LOAD ACS and RUCA DATA FOR PIEDMONT TRIAD CENSUS TRACTS /////////////////////////////////
############################################################################################
acs = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/piedmont_triad_acs_data.csv")

## Merge in 2015 ACS data
piedmont_triad_ct = piedmont_triad_ct |> 
  dplyr::mutate(GEOID = as.numeric(GEOID)) |> 
  dplyr::left_join(acs)

## Merge in 2010 RUCA codes 
ruca = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/ruca2010revised.csv")
piedmont_triad_ct = piedmont_triad_ct |> 
  dplyr::left_join(y = ruca, 
                   by = dplyr::join_by(GEOID == StateCountyTract))

## State averages of each variable
state_avg = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/state_average_acs_data.csv")

############################################################################################
## CREATE FUNCTION TO MAP VARIABLES FROM ACS DATA //////////////////////////////////////////
############################################################################################
plot_tract_acs = function(fill_var, title, legend_title = "", mid, label_scale = scales::number, label_counties = FALSE) {
  if (label_counties) {
    ggplot() + 
      geom_sf(data = piedmont_triad_ct, 
              aes(fill = {{ fill_var }}, 
                  geometry = geometry)) + 
      geom_sf(data = counties, 
              aes(geometry = geometry), 
              fill = NA, 
              size = 25) +   
      geom_sf_label(data = counties, 
                    aes(geometry = geometry, label = NAME)) + 
      scale_fill_gradientn(
        colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
        rescaler = ~ scales::rescale_mid(.x, mid = mid),
        name = legend_title, 
        labels = label_scale,
        guide = guide_colourbar(direction = "vertical",
                                barwidth = 1,
                                barheight = 8)) +
      theme_void(base_size = 8) + 
      theme(plot.margin = margin(0, 0, 0, 0), 
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
              fill = NA, 
              size = 25) +   
      scale_fill_gradientn(
        colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
        rescaler = ~ scales::rescale_mid(.x, mid = mid),
        name = legend_title, 
        labels = label_scale,
        guide = guide_colourbar(direction = "vertical",
                                barwidth = 1,
                                barheight = 8)) +
      theme_void(base_size = 8) + 
      theme(plot.margin = margin(0, 0, 0, 0),
            legend.position = "right", 
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
                            title = "Median Family Income\n(State Average = $54,036)", 
                            label_scale = scales::dollar, 
                            mid = state_avg$INCOME)
## 2. Percent population with income below poverty line (past 12 months)
map_poverty = plot_tract_acs(fill_var = PERC_POVERTY, 
                             title = paste0("Income Below Poverty Line\n(State Average = ", 
                                            round(100*state_avg$PERC_POVERTY),
                                            "%)"), 
                             label_scale = scales::percent, 
                             mid = state_avg$PERC_POVERTY)
## 3. Percent households receiving food stamps / SNAP (past 12 months)
map_snap = plot_tract_acs(fill_var = PERC_SNAP, 
                          title = paste0("Households Receiving SNAP\n(State Average = ", 
                                         round(100*state_avg$PERC_SNAP),
                                         "%)"),
                          label_scale = scales::percent,
                          mid = state_avg$PERC_SNAP)
## 4. Percent workers (>= 16 yo) driving alone in car, truck, or van to work
map_cars = plot_tract_acs(fill_var = PERC_CAR, 
                          title = paste0("Workers Driving To Work\n(State Average = ", 
                                         round(100*state_avg$PERC_CAR),
                                         "%)"),
                          label_scale = scales::percent,
                          mid = state_avg$PERC_CAR)
## 5. Percent population with health insurance (public or private)
map_insured = plot_tract_acs(fill_var = PERC_INSURED, 
                             title = paste0("With Health Insurance\n(State Average = ", 
                                            round(100*state_avg$PERC_INSURED),
                                            "%)"),
                             label_scale = scales::percent,
                             mid = state_avg$PERC_INSURED)
## 6. Percent population (>= 25 yo) completing at least some college
map_college = plot_tract_acs(fill_var = PERC_COLLEGE, 
                             title = paste0("At Least Some College\n(State Average = ", 
                                            round(100*state_avg$PERC_COLLEGE),
                                            "%)"), 
                             label_scale = scales::percent,
                             mid = state_avg$PERC_COLLEGE)
## 7. Population density
map_density = plot_tract_acs(fill_var = PopulationDensity, 
                             title = "Population Density\n(State Average = 625)",
                             label_scale = scales::label_comma(),
                             mid = 625)

## 8. Percent children in female-headed households 
map_fem = plot_tract_acs(fill_var = PERC_FEM_HEAD, 
                         title = paste0("Female-Headed Households\n(State Average = ", 
                                        round(100*state_avg$PERC_FEM_HEAD),
                                        "%)"), 
                         label_scale = scales::percent,
                         mid = state_avg$PERC_FEM_HEAD)
############################################################################################
## COMBINE MAPS AND SAVE ///////////////////////////////////////////////////////////////////
############################################################################################
ggpubr::ggarrange(map_density, map_income, 
                  map_poverty, map_snap, 
                  map_cars, map_insured, 
                  map_college, map_fem, 
                  ncol = 3, nrow = 3, 
                  labels = "AUTO")
 
ggsave(filename = "figures/figS5_map_piedmont_triad_acs_data.png",
       device = "png",
       width = 10,
       height = 10,
       units = "in")
ggsave(filename = "figures/figS5_map_piedmont_triad_acs_data.pdf",
       device = "pdf",
       width = 10,
       height = 10,
       units = "in")