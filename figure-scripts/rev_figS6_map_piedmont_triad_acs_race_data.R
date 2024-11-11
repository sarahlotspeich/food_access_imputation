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
counties = get_acs(state = "NC",
                   geography = "county",
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
## 1. Percent Black population 
map_black = plot_tract_acs(fill_var = PERC_BLACK, 
                           title = paste0("Black or African America Population\n(State Average = ", 
                                          round(100*state_avg$PERC_BLACK),
                                          "%)"), 
                           label_scale = scales::percent,
                           mid = state_avg$PERC_BLACK)

## 2. Percent White population 
map_white = plot_tract_acs(fill_var = PERC_WHITE, 
                           title = paste0("White Population\n(State Average = ", 
                                          round(100*state_avg$PERC_WHITE),
                                          "%)"), 
                           label_scale = scales::percent,
                           mid = state_avg$PERC_WHITE)

## 3. Percent Asian population 
map_asian = plot_tract_acs(fill_var = PERC_ASIAN, 
                           title = paste0("Asian Population\n(State Average = ", 
                                          round(100*state_avg$PERC_ASIAN),
                                          "%)"), 
                           label_scale = scales::percent,
                           mid = state_avg$PERC_ASIAN)

## 4. Percent American Indian or Alaska Native population 
map_native = plot_tract_acs(fill_var = PERC_NATIVE, 
                            title = paste0("American Indian or\nAlaska Native Population\n(State Average = ", 
                                           round(100*state_avg$PERC_NATIVE),
                                           "%)"), 
                            label_scale = scales::percent,
                            mid = state_avg$PERC_NATIVE)

## 5. Percent Native Hawaiian and Other Pacific Islander Alone
map_hawaiian = plot_tract_acs(fill_var = PERC_HAWAIIAN, 
                              title = paste0("Native Hawaiian and\nOther Pacific Islander Population\n(State Average = ", 
                                             round(100*state_avg$PERC_HAWAIIAN),
                                             "%)"), 
                              label_scale = scales::percent,
                              mid = state_avg$PERC_HAWAIIAN)

############################################################################################
## COMBINE MAPS AND SAVE ///////////////////////////////////////////////////////////////////
############################################################################################
ggpubr::ggarrange(map_white, map_black, map_asian,
                  map_native, map_hawaiian,
                  ncol = 3, nrow = 2, 
                  labels = "AUTO")
 
ggsave(filename = "figures/rev_figS6_map_piedmont_triad_acs_race_data.png",
       device = "png",
       width = 10,
       height = 7,
       units = "in")
ggsave(filename = "figures/rev_figS6_map_piedmont_triad_acs_data.pdf",
       device = "pdf",
       width = 10,
       height = 7,
       units = "in")