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
## GET DATA ////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Define counties in Forsyth & border
forsyth_border = c("forsyth", "davidson", "davie", "yadkin", "surry", "stokes", 
                   "rockingham", "guilford", "randolph")

## Load map data (census tracts)
tracts = tidycensus::get_acs(state = "NC", 
                             geography = "tract", 
                             county = forsyth_border,
                             variables = "B19013_001",
                             geometry = TRUE, 
                             year = 2010) 
nrow(tracts) ## M = 340 neighborhoods

## Load health outcomes data
health = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/forsyth-data/disease_prevalences_2022.csv") |> 
  dplyr::filter(toupper(CountyName) %in% toupper(forsyth_border)) |> 
  dplyr::mutate(TractFIPS = as.character(TractFIPS), ### to merge with tracts must be character
                BPHIGH = BPHIGH / POP, ### cases of high blood pressure --> prevalence of high blood pressure
                CHD = CHD / POP, ### cases of coronary heart disease --> prevalence of coronary heart disease
                DIABETES = DIABETES / POP, ### cases of diabetes --> prevalence of diabetes
                OBESITY = OBESITY / POP ### cases of obesity --> prevalence of obesity
                )
length(unique(health$TractFIPS)) ## M = 339 neighborhoods

# Merge health outcomes with map datar
health_geo = health |> 
  dplyr::full_join(tracts, by = c("TractFIPS" = "GEOID"))

## Missing data for one census tract that seems to have no population
health_geo |> 
  dplyr::filter(is.na(BPHIGH))

############################################################################################
## DEFINE MAP-MAKING FUNCTION //////////////////////////////////////////////////////////////
############################################################################################
plot_tract_health = function(fill_var, legend_title) {
  health_geo |> 
    ggplot() + 
    geom_sf(aes(fill = {{ fill_var }}, geometry = geometry)) + 
    scale_fill_viridis_c(option = "magma", 
                         name = legend_title, 
                         labels = scales::percent,
                         guide = guide_colourbar(direction = "vertical",
                                                 barwidth = 1, barheight = 8)) +
    theme_void() + 
    theme(plot.margin = margin(l=25,r=20,t=20,b=25),
          legend.position = "right")
}
############################################################################################
## MAPS FROM EACH HEALTH VARIABLE //////////////////////////////////////////////////////////
############################################################################################
## 1. Prevalence of diagnosed diabetes among adults aged >=18 years
plot_diabetes = plot_tract_health(fill_var = DIABETES, 
                                  legend_title = "Diagnosed\nDiabetes")
## 2. Prevalence of coronary heart disease among adults aged >=18 years
plot_chd = plot_tract_health(fill_var = CHD, 
                             legend_title = "Coronary\nHeart\nDisease")
## 3. Prevalence of obesity among adults aged >=18 years
plot_obesity = plot_tract_health(fill_var = OBESITY,
                                 legend_title = "Obesity")
## 4. Prevalence of high blood pressure among adults aged >=18 years
plot_hbp = plot_tract_health(fill_var = BPHIGH, 
                             legend_title = "High Blood\nPressure")

############################################################################################
## COMBINE MAPS AND SAVE ///////////////////////////////////////////////////////////////////
############################################################################################
ggpubr::ggarrange(plot_chd, plot_diabetes, 
                  plot_hbp, plot_obesity, 
                  ncol = 2, nrow = 2)

ggsave(filename = "fig2_map_forsyth_health_outcomes.png",
       device = "png",
       width = 8,
       height = 6,
       units = "in")
