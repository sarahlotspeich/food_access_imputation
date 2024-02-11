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
## Define counties in Piedmont Triad
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")

## Load map data (census tracts)
tracts = get_acs(state = "NC", 
                 geography = "tract", 
                 county = piedmont_triad,
                 variables = "B19013_001",
                 geometry = TRUE, 
                 year = 2015)
nrow(tracts) ## M = 388 census tracts (neighborhoods)

## Load health outcomes data
health = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/disease_prevalences_2022.csv") |> 
  dplyr::filter(toupper(CountyName) %in% toupper(piedmont_triad)) |> 
  dplyr::mutate(TractFIPS = as.character(TractFIPS), ### to merge with tracts must be character
                BPHIGH = BPHIGH / POP, ### cases of high blood pressure --> prevalence of high blood pressure
                CHD = CHD / POP, ### cases of coronary heart disease --> prevalence of coronary heart disease
                DIABETES = DIABETES / POP, ### cases of diabetes --> prevalence of diabetes
                OBESITY = OBESITY / POP ### cases of obesity --> prevalence of obesity
                )
length(unique(health$TractFIPS)) ## M = 387 census tracts (neighborhoods)

# Merge health outcomes with map data
health_geo = health |> 
  dplyr::full_join(tracts, by = c("TractFIPS" = "GEOID"))

## Missing data for one census tract in Guilford County that seems to have no population
### And be missing the ACS data, as well
health_geo |> 
  dplyr::filter(is.na(BPHIGH))

############################################################################################
## DEFINE MAP-MAKING FUNCTION //////////////////////////////////////////////////////////////
############################################################################################
plot_tract_health = function(fill_var, title, legend_title = "") {
  health_geo |> 
    ggplot() + 
    geom_sf(aes(fill = {{ fill_var }}, geometry = geometry)) + 
    scale_fill_viridis_c(option = "viridis", 
                         name = legend_title, 
                         labels = scales::percent,
                         guide = guide_colourbar(direction = "vertical",
                                                 barwidth = 1, 
                                                 barheight = 10)) +
    theme_void(base_size = 10) + 
    theme(plot.margin = margin(l=25, r=20, t=20, b=25),
          legend.position = "right", 
          plot.title = element_text(face = "bold", hjust = 0.5)) + 
    ggtitle(label = title)
}
############################################################################################
## MAPS FROM EACH HEALTH VARIABLE //////////////////////////////////////////////////////////
############################################################################################
## 1. Prevalence of diagnosed diabetes among adults aged >=18 years
plot_diabetes = plot_tract_health(fill_var = DIABETES, 
                                  title = "Diagnosed Diabetes")
## 2. Prevalence of coronary heart disease among adults aged >=18 years
plot_chd = plot_tract_health(fill_var = CHD, 
                             title = "Coronary Heart Disease")
## 3. Prevalence of obesity among adults aged >=18 years
plot_obesity = plot_tract_health(fill_var = OBESITY,
                                 title = "Obesity")
## 4. Prevalence of high blood pressure among adults aged >=18 years
plot_hbp = plot_tract_health(fill_var = BPHIGH, 
                             title = "High Blood Pressure")

############################################################################################
## COMBINE MAPS AND SAVE ///////////////////////////////////////////////////////////////////
############################################################################################
ggpubr::ggarrange(plot_chd, plot_diabetes, 
                  plot_hbp, plot_obesity, 
                  ncol = 2, nrow = 2)

ggsave(filename = "~/Documents/food/figures/fig2_map_piedmont_triad_health_outcomes.png",
       device = "png",
       width = 10,
       height = 8,
       units = "in")
ggsave(plot = plot_chd, 
       filename = "~/Documents/food/figures/fig2_map_piedmont_triad_chd.png",
       device = "png",
       width = 4,
       height = 4,
       units = "in")
