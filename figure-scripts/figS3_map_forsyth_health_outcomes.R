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
## Load map data (census tracts)
tracts = get_acs(state = "NC", 
                 geography = "tract", 
                 county = "FORSYTH",
                 variables = "B19013_001",
                 geometry = TRUE, 
                 year = 2015)
nrow(tracts) ## M = 93 census tracts (neighborhoods)

## Load health outcomes data
health = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/disease_prevalences_2022.csv") |> 
  dplyr::filter(toupper(CountyName) == "FORSYTH") |> 
  dplyr::mutate(TractFIPS = as.character(TractFIPS), ### to merge with tracts must be character
                BPHIGH = BPHIGH / POP, ### cases of high blood pressure --> prevalence of high blood pressure
                CHD = CHD / POP, ### cases of coronary heart disease --> prevalence of coronary heart disease
                DIABETES = DIABETES / POP, ### cases of diabetes --> prevalence of diabetes
                OBESITY = OBESITY / POP ### cases of obesity --> prevalence of obesity
                )
length(unique(health$TractFIPS)) ## 93 census tracts in Forsyth

# Merge health outcomes with map data
health_geo = health |> 
  dplyr::full_join(tracts, by = c("TractFIPS" = "GEOID"))

# Read in state average (median) prevalences for each disease to center 
state_avg = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/state_average_disease.csv")

############################################################################################
## DEFINE MAP-MAKING FUNCTION //////////////////////////////////////////////////////////////
############################################################################################
plot_tract_health = function(fill_var, title, legend_title = "", mid, city_hall) {
  fill_var_bounds = health_geo |> 
    dplyr::summarize(min = min({{ fill_var }}, na.rm = TRUE), 
                     max = max({{ fill_var }}, na.rm = TRUE))
  health_geo |> 
    ggplot() + 
    geom_sf(aes(fill = {{ fill_var }}, geometry = geometry)) + 
    scale_fill_gradientn(
      colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
      rescaler = ~ scales::rescale_mid(.x, mid = mid),
      name = legend_title, 
      labels = scales::percent,
      guide = guide_colourbar(direction = "vertical",
                              barwidth = 1,
                              barheight = 8)) +
    theme_void(base_size = 10) + 
    theme(plot.margin = margin(l=25, r=20, t=20, b=25),
          legend.position = "right", 
          plot.title = element_text(face = "bold", hjust = 0.5)) + 
    ggtitle(label = title) + 
    geom_point(data = city_hall, 
               aes(x = lon, y = lat), 
               shape = 17, 
               alpha = 0.8, 
               size = 2)
}
############################################################################################
## MAPS FROM EACH HEALTH VARIABLE //////////////////////////////////////////////////////////
############################################################################################
ws_city_hall = ggmap::geocode(location = "101 N Main St, Winston-Salem, NC 27101")

## 1. Prevalence of diagnosed diabetes among adults aged >=18 years
plot_diabetes = plot_tract_health(fill_var = DIABETES, 
                                  title = paste0("Diagnosed Diabetes\n(State Average = ", round(as.numeric(state_avg["DIABETES"] * 100)), "%)"), 
                                  mid = as.numeric(state_avg["DIABETES"]), 
                                  city_hall = ws_city_hall)
## 2. Prevalence of coronary heart disease among adults aged >=18 years
plot_chd = plot_tract_health(fill_var = CHD, 
                             title = paste0("Coronary Heart Disease\n(State Average = ", round(as.numeric(state_avg["CHD"] * 100)), "%)"),
                             mid = as.numeric(state_avg["CHD"]),
                             city_hall = ws_city_hall)
## 3. Prevalence of obesity among adults aged >=18 years
plot_obesity = plot_tract_health(fill_var = OBESITY,
                                 title = paste0("Obesity\n(State Average = ", round(as.numeric(state_avg["OBESITY"] * 100)), "%)"),
                                 mid = as.numeric(state_avg["OBESITY"]),
                                 city_hall = ws_city_hall)
## 4. Prevalence of high blood pressure among adults aged >=18 years
plot_hbp = plot_tract_health(fill_var = BPHIGH, 
                             title = paste0("High Blood Pressure\n(State Average = ", round(as.numeric(state_avg["BPHIGH"] * 100)), "%)"),
                             mid = as.numeric(state_avg["BPHIGH"]),
                             city_hall = ws_city_hall)

############################################################################################
## COMBINE MAPS AND SAVE ///////////////////////////////////////////////////////////////////
############################################################################################
ggpubr::ggarrange(plot_chd, plot_diabetes, 
                  plot_hbp, plot_obesity, 
                  ncol = 2, nrow = 2)
ggsave(filename = "~/Documents/food/figures/figS2_map_forsyth_health_outcomes.png",
       device = "png",
       width = 6,
       height = 5,
       units = "in")
ggsave(filename = "~/Documents/food/figures/figS2_map_forsyth_health_outcomes.pdf",
       device = "pdf",
       width = 6,
       height = 5,
       units = "in")
ggpubr::ggarrange(plot_chd, plot_diabetes, 
                  plot_hbp, plot_obesity, 
                  ncol = 4, nrow = 1)
ggsave(filename = "~/Documents/food/figures/figS3_map_forsyth_health_outcomes_wide.png",
       device = "png",
       width = 12,
       height = 6,
       units = "in")
ggsave(filename = "~/Documents/food/figures/figS3_map_forsyth_health_outcomes_wide.pdf",
       device = "pdf",
       width = 12,
       height = 6,
       units = "in")
