############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps
library(dplyr) ## for data wrangling
library(tidycensus) ## to get underlying census tract map shapes

## Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("set_api_keys.R")

############################################################################################
## GET DATA ////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Define counties in Piedmont Triad
piedmont_triad = c("Surry", "Stokes", "Rockingham", "Caswell", 
                   "Yadkin", "Forsyth", "Guilford", "Alamance", 
                   "Davie", "Davidson", "Randolph", "Montgomery")

## Load USDA Food Research Atlas (2019)
fra = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/FoodAccessResearchAtlasData2019.csv")

## Summarize access for the state
state_avg = fra |> 
  filter(State == "North Carolina") |> 
  group_by(County) |> 
  summarize(prop_LATracts_half = mean(LATracts_half), 
            prop_LATracts1 = mean(LATracts1)) |> 
  ungroup() |> 
  summarize(med_prop_LATracts_half = median(prop_LATracts_half), 
            med_prop_LATracts1 = median(prop_LATracts1))

## Subset to counties in the Piedmont Triad
fra = fra |> 
  filter(State == "North Carolina", 
         County %in% paste(piedmont_triad, "County")) |> 
  select(CensusTract, County, LATracts_half, LATracts1)

## Summarize whole Piedmont Triad
fra |> 
  summarize(prop_LATracts_half = mean(LATracts_half), 
            prop_LATracts1 = mean(LATracts1))

## Summarize by county 
fra_summ = fra |> 
  group_by(County) |> 
  summarize(prop_LATracts_half = mean(LATracts_half), 
            prop_LATracts1 = mean(LATracts1))

# Load map data (census tracts)
tracts = get_acs(state = "NC", 
                 geography = "tract", 
                 county = piedmont_triad,
                 variables = "B19013_001",
                 geometry = TRUE, 
                 year = 2015)

# Merge food research atlas with map data
fra_geo = fra |> 
  dplyr::mutate(CensusTract = as.character(CensusTract)) |> 
  dplyr::full_join(tracts, by = c("CensusTract" = "GEOID"))

# Load map data (counties)
counties = get_acs(state = "NC", 
                   geography = "county", 
                   county = piedmont_triad,
                   variables = "B19013_001",
                   geometry = TRUE, 
                   year = 2015) |> 
  dplyr::mutate(NAME = sub(pattern = " County, North Carolina", 
                           replacement = "", 
                           x = NAME))

# Merge food research atlas with map data
fra_summ_geo = fra_summ |> 
  dplyr::mutate(NAME = sub(" County", "", County)) |> 
  dplyr::full_join(counties)

############################################################################################
## MAPS AT TRACT AND COUNTY LEVELS /////////////////////////////////////////////////////////
############################################################################################
## 1. Low-access tracts at 0.5 miles (census tracts)
plot_tracts_half = fra_geo |> 
  mutate(LATracts_half = factor(x = LATracts_half, 
                                levels = c(0, 
                                           1, 
                                           NA), 
                                labels = c("Access", 
                                           "No Access", 
                                           "Missing"), 
                                exclude = NULL)) |>  
  ggplot() + 
  geom_sf(aes(fill = LATracts_half, geometry = geometry)) + 
  scale_fill_manual(values = c('#FFFFFF', '#FD7446', "grey"), 
                    name = "") +
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = c(0.2, 0.3), 
        plot.title = element_text(face = "bold", hjust = 0.5)) + 
  ggtitle(label = "Census Tract Food Access at 0.5 Miles")

## 2. Low-access tracts at 1.0 mile (census tracts)
plot_tracts_one = fra_geo |> 
  mutate(LATracts_half = factor(x = LATracts1, 
                                levels = c(0, 
                                           1, 
                                           NA), 
                                labels = c("Access", 
                                           "No Access", 
                                           "Missing"), 
                                exclude = NULL)) |>  
  ggplot() + 
  geom_sf(aes(fill = LATracts_half, geometry = geometry)) + 
  scale_fill_manual(values = c('#FFFFFF', '#FD7446', "grey"), 
                    name = "") +
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = c(0.2, 0.3), 
        plot.title = element_text(face = "bold", hjust = 0.5)) + 
  ggtitle(label = "Census Tract Food Access at 1.0 Mile")

## 3. Proportion of low-access tracts by county at 0.5 miles
plot_county_half = fra_summ_geo |> 
  ggplot() + 
  geom_sf(aes(fill = prop_LATracts_half, geometry = geometry)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
    rescaler = ~ scales::rescale_mid(.x, mid = state_avg$med_prop_LATracts_half),
    name = "Proportion of Low-Access Census Tracts", 
    guide = "none") +
  geom_sf_text(aes(label = paste0(NAME, "\n", round(prop_LATracts_half * 100), "%"), 
                   geometry = geometry)) + 
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "right", 
        plot.title = element_text(face = "bold", hjust = 0.5)) + 
  ggtitle(label = "0.5 Miles")

## 4. Proportion of low-access tracts by county at 1 mile
plot_county_one = fra_summ_geo |> 
  ggplot() + 
  geom_sf(aes(fill = prop_LATracts1, geometry = geometry)) + 
  scale_fill_gradientn(
    colours = colorRampPalette(c('#709AE1', '#FFFFFF', '#FD7446'))(100),
    rescaler = ~ scales::rescale_mid(.x, mid = state_avg$med_prop_LATracts1),
    name = "Proportion of Low-Access Census Tracts", 
    guide = "none") +
  geom_sf_text(aes(label = paste0(NAME, "\n", round(prop_LATracts1 * 100), "%"), 
                   geometry = geometry)) + 
  theme_void(base_size = 10) + 
  theme(plot.margin = margin(l=25, r=20, t=20, b=25),
        legend.position = "right", 
        plot.title = element_text(face = "bold", hjust = 0.5)) + 
  ggtitle(label = "1.0 Mile")

ggpubr::ggarrange(plot_county_half, plot_county_one)
