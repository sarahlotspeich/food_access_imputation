############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("food_access_imputation/set_api_keys.R")

############################################################################################
## LOAD MAP DATA FOR FORSYTH AND BORDERING COUNTIES' CENSUS TRACTS /////////////////////////
## To align with CDC Places dataset, use 2015 census geometry. /////////////////////////////
############################################################################################
# County outlines
counties = get_acs(state = "NC", 
                   geography = "county", 
                   variables = "B19013_001",
                   geometry = TRUE, 
                   year = 2015) |> 
  dplyr::mutate(NAME = sub(pattern = " County, North Carolina", 
                           replacement = "", 
                           x = NAME))

############################################################################################
## DEFINE SUBSETS OF COUNTIES USED FOR THE ANALYSIS OR DATA COLLECTION /////////////////////
############################################################################################
# Define Piedmont Triad counties
piedmont_triad = c("SURRY", "STOKES", "ROCKINGHAM", "CASWELL", 
                   "YADKIN", "FORSYTH", "GUILFORD", "ALAMANCE", 
                   "DAVIE", "DAVIDSON", "RANDOLPH", "MONTGOMERY")

# Define bordering counties (used for additional potential healthy food retailers)
border_counties = c("PERSON", "ORANGE", "CHATHAM", "MOORE", "STANLY", "CABARRUS", 
                    "ROWAN", "IREDELL", "WILKES", "ALLEGHANY", "RICHMOND", "ANSON")

# Define Piedmont Triad / Border Categories
counties = counties |> 
  dplyr::mutate(Category = dplyr::case_when(toupper(NAME) %in% piedmont_triad ~ "Piedmont Triad", 
                                            toupper(NAME) %in% border_counties ~ "Bordering Piedmont Triad", 
                                            .default = "Rest of NC"), 
                Category = factor(x = Category, 
                                  levels = c("Piedmont Triad", "Bordering Piedmont Triad", "Rest of NC")))
## Inspect counts per category 
table(counties$Category) ## 12 bordering, 12 Piedmont Triad, 76 rest of NC 

############################################################################################
## MAPS OF COUNTIES ////////////////////////////////////////////////////////////////////////
############################################################################################
ggplot() + 
  geom_sf(data = counties, 
          aes(geometry = geometry, 
              fill = Category), 
          color = "black") +   
  ggrepel::geom_label_repel(
    data = counties, 
    aes(label = NAME, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0, 
    max.overlaps = 50) + 
  theme_void(base_size = 8) + 
  theme(plot.margin = margin(0, 0, 0, 0), 
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.25), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.justification = "left")

# //////////////////////////////////////////////////////////////////////
# Save as 10" wide x 5" tall ////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
ggsave(filename = "figures/rev_figS4_map_piedmont_triad.png",
       device = "png",
       width = 12,
       height = 6,
       units = "in")
ggsave(filename = "figures/rev_figS4_map_piedmont_triad.pdf",
       device = "pdf",
       width = 12,
       height = 6,
       units = "in")
