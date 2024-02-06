############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps
library(tidycensus) ## to get underlying census tract map shapes

# Set API keys (redacted to avoid violating use agreements)
## See ex_set_api_keys.R to set up your own script
source("set_api_keys.R")

############################################################################################
## GET DATA ////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Define counties in Forsyth & border
forsyth_border = c("forsyth", "davidson", "davie", "yadkin", "surry", "stokes", 
                   "rockingham", "guilford", "randolph")

## Load map data (census tracts)
tracts = get_acs(state = "NC", 
                 geography = "tract", 
                 county = forsyth_border,
                 variables = "B19013_001",
                 geometry = TRUE, 
                 year = 2010) |> 
  dplyr::mutate(GEOID = as.numeric(GEOID)) ### character --> numeric to merge into food access data
nrow(tracts) ## M = 340 neighborhoods

## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv(file = "~/Documents/food/forsyth-data/REVIEW_closest_healthy_foods_store_2022.csv") |> 
  dplyr::right_join(tracts, 
                    by = dplyr::join_by(LocationID == GEOID))

## 2010 rural/urban continuum codes (merge into health outcomes and food access)
ruca = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food/main/forsyth-data/ruca2010revised.csv") |> 
  dplyr::select(StateCountyTract, County)
food_access = food_access |> 
  dplyr::left_join(y = ruca, 
                   by = dplyr::join_by(LocationID == StateCountyTract))

## Define query indicators (= 1 if map-based measures are available, = 0 otherwise)
set.seed(918) ### make the sampling reproducible
queried_subset = food_access |> 
  dplyr::group_by(County) |> 
  dplyr::sample_n(size = 4, replace = FALSE) 
### Draw a county-stratified random sample of n = 36 census tracts to query
queried_subset |> 
  dplyr::pull(County) |> 
  table()
### Create column for queried X from complete-case
food_access = food_access |> 
  dplyr::mutate(dist_closest_map_cc = ifelse(test = LocationID %in% queried_subset$LocationID, 
                                             yes = dist_closest_map, 
                                             no = NA))

############################################################################################
## GET PREDICTIONS FROM IMPUTATION /////////////////////////////////////////////////////////
############################################################################################
# Fit imputation model (linear regression, without Y)
imp_mod = lm(formula = dist_closest_map ~ dist_closest_straight, 
             data = queried_subset) ## R^2 = 0.9267

# Extract means for each census tract from imputation model
mu = with(imp_mod, 
          coefficients[1] + coefficients[2] * food_access$dist_closest_straight)

# Multiple imputation
## Set number of imputations 
B = 20

## Create matrix to hold B imputed values for each census tract
imp_proximity = matrix(data = NA, 
                       nrow = nrow(food_access), 
                       ncol = B)
for (b in 1:B) {
  ### Draw  imputed values from distribution (based on imputation model)
  food_access$imp_X = rnorm(n = nrow(food_access),
                            mean = mu, 
                            sd = sigma(imp_mod))
  
  ### Replace non-missing X values 
  food_access[!is.na(food_access$dist_closest_map_cc), "imp_X"] = food_access[!is.na(food_access$dist_closest_map_cc), "dist_closest_map"]
  
  ### Save parameters
  imp_proximity[, b] = as.vector(food_access$imp_X)
}
mean_imp_proximity = rowMeans(imp_proximity)
summary(mean_imp_proximity)

### Create column for queried X from complete-case
food_access = food_access |> 
  dplyr::mutate(dist_closest_imp = mean_imp_proximity)

############################################################################################
## MAKE MAP OF OBSERVED/PREDICTED PROXIMITY ////////////////////////////////////////////////
############################################################################################
food_access |> 
  dplyr::select(LocationID, geometry, 
                dist_closest_straight, dist_closest_map, dist_closest_map_cc, dist_closest_imp) |> 
  tidyr::gather(key = "Analysis", value = "Proximity", -c(1:2)) |> 
  dplyr::mutate(Analysis = factor(x = Analysis, 
                                  levels = c("dist_closest_straight", "dist_closest_map", "dist_closest_map_cc", "dist_closest_imp"), 
                                  labels = c("Naive", "Gold Standard", "Complete Case", "Imputation"))) |> 
  ggplot() + 
  geom_sf(aes(fill = Proximity, geometry = geometry)) + 
  scale_fill_viridis_c(option = "magma", 
                       name = "Proximity to Healthy Foods (in Miles):", 
                       guide = guide_colourbar(direction = "horizontal",
                                               barwidth = 10, barheight = 1)) +
  theme_void() + 
  theme(legend.position = "bottom") + 
  facet_wrap(~ Analysis, nrow = 2)
ggsave(filename = "~/Downloads/fig3_map_proximity.png", 
       device = "png", 
       width = 5, 
       height = 5, 
       units = "in")