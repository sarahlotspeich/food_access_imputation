################################################################################
# SETUP ########################################################################
################################################################################
## Load libraries
library(dplyr) ## to wrangle data

# Load data 
## (1) Health outcomes from 2022 PLACES data
health = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/disease_prevalences_2022.csv")

## (2) Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/review_proximity_healthy_foods.csv") |> 
  right_join(health, 
             by = join_by(LocationID == TractFIPS))

## (3) Population density from 2010 Rural-Urban Commuting Area (RUCA)
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/ruca2010revised.csv") |> 
  select(StateCountyTract, PopulationDensity) |> 
  right_join(food_access, 
             by = join_by(StateCountyTract == LocationID)) |> 
  rename(LocationID = StateCountyTract)

## (4) Socioeconomic factors from 2010 American Community Survey
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/piedmont_triad_acs_data.csv") |> 
  select(GEOID, INCOME, PERC_SNAP, PERC_CAR, PERC_INSURED) |> 
  right_join(food_access, 
             by = join_by(GEOID == LocationID))

## Remove additional food access columns (not needed for primary analysis)
food_access = food_access |> 
  dplyr::select(GEOID, CountyName, 
                dist_closest_straight, dist_closest_map, 
                comp_time_straight, dist_straight_q20, comp_time_map, 
                POP, BPHIGH, CHD, DIABETES, OBESITY, 
                PopulationDensity, PrimaryRUCA, Metropolitan, 
                INCOME, PERC_SNAP, PERC_CAR, PERC_INSURED)

## Order by Location ID
food_access = food_access |> 
  arrange(GEOID) 

## Define query indicators (= 1 if map-based measures are available, = 0 otherwise)
set.seed(918) ### make the sampling reproducible
n = 4 ### set number of census tracts sampled from each county to be queried
queried_subset = food_access |> 
  group_by(CountyName) |> 
  sample_n(size = n, replace = FALSE) 

### Create column for queried X from complete-case
food_access = food_access |> 
  mutate(dist_closest_map_cc = ifelse(test = GEOID %in% queried_subset$GEOID, 
                                      yes = dist_closest_map, 
                                      no = NA))

# Save data 
food_access |> 
  mutate(CountyName = factor(x = CountyName, 
                             levels = c("Alamance", "Caswell", "Davidson", "Davie",
                                        "Forsyth", "Guilford", "Montgomery", "Randolph",
                                        "Rockingham", "Stokes", "Surry", "Yadkin" ), 
                             labels = LETTERS[1:12])) |> 
  dplyr::rename(Xstar = dist_closest_straight, 
                Xstar_time = comp_time_straight,
                Xstar_q20 = dist_straight_q20,
                X_full = dist_closest_map, 
                X_time = comp_time_map, 
                X_partial = dist_closest_map_cc,
                O_POP = POP, 
                Y_BPHIGH = BPHIGH,
                Y_CHD = CHD,
                Y_DIABETES = DIABETES,
                Y_OBESITY = OBESITY, 
                POP_DENS = PopulationDensity, 
                RUCA = PrimaryRUCA, 
                METRO = Metropolitan) |> 
  write.csv("food_access_imputation/piedmont-triad-data/analysis_data.csv", 
            row.names = FALSE)
