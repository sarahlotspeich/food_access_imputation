# Load data 
## (1) Health outcomes from 2022 PLACES data
health = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/disease_prevalences_2022.csv")

## (2) Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv(file = "~/Documents/food/forsyth-data/REVIEW_closest_healthy_foods_store_2022.csv") |> 
  dplyr::bind_rows(
    read.csv(file = "~/Documents/food/piedmont-triad-data/REVIEW_closest_healthy_foods_store_2022.csv")
  ) |> 
  dplyr::right_join(health, 
                    by = dplyr::join_by(LocationID == TractFIPS))

## Remove additional food access columns (not needed for primary analysis)
food_access = food_access |> 
  dplyr::select(LocationID, CountyName, dist_closest_straight, dist_closest_map, 
                POP, BPHIGH, CHD, DIABETES, OBESITY)

## Order by Location ID
food_access = food_access |> 
  dplyr::arrange(LocationID) 

## Define query indicators (= 1 if map-based measures are available, = 0 otherwise)
set.seed(918) ### make the sampling reproducible
n = 4 ### set number of census tracts sampled from each county to be queried
queried_subset = food_access |> 
  dplyr::group_by(CountyName) |> 
  dplyr::sample_n(size = n, replace = FALSE) 

### Create column for queried X from complete-case
food_access = food_access |> 
  dplyr::mutate(dist_closest_map_cc = ifelse(test = LocationID %in% queried_subset$LocationID, 
                                             yes = dist_closest_map, 
                                             no = NA))

food_access |> 
  dplyr::mutate(CountyName = factor(x = CountyName, 
                                    levels = c("Alamance", "Caswell", "Davidson", "Davie",
                                               "Forsyth", "Guilford", "Montgomery", "Randolph",
                                               "Rockingham", "Stokes", "Surry", "Yadkin" ), 
                                    labels = LETTERS[1:12])) |> 
  dplyr::rename(Xstar = dist_closest_straight, 
                X_full = dist_closest_map, 
                X_partial = dist_closest_map_cc,
                O_POP = POP, 
                Y_BPHIGH = BPHIGH,
                Y_CHD = CHD,
                Y_DIABETES = DIABETES,
                Y_OBESITY = OBESITY) |> 
  write.csv("~/Documents/food/piedmont-triad-data/analysis_data.csv", 
            row.names = FALSE)