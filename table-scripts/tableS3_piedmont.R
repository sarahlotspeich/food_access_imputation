# Load packages 
library(tableone) ## for summary table
library(dplyr) ## for data wrangling

# Load data from GitHub
## Analytical dataset for food access models
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/analysis_data.csv") |> 
  dplyr::mutate(Prev_DIABETES = Y_DIABETES / O_POP, 
                Prev_OBESITY = Y_OBESITY / O_POP) |> 
  dplyr::select(GEOID, CountyName, Prev_DIABETES, Prev_OBESITY, X_full, Xstar)

## Rural/urban commuting areas (RUCA) data (2010 release)
ruca = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/ruca2010revised.csv") |> 
  dplyr::mutate(PrimaryRUCA = factor(PrimaryRUCA, 
                                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 99), 
                                     labels = c("Metropolitan", "Metropolitan", "Metropolitan", "Micropolitan", "Micropolitan", "Micropolitan", 
                                                "Small town", "Small town", "Small town", "Rural", "Not coded")))

## Race breakdown (according to the ACS)
race = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/piedmont_triad_acs_data.csv") |> 
  dplyr::select(-NAME, -INCOME, -PERC_POVERTY, -PERC_SNAP, -PERC_CAR, -PERC_PUBLIC_TRANSIT, 
                -PERC_INSURED, -PERC_COLLEGE, -PERC_FEM_HEAD)

# Create Table S2 data 
dat = food_access |> 
  dplyr::left_join(ruca, 
                   by = dplyr::join_by(GEOID == StateCountyTract)) |> 
  dplyr::left_join(race, 
                   by = dplyr::join_by(GEOID == GEOID)) |> 
  dplyr::select(-GEOID, -SecondaryRUCA, Population, -County, -State,-StateCounty)
## Specify which variables should be treated as categorical for summary statistics
catVars = c("CountyName", "PrimaryRUCA")
table_S2 = CreateTableOne(data = dat, 
                          factorVars = catVars)

# Print Table S2
print(table_S2, 
      nonnormal = setdiff(colnames(dat), catVars))
