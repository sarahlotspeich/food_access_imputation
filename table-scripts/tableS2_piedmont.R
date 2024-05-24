# Load packages 
library(tableone) ## for summary table
library(dplyr) ## for data wrangling

# Load data from GitHub
## Analytical dataset for food access models
food_access = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/analysis_data.csv")

## Rural/urban continuum data (2010 release)
ruca = read.csv("https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/main/piedmont-triad-data/ruca2010revised.csv") |> 
  dplyr::mutate(PrimaryRUCA = factor(PrimaryRUCA, 
                                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 99), 
                                     labels = c("Metropolitan", "Metropolitan", "Metropolitan", "Micropolitan", "Micropolitan", "Micropolitan", 
                                                "Small town", "Small town", "Small town", "Rural", "Not coded")))

# Create Table S2 data 
dat = food_access |> 
  dplyr::left_join(ruca, 
                   by = dplyr::join_by(LocationID == StateCountyTract)) |> 
  dplyr::select(-LocationID, -SecondaryRUCA, Population, -County, -State,-StateCounty)
## Divide case counts by population to get prevalences
dat[, c(5:8)] = dat[, c(5:8)] / dat[, 4]
## Specify which variables should be treated as categorical for summary statistics
catVars = c("CountyName", "PrimaryRUCA")
table_S2 = CreateTableOne(data = dat, 
                          factorVars = catVars)

# Print Table S2
print(table_S2, 
      nonnormal = setdiff(colnames(table_S2), catVars))