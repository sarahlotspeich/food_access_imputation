# Combining straight-line and map-based distances to investigate food access and health

This repository contains R code and simulation data to reproduce results from the manuscript by Lotspeich, Mullan, D'Agostino McGowan, and Hepler (2023+). These simulations rely on the `possum` package, which implements the multiple imputation approach for covariate measurement error in Poisson regression from the paper. The package can be found in its own repo [here](https://github.com/sarahlotspeich/possum), and installed in `R` as follows:

``` r
# Run once
# install.packages("devtools")
devtools::install_github("sarahlotspeich/possum", ref = "main")
```

## Tables 

**Table 1.** Simulation results under increasing severity of errors (i.e., increasing error standard deviation $\sigma_U$) in straight-line food access. Data for $N = 340$ neighborhoods were simulated in each setting, assuming 10-14% outcome prevalence and true log prevalence ratio $\beta_1 = 0.049$. 

  - [Script (Run Simulations Locally)](https://github.com/sarahlotspeich/food/blob/main/sims-scripts/sims_vary_sigmaU.R)
  - [Script (Run Simulations on a Cluster)](https://github.com/sarahlotspeich/food/blob/main/cluster-sims-scripts/sims_vary_sigmaU.R)
  - [Script (Make Table)](Table-Scripts/Table1-Gold-Standard.R)
  - [Data (Simulation Results)](Table-Data/data_Table1.csv)

**Table 2.** Simulation results with more neighborhoods with error-prone information (i.e., increasing proportion $q$ ) in straight-line food access. Data for $N = 340$ neighborhoods were simulated in each setting, assuming 10-14% outcome prevalence and true log prevalence ratio $\beta_1 = 0.049$. 

  - [Script (Run Simulations Locally)](https://github.com/sarahlotspeich/food/blob/main/sims-scripts/sims_vary_sigmaU.R)
  - [Script (Run Simulations on a Cluster)](https://github.com/sarahlotspeich/food/blob/main/cluster-sims-scripts/sims_vary_sigmaU.R)
  - [Script (Make Table)](Table-Scripts/Table1-Gold-Standard.R)
  - [Data (Simulation Results)](Table-Data/data_Table1.csv)

**Table S1.** Simulation results under increasing severity of errors (i.e., increasing error mean $\mu$) in straight-line food access. Data for $N = 340$ neighborhoods were simulated in each setting, assuming 10-14% outcome prevalence and true log prevalence ratio $\beta_1 = 0.049$. 

  - [Script (Run Simulations Locally)](https://github.com/sarahlotspeich/food/blob/main/sims-scripts/sims_vary_sigmaU.R)
  - [Script (Run Simulations on a Cluster)](https://github.com/sarahlotspeich/food/blob/main/cluster-sims-scripts/sims_vary_sigmaU.R)
  - [Script (Make Table)](Table-Scripts/Table1-Gold-Standard.R)
  - [Data (Simulation Results)](Table-Data/data_Table1.csv)  

## Figures 

**Figure 1.** Straight-line and map-based distances from Reynolda House (square symbol) to a nearby Food Lion grocery store (triangle symbol) in Winston-Salem, North Carolina.

  - [Figure](figures/fig1_map_comparing_distances.png)
  - [Script (Make Figure)](figure-scripts/fig1_map_comparing_distances.R)

