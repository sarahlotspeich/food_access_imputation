# Combining straight-line and map-based distances to investigate food access and health

This repository contains R code and simulation data to reproduce results from the manuscript by Lotspeich, Mullan, D'Agostino McGowan, and Hepler (2023+). These simulations rely on the `possum` package, which implements the multiple imputation approach for covariate measurement error in Poisson regression from the paper. The package can be found in its own repo [here](https://github.com/sarahlotspeich/possum), and installed in `R` as follows:

``` r
# Run once
# install.packages("devtools")
devtools::install_github("sarahlotspeich/possum", ref = "main")
```

## Tables 

**Table 1.** Simulation results under increasing severity of errors in straight-line proximity to healthy foods, as controlled by the error standard deviation $\sigma_U$.

  - [Script (Run Simulations Locally)](sims-scripts/sims_vary_sigmaU.R)
  - [Script (Run Simulations on a Cluster)](sims-scripts/sims_vary_sigmaU_cluster.R)
  - [Script (Make Table)](table-scripts/table1_vary_sigmaU.R)
  - [Data (Simulation Results)](sims-data/vary_sigmaU_sims_combined.csv)

**Table 2.** Simulation results under increasing proportion of neighborhoods queried to obtain map-based proximity to healthy foods, as controlled by $p_V$.

  - [Script (Run Simulations Locally)](sims-scripts/sims_vary_pV.R)
  - [Script (Run Simulations on a Cluster)](sims-scripts/sims_vary_pV_cluster.R)
  - [Script (Make Table)](table-scripts/table2_vary_pV.R)
  - [Data (Simulation Results)](sims-data/vary_pV_sims_combined.csv)

**Table S1.** Simulation results under increasing severity of errors in straight-line proximity to healthy foods, as controlled by the error mean $\mu_U$.

  - [Script (Run Simulations Locally)](sims-scripts/sims_vary_muU.R)
  - [Script (Run Simulations on a Cluster)](sims-scripts/sims_vary_muU_cluster.R)
  - [Script (Make Table)](table-scripts/tableS1_vary_muU.R)
  - [Data (Simulation Results)](sims-data/vary_muU_sims_combined.csv)  

## Figures 

**Figure 1.** Straight-line and map-based distances from Reynolda House (square symbol) to a nearby Food Lion grocery store (triangle symbol) in Winston-Salem, North Carolina.

  - [Figure](figures/fig1_map_comparing_distances.png)
  - [Script (Make Figure)](figure-scripts/fig1_map_comparing_distances.R)

**Figure S1.** Estimated log prevalence ratios for food access $X_P$ on health. The five possible ways to include the analysis model outcome in the imputation model for $X_P$ were considered. All results are based on 1000 replications.

  - [Script (Run Simulations Locally)](https://github.com/sarahlotspeich/food/blob/main/sims-scripts/sims_incl_outcome.R)
  - [Script (Run Simulations on a Cluster)](https://github.com/sarahlotspeich/food/blob/main/sims-scripts/sims_incl_outcome_cluster.R)
  - [Figure](figures/figS1_inclY_in_imputation_model.png)
  - [Script (Make Figure)](figure-scripts/figS1_inclY_in_imputation_model.R)
