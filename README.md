# Combining straight-line and map-based distances to investigate food access and health

This repository contains R code and simulation data to reproduce results from the manuscript by Lotspeich, Mullan, D'Agostino McGowan, and Hepler (2023+). 

The `possum` package, which implements the multiple imputation approach for covariate measurement error in Poisson regression from the paper, can be found in its own repo [here](https://github.com/sarahlotspeich/possum). 

``` r
# Run once
# install.packages("devtools")
devtools::install_github("sarahlotspeich/possum", ref = "main")
```

Each of the "Script (Run Simulations)" files is coded to run 10 replications of each setting for demonstration. Per the NOTES at the bottom of the scripts, some more time-intensive simulations were run in parallel.

## Tables 

**Table 1.** Simulation results under increasing severity of errors in straight-line food access. Data for $N = 340$ neighborhoods were simulated in each setting, assuming 10-14% outcome prevalence and true log prevalence ratio $\beta_1 = 0.049$. 

  - [Script (Run Simulations)](Sim-Scripts/Table1-Gold-Standard.R)
  - [Script (Make Table)](Table-Scripts/Table1-Gold-Standard.R)
  - [Data (Simulation Results)](Table-Data/data_Table1.csv)  

## Figures 

**Figure 1.** Straight-line and map-based distances from Reynolda House (square symbol) to a nearby Food Lion grocery store (triangle symbol) in Winston-Salem, North Carolina.

  - [Figure](figures/fig1_map_comparing_distances.png)
  - [Script (Make Figure)](figure-scripts/fig1_map_comparing_distances.R)
