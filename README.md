# Combining straight-line and map-based distances to investigate the connection between proximity to healthy foods and disease

This repository contains R code and simulation data to reproduce results from the manuscript by Lotspeich, Mullan, D'Agostino McGowan, and Hepler (2023+). 

These simulations rely on the `possum` package, which implements the multiple imputation approach for covariate measurement error in Poisson regression from the paper. The package can be found in its own repo [here](https://github.com/sarahlotspeich/possum) and installed in `R` as follows:

``` r
# Run once
# install.packages("devtools")
devtools::install_github("sarahlotspeich/possum", ref = "main")
```

## Primary Data Source (Analysis)

**1.  Merged Food Access Measures and Disease Prevalence** created using the **Healthy Food Store Locations**, **Disease Prevalence by Neighborhood (Census Tract)**, and **Neighborhood (Census Tract) Population Centers**. 

  - [Script (Calculate Proximity)](piedmont-triad-data/raw_proximity_healthy_foods.R)
  - *Note:* A small number of census tract population centers could not be geocoded based on the original address. They were manually reviewed and assigned the nearest address that *would* geocode, and then their proximity to healthy foods was recalculated. See the [raw data](piedmont-triad-data/raw_proximity_healthy_foods.csv) and [Script](review_proximity_healthy_foods.R) for more details. 
  - [Data (Proximity)](piedmont-triad-data/review_proximity_healthy_foods.csv)
  - [Data (Food Access + Health)](piedmont-triad-data/analysis_data.csv)

**2.  Healthy Foods Store Locations** taken from the United States Department of Agriculture’s (UDSA’s) Historical Supplemental Nutrition Assistance Program (SNAP) Retail Locator Data (2022 Release).

  - [Download Data](https://www.fns.usda.gov/snap/retailer-locator)
  - [Script (Filter Data)](forsyth-data/healthy_foods_stores_2022.R)
  - [Data (Filtered Data)](forsyth-data/healthy_foods_stores_2022.csv)

**3.  Disease Prevalence by Neighborhood (Census Tract)** taken from the United States Centers for Disease Control and Prevention (CDC’s) PLACES Dataset (2022 Release). 

  - [Download Data](https://data.cdc.gov/500-Cities-Places/PLACES-Census-Tract-Data-GIS-Friendly-Format-2022-/shc3-fzig/data)
  - [Script (Filter Data)](piedmont-triad-data/disease_prevalences_2022.R)
  - [Data (Filtered Data)](piedmont-triad-data/disease_prevalences_2022.csv)

**4.  Neighborhood (Census Tract) Population Centers** taken from the United States Census Bureau (2010 Release). 

  - [Download Data](https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR37.txt)

## Secondary Data Sources (Descriptive)

**5.  Rural-Urban Commuting Areas (RUCA)** taken from the United States Department of Agriculture (2010 Release).

  -  [Download Data](https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/documentation/)

**6.  American Community Survey (ACS)** taken from the United States Census Bureau (2015 Release). Data were extracted using the [`tidycensus`](https://walker-data.com/tidycensus/) package. 

  - [Script (Build Data)](piedmont-triad-data/piedmont_triad_acs_data.R)
  - [Data](piedmont-triad-data/piedmont_triad_acs_data.csv)

## Tables 

**Table 1.** Simulation results under increasing severity of errors in straight-line proximity to healthy foods, as controlled by the error standard deviation $\sigma_U$.

  - [Script (Run Simulations Locally)](sims-scripts/sims_vary_sigmaU.R)
  - [Script (Make Table)](table-scripts/table1_vary_sigmaU.R)
  - [Data (Simulation Results)](sims-data/vary_sigmaU_sims_combined.csv)

**Table 2.** Simulation results under increasing proportion of neighborhoods queried to obtain map-based proximity to healthy foods, as controlled by $p_V$.

  - [Script (Run Simulations Locally)](sims-scripts/sims_vary_pV.R)
  - [Script (Make Table)](table-scripts/table2_vary_pV.R)
  - [Data (Simulation Results)](sims-data/vary_pV_sims_combined.csv)

**Table 3.** Simulation results under higher disease prevalence and prevalence ratios for map-based proximity to healthy foods, as controlled by the coefficients $\beta_0$ and $\beta_1$, respectively.

  - [Script (Run Simulations Locally)](sims-scripts/sims_vary_prev.R)
  - [Script (Make Table)](table-scripts/table3_vary_prev.R)
  - [Data (Simulation Results)](sims-data/vary_prev_sims_combined.csv)

**Table 4.** Simulation results under increasingly severe multiplicative errors in straight-line proximity to healthy foods, as controlled by the max of the error distribution $\tau_W$. 

  - [Script (Run Simulations Locally)](sims-scripts/sims_mult_error.R)
  - [Script (Make Table)](table-scripts/table4_mult_error.R)
  - [Data (Simulation Results)](sims-data/mult_error_sims_combined.csv)

**Table S1.** Simulation results under increasing severity of errors in straight-line proximity to healthy foods, as controlled by the error mean $\mu_U$.

  - [Script (Run Simulations Locally)](sims-scripts/sims_vary_muU.R)
  - [Script (Make Table)](table-scripts/tableS1_vary_muU.R)
  - [Data (Simulation Results)](sims-data/vary_muU/)

**Table S2.** Descriptive statistics of the $N = 387$ census tracts in the Piedmont Triad, North Carolina.

  - [Script (Make Table)](table-scripts/tableS2_piedmont.R)
  - [Data (Rural/Urban)](piedmont-triad-data/ruca2010revised.csv)
  - [Data (Food Access + Health)](piedmont-triad-data/analysis_data.csv)

## Figures 

**Figure 1.** Choropleth map of food access, as measured by proximity to healthy foods for each neighborhood (census tract) in the Piedmont Triad, North Carolina, according to the data used for the naive, gold standard, imputation, and complete-case analyses.

  - [Data (Food Access + Health)](piedmont-triad-data/analysis_data.csv)
  - [Figure](figures/fig1_map_proximity_piedmont.png)
  - [Script (Make Figure)](figure-scripts/fig1_map_piedmont_proximity.R)

**Figure 2.** Choropleth maps of the crude prevalence of diagnosed diabetes and obesity for census tracts in the Piedmont Triad, North Carolina.

  - [Data (PLACES)](piedmont-triad-data/disease_prevalences_2022.csv)
  - [Figure](figures/fig2_map_piedmont_triad_health_outcomes.png)
  - [Script (Make Figure)](figure-scripts/fig2_map_piedmont_triad_health_outcomes.R)

**Figure 3.** Scatter plot of straight-line versus map-based proximity to healthy foods store for neighborhoods in the Piedmont Triad, North Carolina using the fully-queried data ($N = 387$).

  - [Data (PLACES)](piedmont-triad-data/disease_prevalences_2022.csv)
  - [Figure](figures/fig3_scatterplot_proximity_piedmont.png)
  - [Script (Make Figure)](figure-scripts/fig3_scatterplot_proximity_piedmont.R)

**Figure S1.** Straight-line and map-based distances from Reynolda House (square symbol) to a nearby Food Lion grocery store (triangle symbol) in Winston-Salem, North Carolina. 

  - [Figure](figures/figS1_map_comparing_distances.png)
  - [Script (Make Figure)](figure-scripts/figS1_map_comparing_distances.R)

**Figure S2.** Line graph of the cumulative computing time (in seconds) for the map-based versus straight-line distance calculations in the Piedmont Triad data. 

  - [Data (Food Access + Health)](piedmont-triad-data/analysis_data.csv)
  - [Figure](figures/figS2_cum_comp_time_line.png)
  - [Script (Make Figure)](figure-scripts/figS2_cum_comp_time_line.R)

**Figure S3.** Estimated prevalence ratios for food access $X$ on health using multiple imputation. The five possible ways to include the analysis model outcome... in the imputation model for $X$ were considered. 

  - [Script (Run Simulations Locally)](sims-scripts/sims_incl_outcome.R)
  - [Data (Simulation Results)](sims-data/include_outcome/)
  - [Figure](figures/figS3_incl_in_imputation_model_PR.png)
  - [Script (Make Figure)](figure-scripts/figS3_inclY_in_imputation_model.R)

**Figure S4.** Choropleth maps of socioeconomic factors across the census tracts of the Piedmont Triad, North Carolina. 

  - [Data (RUCA)](piedmont-triad-data/ruca2010revised.csv)
  - [Data (ACS)](piedmont-triad-data/piedmont_triad_acs_data.csv)
  - [Figure](figures/figS4_map_piedmont_triad_acs_data.png)
  - [Script (Make Figure)](figure-scripts/figS4_map_piedmont_triad_acs_data.R)

**Figure S5.** Map of $M = 701$ authorized SNAP retailers (as of 2022) in the Piedmont Triad, North Carolina, broken down by store type.

  - [Data (SNAP)](piedmont-triad-data/healthy_foods_stores_2022.csv)
  - [Figure](figures/figS5_map_piedmont_triad_SNAP_wide.png)
  - [Script (Make Figure)](figures/figS5_map_SNAP.R)

**Figure S6.** Map of census tracts in the Piedmont Triad, North Carolina, colored according to whether it was treated as queried in the partially queried analysis. 

  - [Data (Food Access + Health)](piedmont-triad-data/analysis_data.csv)
  - [Figure](figures/figS6_map_piedmont_queried.png)
  - [Script (Make Figure)](figure-scripts/figS6_map_piedmont_queried.R)

**Figure S7.** Choropleth maps of the crude prevalence of adverse health outcomes for census tracts in Forsyth County (top row) and Guilford County (bottom row), North Carolina. 

  - [Data (PLACES)](piedmont-triad-data/disease_prevalences_2022.csv)
  - [Figure](figures/figS7_map_forsyth_guilford_health_outcomes.png)
  - [Script (Make Figure)](figure-scripts/figS7_map_forsyth_guilford_health_outcomes.R)

**Figure S8.** Histogram of additive errors ($U$) and multiplicative errors ($W$) in straight-line proximity to healthy foods ($X^*$) from the fully queried data ($N = 387$) for the Piedmont Triad, North Carolina.

  - [Data (Food Access + Health)](piedmont-triad-data/analysis_data.csv)
  - [Figure](figures/figS8_histogram_errors_piedmont.png)
  - [Script (Make Figure)](figure-scripts/figS8_histogram_errors_proximity.R)
