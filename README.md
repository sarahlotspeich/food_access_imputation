# Combining straight-line and map-based distances to investigate food access and health

This repository contains R code and simulation data to reproduce results from the manuscript by Lotspeich, Mullan, D'Agostino McGowan, and Hepler (2023+). 

The `possum` package, which implements the multiple imputation approach for covariate measurement error in Poisson regression from the paper, can be found in its own repo [here](https://github.com/sarahlotspeich/possum). 

``` r
# Run once
# install.packages("devtools")
devtools::install_github("sarahlotspeich/glmCensRd", ref = "main")
```

Each of the "Script (Run Simulations)" files is coded to run 100 replications of each setting for demonstration. Per the NOTES at the bottom of the scripts, some more time-intensive simulations were run in parallel.

## Tables 

**Table 1.** Simulation results for Weibull $X$ from the full cohort analysis and imputation approaches using the true survival function and adaptive quadrature versus the trapezoidal rule.

  - [Script (Run Simulations)](Sim-Scripts/Table1-Gold-Standard.R)
  - [Script (Make Table)](Table-Scripts/Table1-Gold-Standard.R)
  - [Data (Simulation Results)](Table-Data/data_Table1.csv)  

**Table 2.** Simulation results for Weibull $X$ from the full cohort analysis and imputation approaches using the estimated survival function and adaptive quadrature versus the trapezoidal rule.

  - [Script (Run Simulations)](Sim-Scripts/Table2-Estimated-WeibullX.R)
  - [Script (Make Table)](Table-Scripts/Table2-Estimated-WeibullX.R)
  - [Data (Simulation Results)](Table-Data/data_Table2.csv)  

**Table S1.** Simulation results for Weibull $X$ from the full cohort analysis and imputation approaches using the true survival function, assuming that $X$ was independent of $Z$.

  - [Script (Run Simulations)](Sim-Scripts/TableS1-Gold-Standard-XindepZ.R)
  - [Script (Make Table)](Table-Scripts/TableS1-Gold-Standard-XindepZ.R)
  - [Data (Simulation Results)](Table-Data/data_TableS1.csv)  

**Table S2.** Simulation results for Weibull $X$ from the full cohort analysis and imputation approaches using the estimated survival function, assuming that $X$ was independent of $Z$.

  - [Script (Run Simulations)](Sim-Scripts/TableS2-Estimated-WeibullX-XindepZ.R)
  - [Script (Make Table)](Table-Scripts/TableS2-Estimated-WeibullX-XindepZ.R)
  - [Data (Simulation Results)](Table-Data/data_TableS2.csv)  

**Table S3.** Simulation results for log-normal $X$ from the full cohort analysis and imputation approaches using the estimated survival function and adaptive quadrature versus the trapezoidal rule.

  - [Script (Run Simulations)](Sim-Scripts/TableS3-Estimated-LogNormalX.R)
  - [Script (Make Table)](Table-Scripts/TableS3-Estimated-LogNormalX.R)
  - [Data (Simulation Results)](Table-Data/data_TableS3.csv)  

## Figures 

**Figure S1.** Illustration of the four extrapolation methods for a step survival function $\widehat{S}(t)$ in simulated data.

  - [Script (Make Figure)](Figure-Scripts/FigureS1-Illustrate-Extrapolation-Methods.R)

**Figure S2.** We explored light ($\sim 17\%$), heavy ($\sim 49\%$), and extra heavy ($\sim 82\%$) censoring in Weibull $X$, induced by generating $C$ from an exponential distribution with rates $= 0.5$, $2.9$, and $20$, respectively.

  - [Script (Make Figure)](Figure-Scripts/FigureS2-Percent-Censored.R)
  
**Figure S3.** With Weibull $X$, extrapolating Breslow's estimator $\widehat{S}_0(t)$ beyond the largest uncensored value $\widetilde{X}$ with the Weibull extension offered the lowest bias and best efficiency for $\hat{\beta}$ in conditional mean imputation with adaptive quadrature.

  - [Script (Run Simulations)](Sim-Scripts/FigureS3-Extrapolation-Methods-Weibull.R)
  - [Script (Make Figure)](Figure-Scripts/FigureS3-Extrapolation-Methods-Weibull.R)
  - [Data (Simulation Results)](Figure-Data/data_FigureS3.csv)  

**Figure S4.** With log-normal $X$, extrapolating Breslow's estimator $\widehat{S}_0(t)$ beyond the largest uncensored value $\widetilde{X}$ with any of the three extrapolation methods offered similar bias and efficiency for $\hat{\beta}$ in conditional mean imputation with adaptive quadrature.

  - [Script (Run Simulations)](Sim-Scripts/FigureS4-Extrapolation-Methods-Log-Normal.R)
  - [Script (Make Figure)](Figure-Scripts/FigureS4-Extrapolation-Methods-Log-Normal.R)
  - [Data (Simulation Results)](Figure-Data/data_FigureS4.csv)  

**Figure S5.** Interpolating Breslow's estimator $\widehat{S}_0(t)$ between uncensored values with either of the two interpolation methods offered similar bias and efficiency for $\hat{\beta}$ in conditional mean imputation with adaptive quadrature. 

  - [Script (Run Simulations)](Sim-Scripts/FigureS5-Interpolation-Methods.R)
  - [Script (Make Figure)](Figure-Scripts/FigureS5-Interpolation-Methods.R)
  - [Data (Simulation Results)](Figure-Data/data_FigureS5.csv)  

**Figure S6.** Extrapolating Breslow's estimator $\widehat{S}_0(t)$ beyond the largest uncensored value $\widetilde{X}$ with any of the three extrapolation methods offered similar bias and efficiency for $\hat{\beta}$ in conditional mean imputation with the trapezoidal rule.

  - [Script (Run Simulations)](Sim-Scripts/FigureS6-Extrapolation-Methods-Trapezoidal-Rule.R)
  - [Script (Make Figure)](Figure-Scripts/FigureS6-Extrapolation-Methods-Trapezoidal-Rule.R)
  - [Data (Simulation Results)](Figure-Data/data_FigureS6.csv)  

**Figure S7.** When using the estimated survival function $\widehat{S}(x|z)$, conditional mean imputation with adaptive quadrature could be biased under severe censoring (e.g., $> 82\%$). This residual bias seemed to stem from the estimated survival function, since we saw virtually no bias across these same settings when using the true survival function $S(x|z)$ instead.

  - [Script (Make Figure)](Figure-Scripts/FigureS7-Percent-Censored-vs-Bias.R) 

Note: Figure S7 uses the data from **Tables 1 and 2**. (See above for details.) 

**Figure S8.** Due to the Weibull distribution's skewness, higher censoring rates led to smaller values of $W_{(n)}$ (the maximum of the observed covariate), which led to worse performance (i.e., higher bias) when calculating the conditional mean with the trapezoidal rule.

  - [Script (Make Figure)](Figure-Scripts/FigureS8-Weibull-vs-Log-Normal.R) 
