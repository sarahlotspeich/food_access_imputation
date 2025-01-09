# Note: This script is called by fig4_forest_plot_piedmont.R, and it
## assumes that a number of packages, scripts, and data have already been loaded in 

## Imputation Model Fits 
imp_diab = lm(formula = X_partial ~ METRO * Xstar + log(Y_DIABETES), 
              data = food_access)
imp_obes = lm(formula = X_partial ~ METRO * Xstar + log(Y_OBESITY), 
              data = food_access)

## Imputation Analysis
### Non-Spatial Models
mod_diab = impPossum(imputation_formula = X_partial ~ METRO * Xstar + log(Y_DIABETES), 
                     analysis_formula = Y_DIABETES ~ METRO * X_partial + offset(log(O_POP)), 
                     data = food_access, 
                     B = 20)
mod_obes = impPossum(imputation_formula = X_partial ~ METRO * Xstar + log(Y_OBESITY), 
                     analysis_formula = Y_OBESITY ~ METRO * X_partial + offset(log(O_POP)), 
                     data = food_access, 
                     B = 20)

# Save results for forest plot
imp_res = mod_diab |> 
  dplyr::mutate(Analysis = "Imputation",
                Outcome = "Diagnosed Diabetes", 
                Spatial = FALSE, 
                LB = exp(Estimate - 1.96 * Standard.Error), 
                UB = exp(Estimate + 1.96 * Standard.Error), 
                Estimate = exp(Estimate)) |> 
  dplyr::rename(Est = Estimate) |> 
  dplyr::select(Analysis, Outcome, Spatial, Est, LB, UB, Coefficient) |> 
  dplyr::bind_rows(
    mod_obes |> 
      dplyr::mutate(Analysis = "Imputation",
                    Outcome = "Obesity", 
                    Spatial = FALSE, 
                    LB = exp(Estimate - 1.96 * Standard.Error), 
                    UB = exp(Estimate + 1.96 * Standard.Error), 
                    Estimate = exp(Estimate)) |> 
      dplyr::rename(Est = Estimate) |> 
      dplyr::select(Analysis, Outcome, Spatial, Est, LB, UB, Coefficient)
  )
  
# Model 1a: Diagnosed diabetes among adults aged >=18 years 
## Predictor X = proximity to healthy foods based on map-based distance
mod_diab = impPossum(imputation_formula = X_partial ~ METRO * Xstar + log(Y_DIABETES), 
                     analysis_formula = Y_DIABETES ~ METRO * X_partial + adjacency(1 | GEOID) + offset(log(O_POP)), 
                     data = food_access, 
                     adjMatrix = ptW,
                     B = 20)

# Model 2a: Obesity among adults aged >=18 years
## Predictor X = proximity to healthy foods based on map-based distance
mod_obes = impPossum(imputation_formula = X_partial ~ METRO * Xstar + log(Y_OBESITY), 
                     analysis_formula = Y_OBESITY ~ METRO * X_partial + adjacency(1 | GEOID) + offset(log(O_POP)), 
                     data = food_access, 
                     adjMatrix = ptW,
                     B = 20)

# Save results for forest plot
imp_res = mod_diab |> 
  dplyr::mutate(Analysis = "Imputation",
                Outcome = "Diagnosed Diabetes", 
                Spatial = TRUE, 
                LB = exp(Estimate - 1.96 * Standard.Error), 
                UB = exp(Estimate + 1.96 * Standard.Error), 
                Estimate = exp(Estimate)) |> 
  dplyr::rename(Est = Estimate) |> 
  dplyr::select(Analysis, Outcome, Spatial, Est, LB, UB, Coefficient) |> 
  dplyr::bind_rows(
    mod_obes |> 
      dplyr::mutate(Analysis = "Imputation",
                    Outcome = "Obesity", 
                    Spatial = TRUE, 
                    LB = exp(Estimate - 1.96 * Standard.Error), 
                    UB = exp(Estimate + 1.96 * Standard.Error), 
                    Estimate = exp(Estimate)) |> 
      dplyr::rename(Est = Estimate) |> 
      dplyr::select(Analysis, Outcome, Spatial, Est, LB, UB, Coefficient)
  ) |> 
  dplyr::bind_rows(imp_res)