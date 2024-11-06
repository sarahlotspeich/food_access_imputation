# Note: This script is called by fig4_forest_plot_piedmont.R, and it
## assumes that a number of packages, scripts, and data have already been loaded in 

# Functions to pull out of coefficient summary
est = function(i, fit) {
  round(exp(fit$Estimate[i]), 3)
}
lb = function(i, fit) {
  se = fit$Standard.Error[i]
  round(exp(fit$Estimate[i] - 1.96 * se), 3)
}
ub = function(i, fit) {
  se = fit$Standard.Error[i]
  round(exp(fit$Estimate[i] + 1.96 * se), 3)
}
ci = function(i, fit) {
  se = fit$Standard.Error[i]
  paste(round(exp(fit$Estimate[i] + c(-1.96, 1.96) * se), 3), collapse = ", ")
}

## Imputation Analysis
### Non-Spatial Models
mod_diab = impPossum(imputation_formula = X_partial ~ Xstar + log(Y_DIABETES), 
                     analysis_formula = Y_DIABETES ~ X_partial + offset(log(O_POP)), 
                     data = food_access, 
                     B = 20)
mod_obes = impPossum(imputation_formula = X_partial ~ Xstar + log(Y_OBESITY), 
                     analysis_formula = Y_OBESITY ~ X_partial + offset(log(O_POP)), 
                     data = food_access, 
                     B = 20)

# Save results for forest plot
imp_res = data.frame(Analysis = "Imputation",
                     Outcome = c("Diagnosed Diabetes", "Obesity"),
                     Coefficient = "PR",
                     Spatial = FALSE,
                     Est = c(est(2, mod_diab), est(2, mod_obes)), 
                     LB = c(lb(2, mod_diab), lb(2, mod_obes)),
                     UB = c(ub(2, mod_diab), ub(2, mod_obes))) |> 
  dplyr::bind_rows(
    data.frame(Analysis = "Imputation",
               Outcome = c("Diagnosed Diabetes", "Obesity"),
               Coefficient = "(Intercept)",
               Spatial = FALSE,
               Est = c(est(1, mod_diab), est(1, mod_obes)), 
               LB = c(lb(1, mod_diab), lb(1, mod_obes)),
               UB = c(ub(1, mod_diab), ub(1, mod_obes))) 
  )

# Model 1a: Diagnosed diabetes among adults aged >=18 years 
## Predictor X = proximity to healthy foods based on map-based distance
mod_diab = impPossum(imputation_formula = X_partial ~ Xstar + log(Y_DIABETES), 
                     analysis_formula = Y_DIABETES ~ X_partial + adjacency(1 | LocationID) + offset(log(O_POP)), 
                     data = food_access, 
                     adjMatrix = adj_matrix,
                     B = 20)

# Model 2a: Obesity among adults aged >=18 years
## Predictor X = proximity to healthy foods based on map-based distance
mod_obes = impPossum(imputation_formula = X_partial ~ Xstar + log(Y_OBESITY), 
                     analysis_formula = Y_OBESITY ~ X_partial + adjacency(1 | LocationID) + offset(log(O_POP)), 
                     data = food_access, 
                     adjMatrix = adj_matrix,
                     B = 20)

# Save results for forest plot
imp_res = data.frame(Analysis = "Imputation",
                     Outcome = c("Diagnosed Diabetes", "Obesity"),
                     Coefficient = "PR",
                     Spatial = TRUE,
                     Est = exp(c(mod_diab$Estimate[2], mod_obes$Estimate[2])), 
                     LB = exp(c(mod_diab$Estimate[2] - 1.96 * mod_diab$Standard.Error[2],
                                mod_obes$Estimate[2] - 1.96 * mod_obes$Standard.Error[2])),
                     UB = exp(c(mod_diab$Estimate[2] + 1.96 * mod_diab$Standard.Error[2],
                                mod_obes$Estimate[2] + 1.96 * mod_obes$Standard.Error[2]))) |>  
  dplyr::bind_rows(
    data.frame(Analysis = "Imputation",
               Outcome = c("Diagnosed Diabetes", "Obesity"),
               Coefficient = "(Intercept)",
               Spatial = TRUE,
               Est = exp(c(mod_diab$Estimate[1], mod_obes$Estimate[1])), 
               LB = exp(c(mod_diab$Estimate[1] - 1.96 * mod_diab$Standard.Error[1],
                          mod_obes$Estimate[1] - 1.96 * mod_obes$Standard.Error[1])),
               UB = exp(c(mod_diab$Estimate[1] + 1.96 * mod_diab$Standard.Error[1],
                          mod_obes$Estimate[1] + 1.96 * mod_obes$Standard.Error[1])))
  ) |> 
  dplyr::bind_rows(imp_res)
