# Note: This script is called by fig4_forest_plot_piedmont.R, and it
## assumes that a number of packages, scripts, and data have already been loaded in 

# Functions to pull out of coefficient summary
est = function(i, fit) {
  round(exp(fit$coefficients[i]), 3)
}
lb = function(i, fit) {
  se = sqrt(diag(vcov(fit)))[i]
  round(exp(fit$coefficients[i] - 1.96 * se), 3)
}
ub = function(i, fit) {
  se = sqrt(diag(vcov(fit)))[i]
  round(exp(fit$coefficients[i] + 1.96 * se), 3)
}
ci = function(i, fit) {
  se = sqrt(diag(vcov(fit)))[i]
  paste(round(exp(fit$coefficients[i] + c(-1.96, 1.96) * se), 3), collapse = ", ")
}

## Complete Case Analysis
### Non-Spatial Models
mod_diab = glm(formula = Y_DIABETES ~ X_partial, 
               family = poisson(link = "log"), 
               data = food_access, 
               offset = log(O_POP))
mod_obes = glm(formula = Y_OBESITY ~ X_partial,
               family = poisson(link = "log"), 
               data = food_access,
               offset = log(O_POP))

# Save results for forest plot
cc_res = data.frame(Analysis = "Complete Case",
                    Outcome = c("Diagnosed Diabetes", "Obesity"),
                    Est = c(est(2, mod_diab), est(2, mod_obes)), 
                    LB = c(lb(2, mod_diab), lb(2, mod_obes)),
                    UB = c(ub(2, mod_diab), ub(2, mod_obes))) 

# Model 1a: Diagnosed diabetes among adults aged >=18 years 
## Predictor X = proximity to healthy foods based on map-based distance
mod_diab = fitme(formula = Y_DIABETES ~ X_partial + adjacency(1 | LocationID) + offset(log(O_POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Model 2a: Obesity among adults aged >=18 years
## Predictor X = proximity to healthy foods based on map-based distance
mod_obes = fitme(formula = Y_OBESITY ~ X_partial + adjacency(1 | LocationID) + offset(log(O_POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Save results for forest plot
cc_res = get_sp_mod_summ(terms = "X_full", mod = mod_diab) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "X_full", mod = mod_obes)) |> 
  dplyr::mutate(Analysis = "Complete Case", 
                Outcome = c("Diagnosed Diabetes", "Obesity"), 
                Spatial = TRUE) |> 
  dplyr::select(Analysis, Outcome, Spatial, Est, LB, UB) |> 
  dplyr::bind_rows(gs_res)
