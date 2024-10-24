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

# Model 1a: Diagnosed diabetes among adults aged >=18 years 
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_diab = glm(formula = Y_DIABETES ~ Xstar, 
               family = poisson(link = "log"), 
               offset = log(O_POP),
               data = food_access)

# Model 2a: Obesity among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_obes = glm(formula = Y_OBESITY ~ Xstar,
               family = poisson(link = "log"), 
               offset = log(O_POP),
               data = food_access)

# Save results for forest plot
naive_res = data.frame(Analysis = "Naive",
                       Outcome = c("Diagnosed Diabetes", "Obesity"),
                       Spatial = FALSE,
                       Est = c(est(2, mod_diab), est(2, mod_obes)), 
                       LB = c(lb(2, mod_diab), lb(2, mod_obes)),
                       UB = c(ub(2, mod_diab),ub(2, mod_obes))) 

# Model 1a: Diagnosed diabetes among adults aged >=18 years 
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_diab = fitme(formula = Y_DIABETES ~ Xstar + adjacency(1 | LocationID) + offset(log(O_POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Model 2a: Obesity among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_obes = fitme(formula = Y_OBESITY ~ Xstar + adjacency(1 | LocationID) + offset(log(O_POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Save results for forest plot
naive_res = get_sp_mod_summ(terms = "Xstar", mod = mod_diab) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "Xstar", mod = mod_obes)) |> 
  dplyr::mutate(Analysis = "Naive", 
                Outcome = c("Diagnosed Diabetes", "Obesity"), 
                Spatial = TRUE) |> 
  dplyr::select(Analysis, Outcome, Est, LB, UB) |> 
  dplyr::bind_rows(naive_res)