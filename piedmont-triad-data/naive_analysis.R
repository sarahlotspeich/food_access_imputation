# Note: This script is called by fig4_forest_plot_piedmont.R, and it
## assumes that a number of packages, scripts, and data have already been loaded in 

## Naive Analysis
### Non-Spatial Models
mod_diab = glm(formula = Y_DIABETES ~ METRO * Xstar, 
               family = poisson(link = "log"), 
               offset = log(O_POP),
               data = food_access)
mod_obes = glm(formula = Y_OBESITY ~ METRO * Xstar,
               family = poisson(link = "log"), 
               offset = log(O_POP),
               data = food_access)

# Save results for forest plot
naive_res = mod_diab |> 
  summary() |> 
  coefficients() |> 
  data.frame() |> 
  mutate(Analysis = "Naive", 
         Outcome = "Diagnosed Diabetes", 
         Coefficient = names(mod_diab$coefficients), 
         Spatial = FALSE, 
         LB = exp(Estimate - 1.96 * Std..Error), 
         UB = exp(Estimate + 1.96 * Std..Error), 
         Estimate = exp(Estimate)) |> 
  rename(Est = Estimate) |> 
  select(Analysis, Outcome, Coefficient, Spatial, Est, LB, UB) |> 
  bind_rows(
    mod_obes |> 
      summary() |> 
      coefficients() |> 
      data.frame() |> 
      mutate(Analysis = "Naive", 
             Outcome = "Obesity", 
             Coefficient = names(mod_obes$coefficients), 
             Spatial = FALSE, 
             LB = exp(Estimate - 1.96 * Std..Error), 
             UB = exp(Estimate + 1.96 * Std..Error), 
             Estimate = exp(Estimate)) |> 
      rename(Est = Estimate) |> 
      select(Analysis, Outcome, Coefficient, Spatial, Est, LB, UB)
  )

# Model 1a: Diagnosed diabetes among adults aged >=18 years 
## Predictor X = proximity to healthy foods based on map-based distance
mod_diab = fitme(formula = Y_DIABETES ~ METRO * Xstar + adjacency(1 | GEOID) + offset(log(O_POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = ptW,
                 data = food_access)

# Model 2a: Obesity among adults aged >=18 years
## Predictor X = proximity to healthy foods based on map-based distance
mod_obes = fitme(formula = Y_OBESITY ~ METRO * Xstar + adjacency(1 | GEOID) + offset(log(O_POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = ptW,
                 data = food_access)

# Save results for forest plot
naive_res = do.call(what = dplyr::bind_rows, 
                 args = lapply(X = names(mod_diab$fixef), 
                               FUN = get_sp_mod_summ, 
                               mod = mod_diab)) |> 
  dplyr::rename(Coefficient = terms) |> 
  dplyr::mutate(Analysis = "Naive", 
                Outcome = "Diagnosed Diabetes", 
                Spatial = TRUE) |> 
  dplyr::select(Analysis, Outcome, Spatial, Est, LB, UB, Coefficient) |> 
  dplyr::bind_rows(
    do.call(what = dplyr::bind_rows, 
            args = lapply(X = names(mod_obes$fixef), 
                          FUN = get_sp_mod_summ, 
                          mod = mod_obes)) |> 
      dplyr::rename(Coefficient = terms) |> 
      dplyr::mutate(Analysis = "Naive", 
                    Outcome = "Obesity", 
                    Spatial = TRUE) |> 
      dplyr::select(Analysis, Outcome, Spatial, Est, LB, UB, Coefficient)
  ) |> 
  dplyr::bind_rows(naive_res)