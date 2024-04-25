# Load data 
## Health outcomes 
health = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/piedmont-triad-data/disease_prevalences_2022.csv")

## Proximity to health foods based on straight-line and map-based distances (census tracts)
food_access = read.csv(file = "~/Documents/food/forsyth-data/REVIEW_closest_healthy_foods_store_2022.csv") |> 
  dplyr::bind_rows(
    read.csv(file = "~/Documents/food/piedmont-triad-data/REVIEW_closest_healthy_foods_store_2022.csv")
  ) |> 
  dplyr::right_join(health, 
                    by = dplyr::join_by(LocationID == TractFIPS))

## Remove additional food access columns (not needed for primary analysis)
food_access = food_access |> 
  dplyr::select(LocationID, CountyName, dist_closest_straight, dist_closest_map, 
                POP, BPHIGH, CHD, DIABETES, OBESITY)

## Add "error" between straight-line and map-based distances
food_access = food_access |> 
  dplyr::mutate(dist_diff = dist_closest_map - dist_closest_straight)

food_access = food_access |> 
  dplyr::mutate(dist_closest_straight = dist_closest_straight, 
                dist_closest_map = dist_closest_map)

food_access = food_access |> 
  dplyr::mutate(U = dist_closest_straight - dist_closest_map, 
                W = dist_closest_straight / dist_closest_map,
                LocationID = as.character(LocationID)) |> 
  dplyr::left_join(tracts, 
                   by = dplyr::join_by(LocationID == GEOID))

neighborhoods = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/forsyth-data/nc_ct_pop_centers_2010.csv") |> ## Read in all NC population centers
  dplyr::filter(COUNTYFP %in% piedmont_triad_fips) |> ## Subset to counties in study area
  dplyr::mutate(LocationID = paste0(STATEFP, sprintf("%03s", COUNTYFP), sprintf("%06s", TRACTCE))) |> ## Construct LocationID by combining state, county, and tract FIPS codes
  dplyr::arrange(LocationID) |> ## Order by Location ID
  dplyr::filter(LocationID %in% food_access$LocationID) ## Subset to same census tracts

food_access = food_access |> 
  dplyr::arrange(LocationID) ## Order by Location ID

**Naive analysis model interpretations:** 
  
  -   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of diabetes** is expected to change by a factor of `r est(2, mod_diab)` (95\% CI: `r ci(2, mod_diab)`). 
  -   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of obesity** is expected to change by a factor of `r est(2, mod_obes)` (95\% CI: `r ci(2, mod_obes)`). 
-   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of high blood pressure** is expected to change by a factor of `r est(2, mod_hbp)` (95\% CI: `r ci(2, mod_hbp)`). 
  -   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of coronary heart disease** is expected to change by a factor of `r est(2, mod_chd)` (95\% CI: `r ci(2, mod_chd)`). 

```{r, echo = F, cache = TRUE}
# Save results for forest plot
res = data.frame(Analysis = "Naive",
                 Outcome = c("Coronary Heart Disease", "Diagnosed Diabetes", "High Blood Pressure", "Obesity"),
                 Est = c(est(2, mod_chd), est(2, mod_diab), est(2, mod_hbp), est(2, mod_obes)), 
                 LB = c(lb(2, mod_chd), lb(2, mod_diab), lb(2, mod_hbp), lb(2, mod_obes)),
                 UB = c(ub(2, mod_chd), ub(2, mod_diab), ub(2, mod_hbp), ub(2, mod_obes))) 
```

```{r naive-maps, echo = FALSE, cache = TRUE}
# Add model-predicted case counts 
food_access$BPHIGH_PRED = exp(predict(object = mod_hbp))
food_access$CHD_PRED = exp(predict(object = mod_chd))
food_access$DIABETES_PRED = exp(predict(object = mod_diab))
food_access$OBESITY_PRED = exp(predict(object = mod_obes))
```

### Spatial Models

```{r naive spatial model fits, include=FALSE, warning = FALSE}
# Model 1a: Diagnosed diabetes among adults aged >=18 years 
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_diab = fitme(formula = DIABETES ~ dist_closest_straight + adjacency(1 | LocationID) + offset(log(POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Model 2a: Obesity among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_obes = fitme(formula = OBESITY ~ dist_closest_straight + adjacency(1 | LocationID) + offset(log(POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Model 3a: High blood pressure among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_hbp = fitme(formula = BPHIGH ~ dist_closest_straight + adjacency(1 | LocationID) + offset(log(POP)), 
                family = poisson(link = "log"), 
                adjMatrix = adj_matrix,
                data = food_access)

# Model 4a: Coronary heart disease among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_chd = fitme(formula = CHD ~ dist_closest_straight + adjacency(1 | LocationID) + offset(log(POP)), 
                family = poisson(link = "log"), 
                adjMatrix = adj_matrix,
                data = food_access)
```

```{r, echo = F, cache = TRUE, include = F}
# Save results for forest plot
sp_res = get_sp_mod_summ(terms = "dist_closest_straight", mod = mod_diab) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "dist_closest_straight", mod = mod_obes)) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "dist_closest_straight", mod = mod_hbp)) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "dist_closest_straight", mod = mod_chd)) |> 
  dplyr::mutate(Analysis = "Naive", 
                Outcome = c("Diagnosed Diabetes", "Obesity","High Blood Pressure", "Coronary Heart Disease")) |> 
  dplyr::select(Analysis, Outcome, Est, LB, UB)
```

\newpage

## Gold Standard Analysis

### Non-Spatial Models

```{r gold standard model fits, warning = FALSE}
# Model 1b: Diagnosed diabetes among adults aged >=18 years 
## Predictor X = proximity to healthy foods based on map-based distance
mod_diab = glm(formula = DIABETES ~ dist_closest_map, 
               family = poisson(link = "log"), 
               offset = log(POP),
               data = food_access)

# Model 2b: Obesity among adults aged >=18 years
## Predictor X = proximity to healthy foods based on map-based distance
mod_obes = glm(formula = OBESITY ~ dist_closest_map,
               family = poisson(link = "log"), 
               offset = log(POP),
               data = food_access)

# Model 3b: High blood pressure among adults aged >=18 years
## Predictor X = proximity to healthy foods based on map-based distance
mod_hbp = glm(formula = BPHIGH ~ dist_closest_map, 
              family = poisson(link = "log"), 
              offset = log(POP),
              data = food_access)

# Model 4c: Coronary heart disease among adults aged >=18 years
## Predictor X = proximity to healthy foods based on map-based distance
mod_chd = glm(formula = CHD ~ dist_closest_map, 
              family = poisson(link = "log"), 
              offset = log(POP),
              data = food_access)
```

**Gold standard analysis model interpretations:** 
  
  -   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of diabetes** is expected to change by a factor of `r est(2, mod_diab)` (95\% CI: `r ci(2, mod_diab)`). 
  -   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of obesity** is expected to change by a factor of `r est(2, mod_obes)` (95\% CI: `r ci(2, mod_obes)`). 
-   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of high blood pressure** is expected to change by a factor of `r est(2, mod_hbp)` (95\% CI: `r ci(2, mod_hbp)`). 
  -   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of coronary heart disease** is expected to change by a factor of `r est(2, mod_chd)` (95\% CI: `r ci(2, mod_chd)`). 

```{r, echo = F, cache = TRUE}
# Save results for forest plot
res = res |> 
  dplyr::bind_rows(
    data.frame(Analysis = "Gold Standard",
               Outcome = c("Coronary Heart Disease", "Diagnosed Diabetes", "High Blood Pressure", "Obesity"),
               Est = c(est(2, mod_chd), est(2, mod_diab), est(2, mod_hbp), est(2, mod_obes)), 
               LB = c(lb(2, mod_chd), lb(2, mod_diab), lb(2, mod_hbp), lb(2, mod_obes)),
               UB = c(ub(2, mod_chd), ub(2, mod_diab), ub(2, mod_hbp), ub(2, mod_obes))) 
  )
```

```{r gold standard spatial model fits, include=FALSE, warning = FALSE}
# Model 1a: Diagnosed diabetes among adults aged >=18 years 
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_diab = fitme(formula = DIABETES ~ dist_closest_map + adjacency(1 | LocationID) + offset(log(POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Model 2a: Obesity among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_obes = fitme(formula = OBESITY ~ dist_closest_map + adjacency(1 | LocationID) + offset(log(POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Model 3a: High blood pressure among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_hbp = fitme(formula = BPHIGH ~ dist_closest_map + adjacency(1 | LocationID) + offset(log(POP)), 
                family = poisson(link = "log"), 
                adjMatrix = adj_matrix,
                data = food_access)

# Model 4a: Coronary heart disease among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_chd = fitme(formula = CHD ~ dist_closest_map + adjacency(1 | LocationID) + offset(log(POP)), 
                family = poisson(link = "log"), 
                adjMatrix = adj_matrix,
                data = food_access)
```

```{r, echo = F, cache = TRUE, include = F}
# Save results for forest plot
sp_res = get_sp_mod_summ(terms = "dist_closest_map", mod = mod_diab) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "dist_closest_map", mod = mod_obes)) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "dist_closest_map", mod = mod_hbp)) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "dist_closest_map", mod = mod_chd)) |> 
  dplyr::mutate(Analysis = "Gold Standard", 
                Outcome = c("Diagnosed Diabetes", "Obesity","High Blood Pressure", "Coronary Heart Disease")) |> 
  dplyr::select(Analysis, Outcome, Est, LB, UB) |> 
  dplyr::bind_rows(sp_res)
```

## Complete Case Analysis

```{r, eval = T}
## Define query indicators (= 1 if map-based measures are available, = 0 otherwise)
set.seed(918) ### make the sampling reproducible
n = 4 ### set number of census tracts sampled from each county to be queried
queried_subset = food_access |> 
  dplyr::group_by(CountyName) |> 
  dplyr::sample_n(size = n, replace = FALSE) 
### Draw a county-stratified random sample of n = 36 census tracts to query
queried_subset |> 
  dplyr::pull(CountyName) |> 
  table()
### Create column for queried X from complete-case
food_access = food_access |> 
  dplyr::mutate(dist_closest_map_cc = ifelse(test = LocationID %in% queried_subset$LocationID, 
                                             yes = dist_closest_map, 
                                             no = NA))
```

```{r, eval = F}
## Define query indicators (= 1 if map-based measures are available, = 0 otherwise)
set.seed(918) ### make the sampling reproducible
n = 0.1 * nrow(food_access) ### set number of census tracts sampled to be queried
queried_subset = food_access |> 
  dplyr::arrange(dist_closest_straight) |> 
  dplyr::slice(1:(n / 2)) |> 
  dplyr::pull(LocationID)
queried_subset = food_access |> 
  dplyr::arrange(desc(dist_closest_straight)) |> 
  dplyr::slice(1:(n / 2)) |> 
  dplyr::pull(LocationID) |> 
  append(queried_subset)
### Create column for queried X from complete-case
food_access = food_access |> 
  dplyr::mutate(dist_closest_map_cc = ifelse(test = LocationID %in% queried_subset, 
                                             yes = dist_closest_map, 
                                             no = NA))
```

```{r, warning = FALSE}
# Univariable imputation model fit
uni_imp_mod = lm(formula = dist_closest_map_cc ~ dist_closest_straight, 
                 data = food_access)
## View model summary
summary(uni_imp_mod)
```

```{r scatterplot of straight vs. map-based (partially queried), fig.cap = "Scatter plot of straight-line versus map-based proximity to healthy foods store for neighborhoods in the Piedmont Triad, North Carolina using the partially-queried data ($n = 48$)", message = FALSE, echo = F, cache = TRUE}
# Scatterplot of straight-line vs. map-based food access
food_access |> 
  ggplot(aes(x = dist_closest_straight, y = dist_closest_map_cc)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, 
              color = scales::viridis_pal(option = "magma")(6)[3]) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  annotate(geom = "text", x = 9, y = 13, 
           label = latex2exp::TeX(paste0("$\\widehat{X} = ", paste(round(uni_imp_mod$coefficients, 3), collapse = " + "), "X$")), 
           size = 4, 
           color = scales::viridis_pal(option = "magma")(6)[3]) + 
  theme_minimal(base_size = 12) +
  xlab("Straight-Line Proximity to Healthy Foods (in Miles)") +
  ylab("Map-Based Proximity to Healthy Foods (in Miles)") + 
  xlim(c(-1, 13)) + 
  ylim(c(-1, 13)) + 
  coord_equal() 
```

```{r densities-hybrid, fig.cap = "Densities of straight-line versus map-based distances to the nearest healthy foods store for neighborhoods in the Piedmont Triad, North Carolina using the partially-queried data", echo = F, cache = TRUE, warning = FALSE, message = FALSE}
food_access |>
  dplyr::select(LocationID, dist_closest_straight, dist_closest_map_cc) |> 
  tidyr::gather(key = "Distance", value = "Stores", -1) |> 
  dplyr::mutate(Distance = factor(x = Distance, 
                                  levels = c("dist_closest_straight", "dist_closest_map_cc"), 
                                  labels = c("Straight-Line", "Map-Based"))) |>
  ggplot() + 
  geom_boxplot(aes(x = Stores, y = Distance, fill = Distance)) + 
  scale_fill_manual(values = c("#B63679FF", "#FCFDBFFF"),
                    name = "Distance Calculation:",  
                    guide = "none") +
  theme_minimal(base_size = 24) + 
  theme(legend.position = "top") + 
  xlab("Proximity to Healthy Foods (in Miles)") +
  ylab("Distance Calculation")

food_access |>
  dplyr::select(LocationID, dist_closest_straight, dist_closest_map_cc) |> 
  tidyr::gather(key = "Distance", value = "Stores", -1) |> 
  dplyr::mutate(Distance = factor(x = Distance, 
                                  levels = c("dist_closest_straight", "dist_closest_map_cc"), 
                                  labels = c("Straight-Line", "Map-Based"))) |>
  ggplot() + 
  geom_histogram(aes(x = Stores, fill = Distance), alpha = 0.5) +  
  scale_fill_manual(values = c("#B63679FF", "#FCFDBFFF"),
                    name = "Distance Calculation:") +
  theme_minimal(base_size = 24) + 
  theme(legend.position = "top") + 
  xlab("Proximity to Healthy Foods (in Miles)")
```

```{r cc model fits, warning = FALSE}
# Model 1: Diagnosed diabetes among adults aged >=18 years 
mod_diab = glm(formula = DIABETES ~ dist_closest_map_cc, 
               family = poisson(link = "log"), 
               data = food_access, 
               offset = log(POP))

# Model 2: Obesity among adults aged >=18 years
mod_obes = glm(formula = OBESITY ~ dist_closest_map_cc,
               family = poisson(link = "log"), 
               data = food_access,
               offset = log(POP))

# Model 3: High blood pressure among adults aged >=18 years
mod_hbp = glm(formula = BPHIGH ~ dist_closest_map_cc, 
              family = poisson(link = "log"), 
              data = food_access,
              offset = log(POP))

# Model 4: Coronary heart disease among adults aged >=18 years
mod_chd = glm(formula = CHD ~ dist_closest_map_cc, 
              family = poisson(link = "log"), 
              data = food_access,
              offset = log(POP))
```

**Complete case analysis model interpretations:** 
  
  -   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of diabetes** is expected to change by a factor of `r est(2, mod_diab)` (95\% CI: `r ci(2, mod_diab)`). 
  -   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of obesity** is expected to change by a factor of `r est(2, mod_obes)` (95\% CI: `r ci(2, mod_obes)`). 
-   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of high blood pressure** is expected to change by a factor of `r est(2, mod_hbp)` (95\% CI: `r ci(2, mod_hbp)`). 
  -   For every additional mile to their nearest grocery store, a neighborhood's crude **prevalence of coronary heart disease** is expected to change by a factor of `r est(2, mod_chd)` (95\% CI: `r ci(2, mod_chd)`). 

```{r, echo = F, cache = TRUE}
# Save results for forest plot
res = res |> 
  dplyr::bind_rows(
    data.frame(Analysis = "Complete Case",
               Outcome = c("Coronary Heart Disease", "Diagnosed Diabetes", "High Blood Pressure", "Obesity"),
               Est = c(est(2, mod_chd), est(2, mod_diab), est(2, mod_hbp), est(2, mod_obes)), 
               LB = c(lb(2, mod_chd), lb(2, mod_diab), lb(2, mod_hbp), lb(2, mod_obes)),
               UB = c(ub(2, mod_chd), ub(2, mod_diab), ub(2, mod_hbp), ub(2, mod_obes))) 
  )
```

### Spatial Models

```{r cc spatial model fits, include=FALSE, warning = FALSE}
# Model 1a: Diagnosed diabetes among adults aged >=18 years 
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_diab = fitme(formula = DIABETES ~ dist_closest_map_cc + adjacency(1 | LocationID) + offset(log(POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Model 2a: Obesity among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_obes = fitme(formula = OBESITY ~ dist_closest_map_cc + adjacency(1 | LocationID) + offset(log(POP)), 
                 family = poisson(link = "log"), 
                 adjMatrix = adj_matrix,
                 data = food_access)

# Model 3a: High blood pressure among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_hbp = fitme(formula = BPHIGH ~ dist_closest_map_cc + adjacency(1 | LocationID) + offset(log(POP)), 
                family = poisson(link = "log"), 
                adjMatrix = adj_matrix,
                data = food_access)

# Model 4a: Coronary heart disease among adults aged >=18 years
## Predictor X* = proximity to healthy foods based on straight-line distance
mod_chd = fitme(formula = CHD ~ dist_closest_map_cc + adjacency(1 | LocationID) + offset(log(POP)), 
                family = poisson(link = "log"), 
                adjMatrix = adj_matrix,
                data = food_access)
```

```{r, echo = F, cache = TRUE, include = F}
# Save results for forest plot
sp_res = get_sp_mod_summ(terms = "dist_closest_map_cc", mod = mod_diab) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "dist_closest_map_cc", mod = mod_obes)) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "dist_closest_map_cc", mod = mod_hbp)) |> 
  dplyr::bind_rows(get_sp_mod_summ(terms = "dist_closest_map_cc", mod = mod_chd)) |> 
  dplyr::mutate(Analysis = "Complete Case", 
                Outcome = c("Diagnosed Diabetes", "Obesity","High Blood Pressure", "Coronary Heart Disease")) |> 
  dplyr::select(Analysis, Outcome, Est, LB, UB) |> 
  dplyr::bind_rows(sp_res)
```

## Imputation Analysis

### Imputation Models 

```{r}
food_access |> 
  dplyr::select(-geometry) |>
  write.csv("~/Documents/food/piedmont-triad-data/analysis_data.csv", 
            row.names = FALSE)
```