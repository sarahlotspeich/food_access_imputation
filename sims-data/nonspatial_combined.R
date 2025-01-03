f = paste0("~/Documents/food_access_imputation/sims-data/nonspatial/", 
           list.files("~/Documents/food_access_imputation/sims-data/nonspatial/"))

for (N in c(387, 2168)) {
  for (tau in sqrt(c(0, 0.01, 0.07, 0.14)) * 100) {
    ## Subset by sample size
    sub_f = grep(pattern = paste0("N", N), x = f, value = TRUE) 
    ## Further subset by tau
    sub_f = grep(pattern = paste0("tau", tau), x = sub_f, value = TRUE) 
    ## Combine these files 
    sub_res = do.call(dplyr:::bind_rows, 
                      lapply(X = sub_f, 
                             FUN = read.csv))
    ## Save them 
    sub_res |> 
      write.csv(file = paste0("~/Documents/food_access_imputation/sims-data/nonspatial_combined/proximity_N", N, "_tau", tau, "_seeds11422thru11431.csv"), 
                row.names = FALSE)
  }
}
