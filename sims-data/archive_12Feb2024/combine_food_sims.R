food_dir = "~/Documents/food/sims-data/"
pV_files = paste0(food_dir, "vary_pV/", list.files(paste0(food_dir, "vary_pV/")))
pV_res = do.call(what = dplyr::bind_rows, 
                 args = lapply(X = pV_files, 
                               FUN = read.csv))
pV_res |> 
  write.csv(paste0(food_dir, "vary_pV_sims_combined.csv"), 
            row.names = F)

sigmaU_files = paste0(food_dir, "vary_sigmaU/", list.files(paste0(food_dir, "vary_sigmaU/")))
sigmaU_res = do.call(what = dplyr::bind_rows, 
                     args = lapply(X = sigmaU_files, 
                                   FUN = read.csv))
sigmaU_res |> 
  write.csv(paste0(food_dir, "vary_sigmaU_sims_combined.csv"), 
            row.names = F)

muU_files = paste0(food_dir, "vary_muU/", list.files(paste0(food_dir, "vary_muU/")))
muU_res = do.call(what = dplyr::bind_rows, 
                     args = lapply(X = muU_files, 
                                   FUN = read.csv))
muU_res |> 
  write.csv(paste0(food_dir, "vary_muU_sims_combined.csv"), 
            row.names = F)

inclY_files = paste0(food_dir, "include_outcome/", list.files(paste0(food_dir, "include_outcome/")))
inclY_res = do.call(what = dplyr::bind_rows, 
                    args = lapply(X = inclY_files, 
                                  FUN = read.csv))
inclY_res |> 
  write.csv(paste0(food_dir, "include_outcome_sims_combined.csv"), 
            row.names = F)

prev_files = paste0(food_dir, "vary_prev/", list.files(paste0(food_dir, "vary_prev/")))
prev_res = do.call(what = dplyr::bind_rows, 
                    args = lapply(X = prev_files, 
                                  FUN = read.csv))
prev_res |> 
  write.csv(paste0(food_dir, "vary_prev_sims_combined.csv"), 
            row.names = F)

mult_files = paste0(food_dir, "mult_error/", list.files(paste0(food_dir, "mult_error/")))
mult_res = do.call(what = dplyr::bind_rows, 
                   args = lapply(X = mult_files, 
                                 FUN = read.csv))
mult_res |> 
  write.csv(paste0(food_dir, "mult_error_sims_combined.csv"), 
            row.names = F)
