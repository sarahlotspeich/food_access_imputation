# //////////////////////////////////////////////////////////////////////
# Replicate Table S4 ///////////////////////////////////////////////////
# Caption begins "Simulation results for the mixed-effects model under /
# additive errors in straight-line proximity to healthy foods..." //////
# //////////////////////////////////////////////////////////////////////

## Load packages
library(dplyr) ### To wrangle data
library(kableExtra) ### To format pretty tables

# //////////////////////////////////////////////////////////////////////
# Read in simulation results from GitHub ///////////////////////////////
# //////////////////////////////////////////////////////////////////////
file_urls = c(paste0("https://raw.githubusercontent.com/sarahlotspeich/food/main/sims-data/spatial_combined/proximity_N", rep(c(387, 2168), each = 4), 
                     "_tau", rep(sqrt(c(0, 0.01, 0.07, 0.14)) * 100, times = 2), 
                     "_seeds11422thru11431.csv"))
res = do.call(bind_rows, 
              lapply(X = file_urls, 
                     FUN = read.csv)) |> 
  mutate(tau2 = tau ^ 2) ## create new column with tau ^ 2
## Note: Simulations were run on random seeds 11422 to 11431 (with 100 reps per seed, per setting)
## This information is captured in the "sim" variable which is of the form seed-replicate. 

# //////////////////////////////////////////////////////////////////////
# Summarize simulation results by setting //////////////////////////////
# //////////////////////////////////////////////////////////////////////
## Function to calculate empirical coverage probability of 95% confidence intervals
cp = function(est, se, truth) {
  mean((est - 1.96 * se) <= truth & truth <= (est + 1.96 * se))
}
res_summ = res |> 
  group_by(N, tau2) |> 
  summarize(bias_gs = mean((beta1_gs - beta1) / beta1), ese_gs = sd(beta1_gs), 
            bias_n = mean((beta1_n - beta1) / beta1), ese_n = sd(beta1_n), 
            bias_cc = mean((beta1_cc - beta1) / beta1), ese_cc = sd(beta1_cc), 
            bias_imp = mean((beta1_imp - beta1) / beta1), ese_imp = sd(beta1_imp), ase_imp = mean(se_beta1_imp), 
            cp_imp = cp(est = beta1_imp, se = se_beta1_imp, truth = beta1)
            ) |> 
  dplyr::mutate(re_cc = (ese_gs ^ 2) / (ese_cc ^ 2), 
                re_imp = (ese_gs ^ 2) / (ese_imp ^ 2)) |> 
  dplyr::select(N, tau2, dplyr::ends_with(c("gs", "n", "cc", "imp")))

# //////////////////////////////////////////////////////////////////////
# Format table for export to LaTex /////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
## Write function to add "padded" zeros and wrap with $$ for consistency 
format_num = function(num, digits = 3) {
  paste0("$", format(round(num, 3), nsmall = digits), "$")
}

## Format res_summ for LaTex
res_summ = res_summ |> 
  mutate_at(.vars = 3:14, .funs = format_num, digits = 3) |>
  mutate(N = format_num(num = N, digits = 0), 
         tau2 = format_num(num = tau2, digits = 2))
## Change column names 
colnames(res_summ) = c("$\\pmb{N}$", "$\\pmb{\\sigma_U}$", 
                       rep(c("Bias", "ESE"), times = 2),
                       "Bias", "ESE", "RE",
                       "Bias", "ESE", "ASE", "CP", "RE")
res_summ |> 
  kable(format = "latex", 
        booktabs = TRUE, 
        escape = FALSE, 
        align = "rcrcrcrccrcccc") |> 
  kable_styling() |> 
  add_header_above(header = c(" " = 2, "Gold Standard" = 2, "Naive" = 2, "Complete Case" = 3, "Imputation" = 5), 
                   bold = TRUE) |> 
  row_spec(row = 0, bold = TRUE)
## And a \multicolumn used to separate the three parameters