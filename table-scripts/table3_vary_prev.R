# //////////////////////////////////////////////////////////////////////
# Replicate Table 3 ////////////////////////////////////////////////////
# Caption begins "Simulation results under higher disease prevalence ///
# and varied prevalence ratios for map-based proximity to healthy //////
# foods, as controlled by the coefficients $\beta_0$ (\textbf{Prev.} ///
# $=\exp(\beta_0)$) and $\beta_1$ (\textbf{PR} $=\exp(\beta_1)$), //////
# respectively. ////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////

## Load packages
library(dplyr) ### To wrangle data
library(kableExtra) ### To format pretty tables

# //////////////////////////////////////////////////////////////////////
# Read in simulation results from GitHub ///////////////////////////////
# //////////////////////////////////////////////////////////////////////
file_urls = c(paste0("https://raw.githubusercontent.com/sarahlotspeich/food/main/sims-data/vary_prev/proximity_prev", rep(c(7, 11, 33), each = 5), 
                     "_pr", rep(c(95, 99, 100, 101, 105), times = 3), 
                     "_seed11422.csv"))
res = do.call(bind_rows, 
              lapply(X = file_urls, 
                     FUN = read.csv))
## Note: Simulations were run on random seed 11422 (with 1000 reps per seed, per setting)
## This information is captured in the "sim" variable which is of the form seed-replicate. 

# //////////////////////////////////////////////////////////////////////
# Computer average disease prevalence by setting ///////////////////////
# //////////////////////////////////////////////////////////////////////
res |> 
  mutate(approx.prev = exp(beta0), 
         prev.ratio = exp(beta1)) |> 
  group_by(approx.prev, prev.ratio) |> 
  summarize(avg_prev = mean(avg_prev))

# //////////////////////////////////////////////////////////////////////
# Summarize simulation results by setting //////////////////////////////
# //////////////////////////////////////////////////////////////////////
## Function to calculate empirical coverage probability of 95% confidence intervals
cp = function(est, se, truth) {
  mean((est - 1.96 * se) <= truth & truth <= (est + 1.96 * se))
}
res_summ = res |> 
  dplyr::mutate(prev = exp(beta0), 
                pr = exp(beta1)) |> 
  group_by(prev, pr) |> 
  summarize(bias_gs = mean((beta1_gs - beta1) / ifelse(beta1 == 0, 1, beta1)), ese_gs = sd(beta1_gs), 
            bias_n = mean((beta1_n - beta1) / ifelse(beta1 == 0, 1, beta1)), ese_n = sd(beta1_n), 
            bias_cc = mean((beta1_cc - beta1) / ifelse(beta1 == 0, 1, beta1)), ese_cc = sd(beta1_cc), 
            bias_imp = mean((beta1_imp - beta1) / ifelse(beta1 == 0, 1, beta1)), ese_imp = sd(beta1_imp), ase_imp = mean(se_beta1_imp), 
            cp_imp = cp(est = beta1_imp, se = se_beta1_imp, truth = beta1)
  ) |> 
  dplyr::mutate(re_cc = (ese_gs ^ 2) / (ese_cc ^ 2), 
                re_imp = (ese_gs ^ 2) / (ese_imp ^ 2)) |> 
  dplyr::select(prev, pr, dplyr::ends_with(c("gs", "n", "cc", "imp")))

# //////////////////////////////////////////////////////////////////////
# Format table for export to LaTex /////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
## Write function to add "padded" zeros and wrap with $$ for consistency 
format_num = function(num, digits = 3) {
  paste0("$", format(round(num, digits = digits), nsmall = digits), "$")
}

## Format res_summ for LaTex
res_summ = res_summ |> 
  mutate_at(.vars = 3:14, .funs = format_num, digits = 3) |>
  mutate(prev = format_num(num = prev, digits = 2), 
         pr = format_num(num = pr, digits = 2))
## Change column names 
colnames(res_summ) = c("Prev.", "PR", 
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