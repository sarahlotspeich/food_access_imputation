# //////////////////////////////////////////////////////////////////////
# Replicate Table 3 ////////////////////////////////////////////////////
# Caption begins "Simulation results under higher disease prevalence ///
# and prevalence ratios for map-based proximity to healthy foods." /////
# //////////////////////////////////////////////////////////////////////

# Load packages
library(dplyr) # To wrangle data
library(kableExtra) # To format pretty tables

# //////////////////////////////////////////////////////////////////////
# Read in simulation results from GitHub ///////////////////////////////
# //////////////////////////////////////////////////////////////////////
res = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/sims-data/vary_prev_sims_combined.csv")
## Note: Simulations were run on random seed 114 (with 1000 reps per seed, per setting)
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
cp = function(est, se, truth) {
  mean((est - 1.96 * se) <= truth & truth <= (est + 1.96 * se))
}
res_summ = res |> 
  group_by(beta0, beta1) |> 
  summarize(bias_gs = mean((beta_gs - beta1) / ifelse(beta1 == 0, 1, beta1)), ese_gs = sd(beta_gs), ase_gs = mean(se_beta_gs), 
            cp_gs = cp(est = beta_gs, se = se_beta_gs, truth = beta1), 
            bias_n = mean((beta_n - beta1) / ifelse(beta1 == 0, 1, beta1)), ese_n = sd(beta_n), ase_n = mean(se_beta_n), 
            cp_n = cp(est = beta_n, se = se_beta_n, truth = beta1), 
            bias_cc = mean((beta_cc - beta1) / ifelse(beta1 == 0, 1, beta1)), ese_cc = sd(beta_cc), ase_cc = mean(se_beta_cc), 
            cp_cc = cp(est = beta_cc, se = se_beta_cc, truth = beta1), 
            bias_imp = mean((beta_imp - beta1) / ifelse(beta1 == 0, 1, beta1)), ese_imp = sd(beta_imp), ase_imp = mean(se_beta_imp), 
            cp_imp = cp(est = beta_imp, se = se_beta_imp, truth = beta1)
            ) |> 
  ungroup()

# //////////////////////////////////////////////////////////////////////
# Format table for export to LaTex /////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
# Write function to add "padded" zeros and wrap with $$ for consistency 
format_num = function(num, digits = 3) {
  paste0("$", format(round(num, 3), nsmall = digits), "$")
}

# Format res_summ for LaTex
res_summ = res_summ |> 
  mutate_at(.vars = 3:18, .funs = format_num, digits = 3) |>
  mutate(approx.prev = format_num(num = exp(beta0), digits = 2),
         prev.ratio = format_num(num = exp(beta1), digits = 2)) |> 
  select(approx.prev, prev.ratio, everything(), -beta0, -beta1) |> 
  arrange(approx.prev, prev.ratio)
## Change column names 
colnames(res_summ) = c("Prev.", "PR", rep(c("Bias", "ESE", "ASE", "CP"), times = 4))
res_summ |> 
  kable(format = "latex", 
        booktabs = TRUE, 
        escape = FALSE, 
        align = "ccrcccrcccrcccrccc") |> 
  kable_styling() |> 
  add_header_above(header = c(" " = 2, "Gold Standard" = 4, "Naive" = 4, "Complete Case" = 4, "Imputation" = 4), 
                   bold = TRUE) |> 
  row_spec(row = 0, bold = TRUE)
