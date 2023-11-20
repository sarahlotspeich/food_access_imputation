# //////////////////////////////////////////////////////////////////////
# Replicate Figure S1 in Supplementary Materials  //////////////////////
# Caption begins "Estimated log prevalence ratios for food access $X_P$ 
# on health. The five possible ways to include the analysis model... ///
# //////////////////////////////////////////////////////////////////////

# Load packages
library(dplyr) # To wrangle data
library(tidyr) # To transform data
library(ggplot2) # To create plots
library(latex2exp) # To create LaTex labels for plots

# //////////////////////////////////////////////////////////////////////
# Read in simulation results from GitHub ///////////////////////////////
# //////////////////////////////////////////////////////////////////////
# Read in simulation results 
res = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food/main/sims-data/include_outcome_sims_combined.csv")
## Note: Simulations were run in parallel on random seeds 114-123 (with 100 reps per seed, per setting)
## This information is captured in the "sim" variable which is of the form "seed-replicate." 

# //////////////////////////////////////////////////////////////////////
# Create plot //////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
res |> 
  dplyr::select(sim, N, dplyr::starts_with("beta_")) |> 
  tidyr::gather(key = "imputation_model", value = "beta", -c(1:2)) |> 
  dplyr::mutate(imputation_model = factor(x = imputation_model, 
                                          levels = c("beta_noY", "beta_Y", "beta_logY", "beta_logYoverPop", "beta_logY_logPop"), 
                                          labels = c("E(X|X*)",
                                                     "E(X|X*, Y)",
                                                     "E(X|X*, log(Y))",
                                                     "E{X|X*, log(Y/Pop)}",
                                                     "E{X|X*, log(Y), log(Pop)}"
                                          )),
                N = factor(x = N, 
                           levels = c(100, 340, 2200), 
                           labels = c(TeX("$N = 100$ Neighborhoods"),
                                      TeX("$N = 340$ Neighborhoods"),
                                      TeX("$N = 2200$ Neighborhoods")
                           ))
  ) |> 
  ggplot(aes(x = imputation_model, y = beta, fill = imputation_model)) + 
  geom_boxplot() + 
  geom_hline(aes(yintercept = unique(res$beta1)), 
             linetype = 2) + 
  theme_bw(base_size = 12) + 
  facet_grid(cols = vars(N), 
             scales = "free",
             labeller = label_parsed) + 
  xlab("") + 
  ylab("Estimated Log Prevalence Ratio") + 
  scale_fill_manual(values = scales::viridis_pal(option = "magma")(6)[-1],
                    name = "Imputation Model") +
  theme(legend.position = "top", 
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# //////////////////////////////////////////////////////////////////////
# Save as 9" wide x 5" tall ////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////
ggsave("figures/figS1_inclY_in_imputation_model.png", 
       width = 9, height = 5, units = "in")
