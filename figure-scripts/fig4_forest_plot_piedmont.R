############################################################################################
## SETUP ///////////////////////////////////////////////////////////////////////////////////
############################################################################################
## Load libraries
library(ggplot2) ## to make maps
library(tidycensus) ## for shapefiles

############################################################################################
## LOAD FOREST PLOT DATA ///////////////////////////////////////////////////////////////////
############################################################################################
plot_data = read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/forest_plot_data.csv")

############################################################################################
## CREATE A FOREST PLOT OF ALL MODEL RESULTS FOR PREVALENCE RATIO //////////////////////////
############################################################################################
plot_data |> 
  dplyr::filter(Coefficient != "(Intercept)") |> 
  dplyr::mutate(Analysis = factor(x = Analysis, 
                                  levels = c("Naive", 
                                             "Complete Case",
                                             "Imputation", 
                                             "Gold Standard"),
                                  labels = c("Naive", 
                                             "Complete\nCase",
                                             "Imputation",
                                             "Gold\nStandard")),
                Spatial = ifelse(test = Spatial, 
                                 yes = "Spatial", 
                                 no = "Non-Spatial"),
                Coefficient = factor(x = Coefficient, 
                                     levels = c("(Intercept)", "X_full", "X_partial", 
                                                "Xstar", "METROTRUE", "METROTRUE:X_full", 
                                                "METROTRUE:X_partial", "METROTRUE:Xstar"), 
                                     labels = c("Baseline Prevalence", "Proximity", "Proximity", 
                                                "Proximity", "Metropolitan", "Proximity x Metropolitan", 
                                                "Proximity x Metropolitan", "Proximity x Metropolitan"))) |> 
  ggplot(aes(x = Analysis, y = Est, color = Spatial)) + 
  geom_hline(data = data.frame(Coefficient = c("Proximity", "Metropolitan", 
                                               "Proximity x Metropolitan"), 
                               Yint = 1), 
             col = "grey", 
             lwd = 1.5, 
             aes(yintercept = Yint),
             linetype = 2) + 
  geom_point(size = 3, 
             position = position_dodge(width = 1)) + 
  geom_errorbar(aes(ymin = LB, ymax = UB), 
                lwd = 1.5, 
                position = position_dodge(width = 1)) + 
  facet_grid(rows = vars(Coefficient),
             cols = vars(Outcome), 
             scales = "free") + 
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", 
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white"), 
        legend.box = "vertical") +
  scale_color_manual(name = "Model:", 
                     values = c("#EB7B51", "#6F98DF")) + 
  xlab("Analysis Method") +
  ylab("Prevalence Ratio (95% Confidence Interval)")

ggsave(filename = "figures/rev_fig4_forest_plot_piedmont.png", 
       device = "png", 
       width = 10, 
       height = 7, 
       units = "in")
ggsave(filename = "figures/rev_fig4_forest_plot_piedmont.pdf", 
       device = "pdf", 
       width = 10, 
       height = 7, 
       units = "in")
