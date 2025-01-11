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

## Subset to log prevalence ratio (log PR) for proximity to healthy foods
plot_data = plot_data |> 
  dplyr::filter(Coefficient == "(Intercept)")

############################################################################################
## CREATE A FOREST PLOT OF ALL MODEL RESULTS FOR PREVALENCE RATIO //////////////////////////
############################################################################################
plot_data |> 
  dplyr::mutate(Analysis = factor(x = Analysis, 
                                  levels = c("Naive", 
                                             "Complete Case",
                                             "Imputation", 
                                             "Gold Standard"),
                                  labels = c("Naive", 
                                             "Complete\nCase",
                                             "Imputation",
                                             "Gold\nStandard")),
                Outcome = factor(x = Outcome, 
                                 levels = c("Diagnosed Diabetes", "Obesity"), 
                                 labels = c("Outcome: Diagnosed Diabetes", "Outcome: Obesity")),
                Spatial = ifelse(test = Spatial, 
                                 yes = "Spatial", 
                                 no = "Non-Spatial"),
                Spatial = factor(x = Spatial, 
                                 levels = c("Non-Spatial", "Spatial")),
                Coefficient = factor(x = Coefficient, 
                                     levels = c("(Intercept)", "X_full", "X_partial", 
                                                "Xstar", "METROTRUE", "METROTRUE:X_full", 
                                                "METROTRUE:X_partial", "METROTRUE:Xstar"), 
                                     labels = c("Coefficient: Baseline Prevalence", "Coefficient: Proximity", "Coefficient: Proximity", 
                                                "Coefficient: Proximity", "Coefficient: Metropolitan", "Coefficient: Proximity x Metropolitan", 
                                                "Coefficient: Proximity x Metropolitan", "Coefficient: Proximity x Metropolitan")), 
                Null = ifelse(test = LB < 1 & UB > 1, 
                              yes = "Contains 1", 
                              no = "Does Not Contain 1")) |> 
  ggplot(aes(x = Analysis, y = Est, 
             shape = Spatial, group = Spatial, linetype = Spatial)) + 
  geom_errorbar(aes(ymin = LB, ymax = UB), 
                lwd = 1.5, 
                position = position_dodge(width = 1), 
                alpha = 1, 
                color = "#6F98DF") + 
  geom_point(size = 3, 
             position = position_dodge(width = 1), 
             color = "#6F98DF") + 
  facet_wrap(. ~ Outcome + Coefficient, 
             scales = "free") + 
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", 
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white"), 
        legend.box = "horizontal", 
        legend.direction = "horizontal") +
  scale_shape_manual(name = "Model", 
                     values = c(15, 16)) + 
  scale_linetype_manual(name = "Model", 
                        values = c(1, 11)) + 
  xlab("Analysis Method") +
  ylab("Baseline Prevalence (95% Confidence Interval)")

ggsave(filename = "figures/figS12_forest_plot_intercept_piedmont.png", 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in")
ggsave(filename = "figures/figS12_forest_plot_intercept_piedmont.pdf", 
       device = "pdf", 
       width = 7, 
       height = 5, 
       units = "in")
