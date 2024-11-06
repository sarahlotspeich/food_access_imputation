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
                Spatial = ifelse(test = Spatial, 
                                 yes = "Spatial", 
                                 no = "Non-Spatial")) |>
  ggplot(aes(x = Analysis, y = Est, col = Spatial)) +
  geom_point(size = 3, position = position_dodge(width = 0.8)) +
  geom_errorbar(lwd = 1.5, aes(ymin = LB, ymax = UB), position = position_dodge(width = 0.8)) +
  scale_color_manual(name = "Model:", 
                     values = c("#EB7B51", "#6F98DF")) + 
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", 
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white"), 
        legend.box = "vertical") +
  xlab("Analysis Method") +
  ylab("Baseline Prevalence") + 
  facet_wrap(~ Outcome, scales = "free")

ggsave(filename = "figures/figS9_forest_plot_intercept_piedmont.png", 
       device = "png", 
       width = 10, 
       height = 5, 
       units = "in")
ggsave(filename = "figures/figS9_forest_plot_intercept_piedmont.pdf", 
       device = "pdf", 
       width = 10, 
       height = 5, 
       units = "in")
