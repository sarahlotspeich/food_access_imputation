# Install libraries
## Run once
## if needed: install.packages("devtools") 
## then: 
## devtools::install_github("sarahlotspeich/possum", 
##                          ref = "main") 

# Load libraries
library(possum) ## for multiple imputation estimatorx
library(tictoc) ## to calculate runtime

# Random seed to be used for each simulation setting
sim_seed = 11422

# Number of replicates per simulation setting
num_reps = 1000
## Note: You may want to run this code was run in parallel a cluster instead of locally, as it can be slow.

# Set parameters that won't be varied in the loop
## These values will be set as the defaults in the sim_data() function for convenience
fix_beta0 = -2.7 ## outcome model intercept (leads to ~ 7% prevalence)
fix_beta1 = log(1.01) ## log prevalence ratio
fix_muU = -0.7 ## error mean
fix_sigmaU = 0.8 ## error standard deviation

# --------------------------------------------------------------------
# Function to simulate data (arguments defined as follows)
## N = number of neighborhoods (sample size)
## beta0 = model intercept
## beta1 = log prevalence ratio 
## muU = mean of the measurement error distribution
## sigmaU = standard deviation of the measurement error distribution
# --------------------------------------------------------------------
sim_data = function(N, beta0 = fix_beta0, beta1 = fix_beta1, muU = fix_muU, sigmaU = fix_sigmaU) {
  ## Simulate true (map-based) proximity to grocery store
  X = rgamma(n = N,
             shape = 1,
             scale = 2.5) 
  
  ## Simulate random errors
  U = truncnorm::rtruncnorm(n = N, 
                            a = -Inf, 
                            b = 0, 
                            mean = muU, 
                            sd = sigmaU)
  
  ## Construct error-prone (straight-line) proximity to grocery store
  Xstar = X + U ### assuming additive measurement error model
  
  ## Simulate population
  P = rpois(n = N, 
            lambda = 4165)
  
  ## Simulate cases of health outcome
  lambda = exp(beta0 + beta1 * X)
  Cases = rpois(n = N, 
                lambda = P * lambda)
  
  ## Create dataset
  dat = data.frame(id = 1:N, X, Xstar, P, Cases)
  
  # Return dataset
  return(dat)
}

# Loop over different sample sizes: N = 390 (Piedmont Triad), 2200 (all of NC)
for (N in c(390, 2200)) {
  tic(paste("Sims with N =", N)) ## Start counting runtime for sims with current sample size N
  # And proportion to be queried for complete case/imputation analyses: 0.1, 0.25, 0.5, 0.75
  for (q in c(0.1, 0.25, 0.5, 0.75)){
    # Be reproducible
    set.seed(sim_seed) ## set random seed
    
    # Create dataframe to save results for setting
    sett_res = data.frame(sim = paste(sim_seed, 1:num_reps, sep = "-"), 
                          N, beta0 = fix_beta0, beta1 = fix_beta1, muU = fix_muU, sigmaU = fix_sigmaU, q = q, avg_prev = NA, ## simulation setting
                          beta0_gs = NA, se_beta0_gs = NA, beta1_gs = NA, se_beta1_gs = NA, ## gold standard analysis
                          beta0_n = NA, se_beta0_n = NA, beta1_n = NA, se_beta1_n = NA, ## naive analysis
                          beta0_cc = NA, se_beta0_cc = NA, beta1_cc = NA, se_beta1_cc = NA, ## complete case analysis
                          beta0_imp = NA, se_beta0_imp = NA, beta1_imp = NA, se_beta1_imp = NA ## imputation analysis
    )
    
    # Loop over replicates 
    for (r in 1:num_reps) {
      # Generate data
      dat = sim_data(N  = N) ## sample size
      
      # Save average neighborhood prevalence
      sett_res$avg_prev[r] = mean(dat$Cases / dat$P)
      
      # Fit the gold standard model
      fit_gs = glm(formula = Cases ~ X, 
                   family = poisson,
                   offset = log(P),
                   data = dat)
      sett_res[r, c("beta0_gs", "beta1_gs")] = coefficients(fit_gs) ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_gs", "se_beta1_gs")] = sqrt(diag(vcov(fit_gs))) ## and its standard error
      
      # Fit the naive model
      fit_n = glm(formula = Cases ~ Xstar, 
                  family = poisson,
                  offset = log(P),
                  data = dat)
      sett_res[r, c("beta0_n", "beta1_n")] = coefficients(fit_n) ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_n", "se_beta1_n")] = sqrt(diag(vcov(fit_n))) ## and its standard error
      
      # Select subset of neighborhoods/rows for map-based measures
      query_rows = sample(x = 1:N, 
                          size = ceiling(q * N), 
                          replace = FALSE)
      
      # Make X NA/missing for rows not in selected subset (query_rows)
      dat[!(dat$id %in% query_rows), "X"] = NA 
      
      # Fit the complete case model
      fit_cc = glm(formula = Cases ~ X, 
                   family = poisson,
                   offset = log(P),
                   data = dat)
      sett_res[r, c("beta0_cc", "beta1_cc")] = coefficients(fit_cc) ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_cc", "se_beta1_cc")] = sqrt(diag(vcov(fit_cc))) ## and its standard error
      
      # Fit the pooled MI model
      fit_imp = impPossum(imputation_formula = X ~ Xstar + log(Cases), 
                          analysis_formula = Cases ~ X + offset(log(P)), 
                          data = dat, 
                          B = 20)
      sett_res[r, c("beta0_imp", "beta1_imp")] = fit_imp$Estimate ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_imp", "se_beta1_imp")] = fit_imp$Standard.Error ## and its standard error
      
      # Save results
      write.csv(x = sett_res,
                file = paste0("sims-data/vary_q/proximity_N", N, "_q", 100 * q, "_seed", sim_seed, ".csv"), 
                row.names = F)
    }
  }
  toc() ## End runtime for sims with current sample size N
}

# Timing from tictoc:
## Sims with N = 390: 169.051 sec elapsed
## Sims with N = 2200: 448.112 sec elapsed