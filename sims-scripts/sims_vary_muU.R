# Run once
# if needed: install.packages("devtools") 
# then: 
# devtools::install_github("sarahlotspeich/possum", 
#                          ref = "main") 

library(possum) ## for multiple imputation estimator

# Random seed to be used for each simulation setting
sim_seed = 11422

# Number of replicates per simulation setting
num_reps = 100
## Note: You may want to run this code was run in parallel a cluster instead of locally, as it can be slow.

# Set parameters that won't be varied in the loop
## These values will be set as the defaults in the sim_data() function for convenience
fix_avg_prev = 0.11 ## average prevalence
fix_beta1 = log(1.05) ## log prevalence ratio
fix_sigmaU = 1 ## error standard deviation
fix_pV = 0.1 ## proportion of neighborhoods to be queried# Run once

# --------------------------------------------------------------------
# Function to simulate data (arguments defined as follows)
## N = number of neighborhoods (sample size)
## avg_prev = approximate outcome prevalence
## beta1 = log prevalence ratio 
## muU = mean of the measurement error distribution
## sigmaU = standard deviation of the measurement error distribution
## pV = proportion of neighborhoods to be queried 
# --------------------------------------------------------------------
sim_data = function(N, avg_prev = fix_avg_prev, beta1 = fix_beta1, muU, sigmaU = fix_sigmaU) {
  ## Get model intercept
  beta0 = log(avg_prev)
  
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

# Loop over different sample sizes: N = 100, 340, 2200
for (N in c(100, 340, 2200)) {
  # And error standard deviation: 0.25, 0.5, 1
  for (mu in c(-1.2, -0.8, -0.4)){
    # Be reproducible
    set.seed(sim_seed) ## set random seed
    
    # Create dataframe to save results for setting
    sett_res = data.frame(sim = paste(sim_seed, 1:num_reps, sep = "-"), 
                          N, beta1 = fix_beta1, muU = mu, sigmaU = fix_sigmaU, pV = fix_pV, avg_prev = NA, ## simulation setting
                          beta_gs = NA, se_beta_gs = NA, ## gold standard analysis
                          beta_n = NA, se_beta_n = NA, ## naive analysis
                          beta_cc = NA, se_beta_cc = NA, ## complete case analysis
                          beta_imp = NA, se_beta_imp = NA ## imputation analysis
    )

    # Loop over replicates 
    for (r in 1:num_reps) {
      # Generate data
      dat = sim_data(N  = N, ## sample size
                     muU = mu) ## error mean
      
      # Save average neighborhood prevalence
      sett_res$avg_prev[r] = mean(dat$Cases / dat$P)
      
      # Fit the gold standard model
      fit_gs = glm(formula = Cases ~ X, 
                   family = poisson,
                   offset = log(P),
                   data = dat)
      sett_res$beta_gs[r] = coefficients(fit_gs)[2] ## estimated log prevalence ratio
      sett_res$se_beta_gs[r] = sqrt(diag(vcov(fit_gs)))[2] ## and its standard error
      
      # Fit the gold standard model
      fit_n = glm(formula = Cases ~ Xstar, 
                  family = poisson,
                  offset = log(P),
                  data = dat)
      sett_res$beta_n[r] = coefficients(fit_n)[2] ## estimated log prevalence ratio
      sett_res$se_beta_n[r] = sqrt(diag(vcov(fit_n)))[2] ## and its standard error
      
      # Select subset of neighborhoods/rows for map-based measures
      query_rows = sample(x = 1:N, 
                          size = ceiling(fix_pV * N), 
                          replace = FALSE)
      
      # Make X NA/missing for rows not in selected subset (query_rows)
      dat[!(dat$id %in% query_rows), "X"] = NA 
      
      # Fit the complete case model
      fit_cc = glm(formula = Cases ~ X, 
                   family = poisson,
                   offset = log(P),
                   data = dat)
      sett_res$beta_cc[r] = coefficients(fit_cc)[2] ## estimated log prevalence ratio
      sett_res$se_beta_cc[r] = sqrt(diag(vcov(fit_cc)))[2] ## and its standard error

      # Fit the pooled MI model
      fit_imp = possum::impPossum(imputation_formula = X ~ Xstar + log(Cases), 
                                  analysis_formula = Cases ~ X + offset(log(P)), 
                                  data = dat, 
                                  B = 20)
      sett_res$beta_imp[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
      sett_res$se_beta_imp[r] = fit_imp$Standard.Error[2] ## and its standard error
      
      # Save results
      write.csv(x = sett_res,
                file = paste0("vary_muU/proximity_N", N, "_muU", 100 * abs(mu), "_seed", sim_seed, ".csv"), 
                row.names = F)
    }
  }
}