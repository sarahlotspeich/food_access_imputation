# Install libraries
## Run once
## if needed: install.packages("devtools") 
## then: 
## devtools::install_github("sarahlotspeich/possum", 
##                          ref = "main") 

# Load libraries
library(possum) ## for multiple imputation estimator
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
fix_q = 0.1 ## proportion to be queried

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
  
  # Be reproducible
  set.seed(sim_seed) ## set random seed
  
  # Create dataframe to save results for setting
  sett_res = data.frame(sim = paste(sim_seed, 1:num_reps, sep = "-"), 
                        N, beta1 = fix_beta1, muU = fix_muU, sigmaU = fix_sigmaU, q = fix_q, avg_prev = NA, ## simulation setting
                        beta_noY = NA, se_beta_noY = NA, ## impute with E(X|X*)
                        beta_Y = NA, se_beta_Y = NA, ## impute with E(X|X*, Y)
                        beta_logY = NA, se_beta_logY = NA, ## impute with E(X|X*, log(Y))
                        beta_logYoverPop = NA, se_beta_logYoverPop = NA, ## impute with E(X|X*, log(Y/Pop))
                        beta_logY_logPop = NA, se_beta_logY_logPop = NA ## impute with E(X|X*, log(Y), log(Pop))
  )
  
  # Loop over replicates 
  for (r in 1:num_reps) {
    # Generate data
    dat = sim_data(N  = N) ## sample size
    
    # Save average neighborhood prevalence
    sett_res$avg_prev[r] = mean(dat$Cases / dat$P)

    # Select subset of neighborhoods/rows for map-based measures
    query_rows = sample(x = 1:N, 
                        size = ceiling(fix_q * N), 
                        replace = FALSE)
    
    # Make X NA/missing for rows not in selected subset (query_rows)
    dat[!(dat$id %in% query_rows), "X"] = NA 
    
    # 1. Fit the pooled MI model with E(X|X*)
    fit_imp = impPossum(imputation_formula = X ~ Xstar, 
                        analysis_formula = Cases ~ X + offset(log(P)), 
                        data = dat, 
                        B = 20)
    sett_res$beta_noY[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_noY[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # 2. Fit the pooled MI model with E(X|X*, Y)
    fit_imp = impPossum(imputation_formula = X ~ Xstar + Cases, 
                        analysis_formula = Cases ~ X + offset(log(P)), 
                        data = dat, 
                        B = 20)
    sett_res$beta_Y[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_Y[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # 3. Fit the pooled MI model with E(X|X*, log(Y))
    fit_imp = impPossum(imputation_formula = X ~ Xstar + log(Cases), 
                        analysis_formula = Cases ~ X + offset(log(P)), 
                        data = dat, 
                        B = 20)
    sett_res$beta_logY[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_logY[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # 4. Fit the pooled MI model with E(X|X*, log(Y/Pop))
    fit_imp = impPossum(imputation_formula = X ~ Xstar + log(Cases/P), 
                        analysis_formula = Cases ~ X + offset(log(P)), 
                        data = dat, 
                        B = 20)
    sett_res$beta_logYoverPop[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_logYoverPop[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # 5. Fit the pooled MI model with E(X|X*, log(Y), log(Pop))
    fit_imp = impPossum(imputation_formula = X ~ Xstar + log(Cases) + log(P), 
                        analysis_formula = Cases ~ X + offset(log(P)), 
                        data = dat, 
                        B = 20)
    sett_res$beta_logY_logPop[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_logY_logPop[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # Save results
    write.csv(x = sett_res,
              file = paste0("sims-data/include_outcome/proximity_N", N, "_q", 100 * fix_q, "_seed", sim_seed, ".csv"), 
              row.names = F)
  }
  toc() ## End runtime for sims with current sample size N
}

# Timing from tictoc:
## Sims with N = 390: 150.197 sec elapsed
## Sims with N = 2200: 440.436 sec elapsed