# Run once
# if needed: install.packages("devtools") 
# then: 
# devtools::install_github("sarahlotspeich/possum", 
#                          ref = "main") 

library(possum, ## for multiple imputation estimator
        lib.loc = "/home/lotspes/R/x86_64-pc-linux-gnu-library/4.0") 

# Random seed to be used for each simulation setting
args = commandArgs(TRUE)
## When running on the cluster, give each array a unique seed by adding the array ID to 11422
sim_seed = 11422 + as.integer(args)

# Number of replicates per simulation setting
num_reps = 100
## Note: You may want to run this code was run in parallel a cluster instead of locally, as it can be slow.

# Set parameters that won't be varied in the loop
## These values will be set as the defaults in the sim_data() function for convenience
fix_avg_prev = 0.07 ## average prevalence (based on CHD)
fix_beta1 = log(1.01) ## log prevalence ratio (based on CHD)
fix_muU = -0.7 ## error mean
fix_sigmaU = 0.6 ## error standard deviation
fix_pV = 0.1 ## proportion of neighborhoods to be queried

# --------------------------------------------------------------------
# Function to simulate data (arguments defined as follows)
## N = number of neighborhoods (sample size)
## avg_prev = approximate outcome prevalence
## beta1 = log prevalence ratio 
## muU = mean of the measurement error distribution
## sigmaU = standard deviation of the measurement error distribution
## pV = proportion of neighborhoods to be queried 
# --------------------------------------------------------------------
sim_data = function(N, avg_prev = fix_avg_prev, beta1 = fix_beta1, muU = fix_muU, sigmaU = fix_sigmaU) {
  ## Get model intercept
  beta0 = log(avg_prev)
  
  ## Simulate true (map-based) proximity to grocery store from a Gamma(1.4, 1.7)
  X = rgamma(n = N,
             shape = 1.4,
             scale = 1.7) 
  
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
  # Be reproducible
  set.seed(sim_seed) ## set random seed
  
  # Create dataframe to save results for setting
  sett_res = data.frame(sim = paste(sim_seed, 1:num_reps, sep = "-"), 
                        N, beta1 = fix_beta1, muU = fix_muU, sigmaU = fix_sigmaU, pV = fix_pV, avg_prev = NA, ## simulation setting
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
                        size = ceiling(fix_pV * N), 
                        replace = FALSE)
    
    # Make X NA/missing for rows not in selected subset (query_rows)
    dat[!(dat$id %in% query_rows), "X"] = NA 
 
    # 1. Fit the pooled MI model with E(X|X*)
    fit_imp = possum::impPossum(imputation_formula = X ~ Xstar, 
                                analysis_formula = Cases ~ X + offset(log(P)), 
                                data = dat, 
                                B = 20)
    sett_res$beta_noY[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_noY[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # 2. Fit the pooled MI model with E(X|X*, Y)
    fit_imp = possum::impPossum(imputation_formula = X ~ Xstar + Cases, 
                                analysis_formula = Cases ~ X + offset(log(P)), 
                                data = dat, 
                                B = 20)
    sett_res$beta_Y[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_Y[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # 3. Fit the pooled MI model with E(X|X*, log(Y))
    fit_imp = possum::impPossum(imputation_formula = X ~ Xstar + log(Cases), 
                                analysis_formula = Cases ~ X + offset(log(P)), 
                                data = dat, 
                                B = 20)
    sett_res$beta_logY[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_logY[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # 4. Fit the pooled MI model with E(X|X*, log(Y/Pop))
    fit_imp = possum::impPossum(imputation_formula = X ~ Xstar + log(Cases/P), 
                                analysis_formula = Cases ~ X + offset(log(P)), 
                                data = dat, 
                                B = 20)
    sett_res$beta_logYoverPop[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_logYoverPop[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # 5. Fit the pooled MI model with E(X|X*, log(Y), log(Pop))
    fit_imp = possum::impPossum(imputation_formula = X ~ Xstar + log(Cases) + log(P), 
                                analysis_formula = Cases ~ X + offset(log(P)), 
                                data = dat, 
                                B = 20)
    sett_res$beta_logY_logPop[r] = fit_imp$Estimate[2] ## estimated log prevalence ratio
    sett_res$se_beta_logY_logPop[r] = fit_imp$Standard.Error[2] ## and its standard error
    
    # Save results
    write.csv(x = sett_res,
              file = paste0("incl_outcome/incl_outcome_N", N, "_seed", sim_seed, ".csv"), 
              row.names = F)
  }
}

