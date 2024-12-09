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
### Some were based on the Piedmont Triad data 
N = 387 ## number of neighborhoods / census tracts
muU = -0.7 ## error mean
sigmaU = 0.8 ## error standard deviation
lambdaPOP = 4095 ## average population per census tract
### And others were experimental 
q = 0.1 ## proportion of neighborhoods to be queried

# --------------------------------------------------------------------
# Function to simulate data (arguments defined as follows)
## beta0 = model intercept --> controls underlying prevalence of Y | X = 0
## beta1 = log prevalence ratio of Y ~ X 
# --------------------------------------------------------------------
sim_data = function(beta0, beta1) {
  ## Simulate true (map-based) proximity to grocery store
  X = rgamma(n = N,
             shape = 1,
             scale = 2.5) 
  
  ## Simulate random errors
  U = truncnorm::rtruncnorm(n = N, 
                            a = -X, 
                            b = 0, 
                            mean = muU, 
                            sd = sigmaU)
  
  ## Construct error-prone (straight-line) proximity to grocery store
  Xstar = X + U ### assuming additive measurement error model
  
  ## Simulate population
  POP = rpois(n = N, 
              lambda = lambdaPOP)
  
  ## Simulate cases of health outcome
  lambda = exp(beta0 + beta1 * X)
  Y = rpois(n = N, 
                lambda = POP * lambda)
  
  ## Create dataset
  dat = data.frame(id = 1:N, X, Xstar, POP, Y)
  
  # Return dataset
  return(dat)
}

# Loop over different intercepts: -2.7, -2.2, -1.1
for (beta0 in c(-2.7, -2.2, -1.1)) {
  # And log prevalence ratios: -0.05, -0.01, 0, 0.01, 0.05
  tic(paste("Sims with beta0 =", beta0)) ## Start counting runtime for sims with current intercept beta0
  for (beta1 in c(-0.05, -0.01, 0, 0.01, 0.05)){
    # Be reproducible
    set.seed(sim_seed) ## set random seed
    
    # Create dataframe to save results for setting
    sett_res = data.frame(sim = paste(sim_seed, 1:num_reps, sep = "-"), 
                          N = N, beta0 = beta0, beta1 = beta1, muU = muU, sigmaU = sigmaU, q = q, avg_prev = NA, ## simulation setting
                          beta0_gs = NA, se_beta0_gs = NA, beta1_gs = NA, se_beta1_gs = NA, ## gold standard analysis
                          beta0_n = NA, se_beta0_n = NA, beta1_n = NA, se_beta1_n = NA, ## naive analysis
                          beta0_cc = NA, se_beta0_cc = NA, beta1_cc = NA, se_beta1_cc = NA, ## complete case analysis
                          beta0_imp = NA, se_beta0_imp = NA, beta1_imp = NA, se_beta1_imp = NA ## imputation analysis
    )

    for (r in 1:num_reps) {
      # Generate data
      dat = sim_data(beta0 = beta0, ## intercept
                     beta1 = beta1) ## log prevalence ratio
      
      # Save average neighborhood prevalence
      sett_res$avg_prev[r] = mean(dat$Y / dat$POP)
      
      # Fit the gold standard model
      fit_gs = glm(formula = Y ~ X, 
                   family = poisson,
                   offset = log(POP),
                   data = dat)
      sett_res[r, c("beta0_gs", "beta1_gs")] = coefficients(fit_gs) ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_gs", "se_beta1_gs")] = sqrt(diag(vcov(fit_gs))) ## and its standard error
      
      # Fit the gold standard model
      fit_n = glm(formula = Y ~ Xstar, 
                  family = poisson,
                  offset = log(POP),
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
      fit_cc = glm(formula = Y ~ X, 
                   family = poisson,
                   offset = log(POP),
                   data = dat)
      sett_res[r, c("beta0_cc", "beta1_cc")] = coefficients(fit_cc) ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_cc", "se_beta1_cc")] = sqrt(diag(vcov(fit_cc))) ## and its standard error
      
      # Fit the pooled MI model
      fit_imp = impPossum(imputation_formula = X ~ Xstar + log(Y), 
                          analysis_formula = Y ~ X + offset(log(POP)), 
                          data = dat, 
                          B = 20)
      sett_res[r, c("beta0_imp", "beta1_imp")] = fit_imp$Estimate ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_imp", "se_beta1_imp")] = fit_imp$Standard.Error ## and its standard error
      
      # Save results
      write.csv(x = sett_res,
                file = paste0("vary_prev/proximity_prev", round(exp(beta0) * 100), "_pr", round(exp(beta1) * 100), "_seed", sim_seed, ".csv"), 
                row.names = F)
    }
  }
  toc() ## End runtime for sims with current intercept beta0
}

# Timing from tictoc:
## Sims with beta0 = -2.7: 212.347 sec elapsed
## Sims with beta0 = -2.2: 208.11 sec elapsed
## Sims with beta0 = -1.1: 199.923 sec elapsed