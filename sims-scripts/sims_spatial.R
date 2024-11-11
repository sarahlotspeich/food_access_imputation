# Install libraries
## Run once
## if needed: install.packages("devtools") 
## then: 
## devtools::install_github("sarahlotspeich/possum", 
##                          ref = "main") 

# Load libraries
library(possum) ## for multiple imputation estimator
library(tictoc) ## to calculate runtime
library(spaMM) ## for spatial generalized mixed models 

# Random seed to be used for each simulation setting
sim_seed = 11422

# Number of replicates per simulation setting
num_reps = 1000
## Note: You may want to run this code was run in parallel a cluster instead of locally, as it can be slow.

# Set parameters that won't be varied in the loop
## These values will be set as the defaults in the sim_data() function for convenience
### Some were based on the Piedmont Triad data 
beta0 = -2.2 ## outcome model intercept (leads to ~ 11% prevalence, based on diabetes)
muU = -0.7 ## error mean
sigmaU = 0.8 ## error standard deviation
lambdaPOP = 4095 ## average population per census tract
tau = sqrt(0.07) ## standard deviation of random intercepts (based on diabetes)
### And others were experimental 
beta1 = log(1.01) ## log prevalence ratio for X on Y
q = 0.1 ## proportion of neighborhoods to be queried
rho = 0.9 ## correlation (to approximate rho = 1 in the ICAR)

# Load adjacency matrices for Piedmont Triad and North Carolina 
ptW = readRDS(file = gzcon(url("https://github.com/sarahlotspeich/food_access_imputation/raw/main/piedmont-triad-data/piedmont_adjacency_matrix.Rds")))
ncW = readRDS(file = gzcon(url("https://github.com/sarahlotspeich/food_access_imputation/raw/main/piedmont-triad-data/nc_adjacency_matrix.Rds")))
## Exclude one NC census tract with no neighbors (makes covariance matrix not full rank)
ncW = ncW[rowSums(ncW) > 0, rowSums(ncW) > 0]

## Rename rows/columns
rownames(ptW) = colnames(ptW) = 1:nrow(ptW)
rownames(ncW) = colnames(ncW) = 1:nrow(ncW)

## And create matrix D with rowsums from the adjacency matrix
ptD = diag(rowSums(ptW))
ncD = diag(rowSums(ncW))

# --------------------------------------------------------------------
# Function to simulate data (arguments defined as follows)
## N = number of neighborhoods (sample size)
## tau = standard deviation of the random intercept
# --------------------------------------------------------------------
sim_data = function(N, tau) {
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
  
  ## Define the covariance matrix for the random intercepts 
  ptCovR = tau ^ 2 * solve(ptD - rho * ptW)
  ncCovR = tau ^ 2 * solve(ncD - rho * ncW)
  
  ## Simulate random intercept based on neighbors 
  if (N == 387) { ### based on Piedmont Triad adjacency
    R = MASS::mvrnorm(n = N, 
                      mu = rep(0, N), 
                      Sigma = ptCovR)
  } else if (N == 2169) { ### based of NC adjacency
    R = MASS::mvrnorm(n = N, 
                      mu = rep(0, N),
                      Sigma = ncCovR)
  }

  ## Simulate cases of health outcome
  lambda = exp(beta0 + beta1 * X + R)
  Y = rpois(n = N, 
            lambda = POP * lambda)
  
  ## Create dataset
  dat = data.frame(id = 1:N, X, Xstar, POP, Y)
  
  # Return dataset
  return(dat)
}

# Loop over different sample sizes: N = 387 (Piedmont Triad), 2169 (all of NC)
for (N in c(387, 2168)) {
  # Set appropriate adjacency matrix for the sample size N
  if (N == 387) {
    adj_mat = ptW ## Piedmont Triad
  } else if (N == 2169) {
    adj_mat = ncW ## North Carolina
  }
  
  tic(paste("Sims with N =", N)) ## Start counting runtime for sims with current sample size N
  # And random intercept standard deviation: 0, sqrt(0.01), sqrt(0.07), 1
  for (tau in c(0, sqrt(0.01), sqrt(0.07), 1)){
    # Be reproducible
    set.seed(sim_seed) ## set random seed
    
    # Create dataframe to save results for setting
    sett_res = data.frame(sim = paste(sim_seed, 1:num_reps, sep = "-"), 
                          N, beta0 = beta0, beta1 = beta1, muU = muU, sigmaU = sigmaU, q = q, tau = tau, avg_prev = NA, ## simulation setting
                          beta0_gs = NA, se_beta0_gs = NA, beta1_gs = NA, se_beta1_gs = NA, ## gold standard analysis
                          beta0_n = NA, se_beta0_n = NA, beta1_n = NA, se_beta1_n = NA, ## naive analysis
                          beta0_cc = NA, se_beta0_cc = NA, beta1_cc = NA, se_beta1_cc = NA, ## complete case analysis
                          beta0_imp = NA, se_beta0_imp = NA, beta1_imp = NA, se_beta1_imp = NA ## imputation analysis
    )
    
    # Loop over replicates 
    for (r in 1:num_reps) {
      # Generate data
      dat = sim_data(N  = N, ## sample size
                     tau = tau) ## random intercept standard deviation
      
      # Save average neighborhood prevalence
      sett_res$avg_prev[r] = mean(dat$Y / dat$POP)
      
      # Fit the gold standard model
      fit_gs = suppressMessages(
        fitme(formula = Y ~ X + adjacency(1 | id) + offset(log(POP)), 
              family = poisson(link = "log"), 
              adjMatrix = adj_mat,
              data = dat)
      )
      coefs = as.data.frame(summary(fit_gs)$beta_table) ## make table of fixed effects estimates
      sett_res[r, c("beta0_gs", "beta1_gs")] = coefs[, "Estimate"] ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_gs", "se_beta1_gs")] = coefs[, "Cond. SE"] ## and its standard error
      
      # Fit the gold standard model
      fit_n = suppressMessages(
        fitme(formula = Y ~ Xstar + adjacency(1 | id) + offset(log(POP)), 
              family = poisson(link = "log"), 
              adjMatrix = adj_mat,
              data = dat)
      )
      coefs = as.data.frame(summary(fit_n)$beta_table) ## make table of fixed effects estimates
      sett_res[r, c("beta0_n", "beta1_n")] = coefs[, "Estimate"] ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_n", "se_beta1_n")] = coefs[, "Cond. SE"] ## and its standard error
 
      # Select subset of neighborhoods/rows for map-based measures
      query_rows = sample(x = 1:N, 
                          size = ceiling(q * N), 
                          replace = FALSE)
      
      # Make X NA/missing for rows not in selected subset (query_rows)
      dat[!(dat$id %in% query_rows), "X"] = NA 
      
      # Fit the complete case model
      fit_cc = suppressMessages(
        fitme(formula = Y ~ X + adjacency(1 | id) + offset(log(POP)), 
              family = poisson(link = "log"), 
              adjMatrix = adj_mat,
              data = dat)
      )
      coefs = as.data.frame(summary(fit_cc)$beta_table) ## make table of fixed effects estimates
      sett_res[r, c("beta0_cc", "beta1_cc")] = coefs[, "Estimate"] ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_cc", "se_beta1_cc")] = coefs[, "Cond. SE"] ## and its standard error
  
      # Fit the pooled MI model
      fit_imp = impPossum(imputation_formula = X ~ Xstar + log(Y), 
                          analysis_formula = Y ~ X + adjacency(1 | id) + offset(log(POP)), 
                          data = dat, 
                          adjMatrix = adj_mat,
                          B = 20)
      sett_res[r, c("beta0_imp", "beta1_imp")] = fit_imp$Estimate ## estimated log prevalence ratio
      sett_res[r, c("se_beta0_imp", "se_beta1_imp")] = fit_imp$Standard.Error ## and its standard error
      
      # Save results
      write.csv(x = sett_res,
                file = paste0("spatial/proximity_N", N, "_tau", 100 * tau, "_seed", sim_seed, ".csv"), 
                row.names = F)
    }
  }
  toc() ## End runtime for sims with current sample size N
}

# Timing from tictoc:
## Sims with N = 387: 168.67 sec elapsed
## Sims with N = 2169: 520.163 sec elapsed