# Loading packages and functions -----------------------------------------------

library(gamlss)

source("UQchen.R") # UQC gamlss implementation

# function to calculate the estimator metrics
metrics <- function(true_values, mu_result, sigma_result) {
  
  # calculating estimation mean
  mean_values <- c( 
    apply(mu_result, 2, mean),
    apply(sigma_result, 2, mean)
  )
  # calculating estimation relative bias
  b_values <- (true_values - mean_values) / true_values * 100
  # calculating MSE
  eqm_values <- c( 
    apply(mu_result, 2, var),
    apply(sigma_result, 2, var)
  ) + (true_values - mean_values)^2
  skewness <- c( 
    moments::skewness(mu_result),
    moments::skewness(sigma_result)
  )
  kurtosis <- c( 
    moments::kurtosis(mu_result),
    moments::kurtosis(sigma_result)
  )
  # creating a results matrix
  result <- cbind(
    true_values,
    round(mean_values,3),
    round(b_values,3),
    round(eqm_values,3),
    round(skewness,3),
    round(kurtosis,3)
  )
  colnames(result) <- c("true value", "mean", "relative_bias", "eqm", "SK", "KUR") # metrics
  rownames(result) <- c("b1", "b2", "g1", "g2") # par names
  return(result)
}


# Simulation -------------------------------------------------------------------

# fixing scenario
{ set.seed(15) # fixing seed
  # n <- 100 # sample size
  ns <- c(50, 150, 250, 500) # sample sizes
  RS <- 55 # Monte Carlo replics simulated
  RR <- 50 # Monte Carlo replics required
  # Creating link functions
  logit_link <- make.link("logit")
  log_link <- make.link("log")
  # Fixing true par values
  b1 <- 1.6
  b2 <- -0.7
  g1 <- 0.5
  g2 <- -1
  true_values <- c(b1, b2, g1, g2)
  # Objects to save the estimation results
  mu_temp <- sigma_temp <- matrix(NA, RS, 2)
  fits <- list() # list to save all fits
  }

# Monte Carlo loop
for (j in 1:length(ns)) {
  X <- runif(ns[j])
  mu_true <- logit_link$linkinv(b1 + b2 * X)
  sigma_true <- log_link$linkinv(g1 + g2 * X)
  for (i in 1:RS) {
    y <- rUQC(ns[j], mu_true, sigma_true)
    fit <- try(gamlss(y ~ X,
      sigma.formula = ~X,
      family = UQC(mu.link = "logit", sigma.link = "log"),
      trace = F, method = RS()
    ), silent = TRUE)
    
    # if fit fails, return NA
    if (!inherits(fit, "try-error")) {
      mu_temp[i, ] <- fit$mu.coefficients 
      sigma_temp[i, ] <- fit$sigma.coefficients
    } else {
      mu_temp[i, ] <- rep(NA, 2)
      sigma_temp[i, ] <- rep(NA, 2)
    }
  }
  
  # Selecting only RR replics
  mu_result <- na.omit(mu_temp)[1:RR,]
  sigma_result <- na.omit(sigma_temp)[1:RR,]
  
  fits[[j]] <- metrics(true_values, mu_result, sigma_result)
}

fits


