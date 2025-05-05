# Loading packages and functions -----------------------------------------------
suppressPackageStartupMessages(library(gamlss))
suppressPackageStartupMessages(library(dplyr))

# Functions for Estimating the Parameters of Distributions
source("Estimation_gamlss.R")

# Function to extract fit quality metrics
extract_fit <- function(estimation) {
  c(estimation$P.deviance, estimation$aic, estimation$sbc)
}

# Importing Data ---------------------------------------------------------------
library(readr)

data_soil <- read_csv("data_sets/updated_data.csv")
View(data_soil)
data<-data_soil


# --------------

# Define beta and gamma
mu <- c(1.2, -0.5, 0.7)
sigma <- c(0.4, -1)

# Use log link
log_link <- make.link("log")

# BETA Original (log | logit)
estimation_beta <- fitted_dist(y = data$sm_tgt, family = "BEo")
fit1 <- extract_fit(estimation_beta)

# Beta reparametrizada na média (logit | logit)
estimation_betamean <- fitted_dist(y = data$sm_tgt, family = "BE")
fit2 <- extract_fit(estimation_betamean)

# Simplex (logit | log)
estimation_simplex <- fitted_dist(y = data$sm_tgt, family = "SIMPLEX")
fit3 <- extract_fit(estimation_simplex)

# Unit Gamma (logit | log)
estimation_ugamma <- fitted_dist(y = data$sm_tgt, family = "UG")
fit4 <- extract_fit(estimation_ugamma)

# Unit Lindley (logit)
estimation_ulindley <- fitted_dist(y = data$sm_tgt, family = "UL")
fit5 <- extract_fit(estimation_ulindley)

# Unit Weibull (logit | log)
estimation_uweibull <- fitted_dist(y = data$sm_tgt, family = "UW")
fit6 <- extract_fit(estimation_uweibull)

# Kumaraswamy (logit | log)
estimation_kumaraswamy <- fitted_dist(y = data$sm_tgt, family = "KW")
fit7 <- extract_fit(estimation_kumaraswamy)

# Unit Chen (logit | log)
estimation_uchen <- fitted_dist(y = data$sm_tgt, family = "UC")
fit8 <- extract_fit(estimation_uchen)

# Unit Triangular (logit)
estimation_utriangular <- fitted_dist(y = data$sm_tgt, family = "TRI")
fit9 <- extract_fit(estimation_utriangular)

# Unit Gompertz (logit | log)
estimation_ugompertz <- fitted_dist(y = data$sm_tgt, family = "UGo")
fit10 <- extract_fit(estimation_ugompertz)


# Comparing Fits ---------------------------------------------------------------

names <- c(
  "Beta", "Beta_m", "Simplex", "UGamma", "ULindley", "UWeibull", "Kumaraswamy",
  "UChen", "Triangular", "UGompertz"
)
results <- matrix(NA, ncol = 3, nrow = 10)
colnames(results) <- c("GD", "AIC", "SBC")
rownames(results) <- names

results[1, ] <- fit1
results[2, ] <- fit2
results[3, ] <- fit3
results[4, ] <- fit4
results[5, ] <- fit5
results[6, ] <- fit6
results[7, ] <- fit7
results[8, ] <- fit8
results[9, ] <- fit9
results[10, ] <- fit10

results

