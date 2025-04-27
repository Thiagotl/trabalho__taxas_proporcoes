# Loading packages and functions -----------------------------------------------
suppressPackageStartupMessages(library(gamlss))
suppressPackageStartupMessages(library(dplyr))

# Functions for Estimating the Parameters of Distributions
source("Estimation_gamlss.R")

# Function to extract fit quality metrics
extract_fit <- function(estimation) {
  c(estimation$P.deviance, estimation$aic, estimation$sbc)
}

# fit2 <- c(mod2$W, mod2$A, mod2$KS$statistic, mod2$AIC, mod2$BIC, mod2$HQIC)

data <- runif(1000)


# Estimando os ajustes ---------------------------------------------------------

# BETA Original
estimation_beta <- fitted_dist(y = data, family = "BEo")
fit1 <- extract_fit(estimation_beta)

# Beta reparametrizada na média
estimation_betamean <- fitted_dist(y = data, family = "BE")
fit2 <- extract_fit(estimation_betamean)

# Simplex
estimation_simplex <- fitted_dist(y = data, family = "SIMPLEX")
fit3 <- extract_fit(estimation_simplex)

# Unit Gamma
estimation_ugamma <- fitted_dist(y = data, family = "UG")
fit4 <- extract_fit(estimation_ugamma)

# Unit Lindley
estimation_ulindley <- fitted_dist(y = data, family = "UL")
fit5 <- extract_fit(estimation_ulindley)

# Unit Weibull
estimation_uweibull <- fitted_dist(y = data, family = "UW")
fit6 <- extract_fit(estimation_uweibull)

# Kumaraswamy
estimation_kumaraswamy <- fitted_dist(y = data, family = "KW")
fit7 <- extract_fit(estimation_kumaraswamy)

# Unit Chen
estimation_uchen <- fitted_dist(y = data, family = "UC")
fit8 <- extract_fit(estimation_uchen)

# Unit Triangular
estimation_utriangular <- fitted_dist(y = data, family = "TRI")
fit9 <- extract_fit(estimation_utriangular)

# Unit Gompertz
estimation_ugompertz <- fitted_dist(y = data, family = "UGo")
fit10 <- extract_fit(estimation_ugompertz)


# Comparing Fits ---------------------------------------------------------------

nomes <- c(
  "Beta", "Beta_m", "Simplex", "UGamma", "ULindley", "UWeibull", "Kumaraswamy",
  "UChen", "Triangular", "UGomportez"
)
results <- matrix(0, ncol = 3, nrow = 10)
colnames(results) <- c("GD", "AIC", "SBC")
rownames(results) <- nomes

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
