# Loading packages and functions -----------------------------------------------
suppressPackageStartupMessages(library(gamlss))
suppressPackageStartupMessages(library(dplyr))
library(lmtest)
library(readr)
library(lubridate)

# Functions for Estimating the Parameters of Distributions
source("Estimation_gamlss.R")
source("resid_analisys.R")

# Function to extract fit quality metrics
extract_fit <- function(estimation) {
  c(estimation$P.deviance, estimation$aic, estimation$sbc)
}

# Importing Data ---------------------------------------------------------------

dados <- readr::read_csv("data_sets/Sleep_Efficiency.csv") |> 
  dplyr::filter(!is.na(Awakenings)
                & !is.na(`Alcohol consumption`)) |> 
  dplyr::mutate(
    # Dicotomizando as variáveis
    Gender = dplyr::case_when(
    Gender == "Female" ~ 1,
    TRUE ~ 0
  ),
  `Alcohol consumption` = dplyr::case_when(
    `Alcohol consumption` == 0 ~ 0,
    TRUE ~ 1),
  `Exercise frequency` = dplyr::case_when(
    `Exercise frequency` == 0 ~ 0,
    TRUE ~ 1
  ),
  `Smoking status`= dplyr::case_when(
    `Smoking status` == "Yes" ~ 1,
    TRUE ~ 0),
  # Transformando porcentagem em proporção
  `REM sleep percentage` = `REM sleep percentage`/100,
  `Deep sleep percentage` = `Deep sleep percentage`/100,
  `Light sleep percentage` = `Light sleep percentage`/100
  ) |> 
  dplyr::rename(
    `REM sleep proportion` = `REM sleep percentage`,
    `Deep sleep proportion` = `Deep sleep percentage`,
    `Light sleep proportion` = `Light sleep percentage`
  ) |> 
  na.omit() |> 
  dplyr::select(
    - c(1,4,5)
  )

# Variável Resposta
rvar <- dados$`Sleep efficiency`
y <- dados$`Sleep efficiency`

# Matrizes de covariáveis
cov_mu <- dados |> 
  dplyr::select(
    -c(2,3,4,9,10,12)
  ) |> 
  as.matrix()

cov_sigma <- dados |> 
  dplyr::select(
    -c(1,2,3,4,5,6,7,9,10,11,12)
  ) |> 
  as.matrix()
 
cov <- dados |>
  dplyr::select(
    -c(2,3,4,5,9,12)
  ) |>
  as.matrix() 

# beta
# estimation_betamean <- fitted_dist(y = rvar, family = "BE", X = cov)
# fit2 <- extract_fit(estimation_betamean)

# beta
estimation_betamean <- gamlss(rvar ~ cov_mu, sigma.formula = ~ cov_sigma, family = BE(), trace = F, method = RS())

plot(estimation_betamean)
shapiro.test(estimation_betamean$residuals)
summary(estimation_betamean)
wp(estimation_betamean)

# UQC
rvar <- dados$`Sleep efficiency`
cov <- dados |>
  dplyr::select(
    -c(2,3,4,5,6,9,12)
  ) |>
  as.matrix() 
which(abs(estimation_UQC$residuals) > 3)

cov2 <- cov[-c(51,107),]  
rvar2 <- rvar[-c(51,107)]


estimation_UQC <- gamlss(rvar2 ~ cov2, sigma.formula = ~ 1, family = UQC(), trace = F, method = RS())

plot(estimation_UQC)
shapiro.test(estimation_UQC$residuals)
summary(estimation_UQC)
wp(estimation_UQC)

# SIMPLEX

estimation_SIMPLEX <- gamlss(rvar ~ cov_mu, sigma.formula = ~ cov_sigma, family = SIMPLEX(), trace = F, method = RS())

plot(estimation_SIMPLEX)
shapiro.test(estimation_SIMPLEX$residuals)
summary(estimation_SIMPLEX)
wp(estimation_SIMPLEX)

# KW

estimation_KW <- gamlss(rvar ~ cov_mu, sigma.formula = ~ 1, family = KW(), trace = F, method = RS())

plot(estimation_KW)
shapiro.test(estimation_KW$residuals)
summary(estimation_KW)
wp(estimation_KW)

# UG
rvar <- dados$`Sleep efficiency`
cov <- dados |>
  dplyr::select(
    -c(2,3,4,5,9,12)
  ) |>
  as.matrix() 

cov2 <- cov[-51,]  
rvar2 <- rvar[-51]

estimation_UG <- gamlss(rvar2 ~ cov2, sigma.formula = ~ 1, family = UG(), trace = F, method = RS())

plot(estimation_UG)
shapiro.test(estimation_UG$residuals)
summary(estimation_UG)
wp(estimation_UG)


# Modelos ----------------------------------------------------------------------

# BETA Original (log | logit)
# estimation_beta <- fitted_dist(y = rvar, family = "BEo", X = cov)
# fit1 <- extract_fit(estimation_beta)

# Beta reparametrizada na média (logit | logit)
estimation_betamean <- fitted_dist(y = rvar, family = "BE", X = cov)
fit2 <- extract_fit(estimation_betamean)

# Simplex (logit | log)
estimation_simplex <- fitted_dist(y = rvar, family = "SIMPLEX", X = cov)
fit3 <- extract_fit(estimation_simplex)

# Unit Gamma (logit | log)
estimation_ugamma <- fitted_dist(y = rvar, family = "UG", X = cov)
fit4 <- extract_fit(estimation_ugamma)
# Unit Lindley (logit)
estimation_ulindley <- fitted_dist(y = rvar, family = "UL",X = cov)
fit5 <- extract_fit(estimation_ulindley)

# Unit Weibull (logit | log)
estimation_uweibull <- fitted_dist(y = rvar, family = "UW",X = cov)
fit6 <- extract_fit(estimation_uweibull)

# Kumaraswamy (logit | log)
estimation_kumaraswamy <- fitted_dist(y = rvar, family = "KW", X = cov)
fit7 <- extract_fit(estimation_kumaraswamy)

# Unit Chen (log | log)
estimation_uchen <- fitted_dist(y = rvar, family = "UC", X = cov)
fit8 <- extract_fit(estimation_uchen)

# Unit Triangular (logit)
estimation_utriangular <- fitted_dist(y = rvar, family = "TRI", X = cov)
fit9 <- extract_fit(estimation_utriangular)

# Unit Gompertz (logit | log)
estimation_ugompertz <- fitted_dist(y = rvar, family = "UGo", X = cov)
fit10 <- extract_fit(estimation_ugompertz)

# Unit Quantile Chen (logit | log)
estimation_uqchen <- fitted_dist(y = rvar, family = "UQC", X = cov)
fit11 <- extract_fit(estimation_uqchen)

# Comparing Fits ---------------------------------------------------------------

names <- c(
  "Beta", "Beta_m", "Simplex", "UGamma", "ULindley", "UWeibull", "Kumaraswamy",
  "UChen", "Triangular", "UGompertz", "UQChen"
)
results <- matrix(NA, ncol = 3, nrow = 11)
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
results[11, ] <- fit11

results
