# Loading packages and functions -----------------------------------------------
suppressPackageStartupMessages(library(gamlss))
suppressPackageStartupMessages(library(dplyr))
library(lubridate)

# Functions for Estimation of Distributions
source("Estimation_gamlss.R")
source("resid_analisys.R")

# Function to extract fit quality metrics
extract_fit <- function(estimation) {
  c(estimation$P.deviance, estimation$aic, estimation$sbc)
}

# Importing Data ---------------------------------------------------------------

# Filtering dataset
dados <- readr::read_csv("data_sets/Sleep_Efficiency.csv") |>
  dplyr::filter(!is.na(Awakenings) &
                  !is.na(`Alcohol consumption`)) |>
  dplyr::mutate(
    # Dicotomizando as variáveis
    Gender = dplyr::case_when(
      Gender == "Female" ~ 1,
      TRUE ~ 0
    ),
    `Alcohol consumption` = dplyr::case_when(
      `Alcohol consumption` == 0 ~ 0,
      TRUE ~ 1
    ),
    `Exercise frequency` = dplyr::case_when(
      `Exercise frequency` == 0 ~ 0,
      TRUE ~ 1
    ),
    `Smoking status` = dplyr::case_when(
      `Smoking status` == "Yes" ~ 1,
      TRUE ~ 0
    ),
    # Transforming percentage to proportion
    `REM sleep percentage` = `REM sleep percentage` / 100,
    `Deep sleep percentage` = `Deep sleep percentage` / 100,
    `Light sleep percentage` = `Light sleep percentage` / 100
  ) |>
  # Renaming variables
  dplyr::rename(
    `REM sleep proportion` = `REM sleep percentage`,
    `Deep sleep proportion` = `Deep sleep percentage`,
    `Light sleep proportion` = `Light sleep percentage`
  ) |>
  # Removing NA's
  na.omit() |>
  # Removing useless variables
  dplyr::select(
    -c(1, 4, 5)
  )

# Response variable

rvar <- dados$`Sleep efficiency`[-51]
y <- dados$`Sleep efficiency`

# Covariable Matrices

# matrix for mu
cov_mu <- dados |>
  dplyr::select(
    # Removing response variable
    -4,
    # Removing useless covariables
    -c(2,3,9,12)
  ) |>
  filter(row_number() != 51) |>
  as.matrix()

# matrix for sigma
cov_sigma <- dados |>
  dplyr::select(
    # Removing response variable
    -4,
    # Removing useless covariables
    -c(1:7,9:12)
  ) |>
  filter(row_number() != 51) |>
  as.matrix()

# matrix for both parameters
cov <- dados |>
  dplyr::select(
    # Removing response variable
    -4,
    # Removing useless covariables
    # -c()
  ) |>
  filter(row_number() != 51) |>
  as.matrix()


# Fitting Best Model -----------------------------------------------------------

# Beta mean (mean / scale)
estimation_betamean <- gamlss(rvar ~ cov_mu, sigma.formula = ~ cov_sigma, family = BE(), trace = F, method = RS())
fit1 <- extract_fit(estimation_betamean)

# residual analisys
plot(estimation_betamean)
shapiro.test(estimation_betamean$residuals)
summary(estimation_betamean)
wp(estimation_betamean)

# SIMPLEX (mean / scale)
estimation_SIMPLEX <- gamlss(rvar2 ~ cov_mu, sigma.formula = ~ cov_sigma, family = SIMPLEX(), trace = F, method = RS())
fit2 <- extract_fit(estimation_SIMPLEX)

# residual analisys
plot(estimation_SIMPLEX)
shapiro.test(estimation_SIMPLEX$residuals)
summary(estimation_SIMPLEX)
wp(estimation_SIMPLEX)

# Unit Gamma (mean / precision)
estimation_UG <- gamlss(rvar2 ~ cov_mu, sigma.formula = ~ cov_sigma, family = UG(), trace = F, method = RS())
fit3 <- extract_fit(estimation_UG)

# residual analisys
plot(estimation_UG)
shapiro.test(estimation_UG$residuals)

summary(estimation_UG)
wp(estimation_UG)


# Unit Weibull (Quantile / shape)
estimation_UW <- gamlss(rvar ~ cov_mu, sigma.formula = ~ cov_sigma, family = UW(), trace = F, method = RS())
fit4 <- extract_fit(estimation_UW)

# residual analisys
plot(estimation_UW)
shapiro.test(estimation_UW$residuals)
summary(estimation_UW)
wp(estimation_UW)


# Kumaraswamy (median / dispersion)
estimation_KW <- gamlss(rvar ~ cov_mu, sigma.formula = ~ cov_sigma, family = KW(), trace = F, method = RS())
fit5 <- extract_fit(estimation_KW)

# residual analisys
plot(estimation_KW)
shapiro.test(estimation_KW$residuals)
summary(estimation_KW)
wp(estimation_KW)

# Unit Quantile Chen (Quantile / Shape)
estimation_UQC <- gamlss(rvar2 ~ cov_mu2, sigma.formula = ~ 1, family = UQC(), trace = F, method = RS())
fit6 <- extract_fit(estimation_UQC)

# residual analisys
plot(estimation_UQC)
shapiro.test(estimation_UQC$residuals)
summary(estimation_UQC)
wp(estimation_UQC)

names <- c(
  "Beta", "Simplex", "UGamma", "UWeibull", "Kumaraswamy", "UQChen"
)
results <- matrix(NA, ncol = 3, nrow = 6)
colnames(results) <- c("GD", "AIC", "SBC")
rownames(results) <- names

results[1, ] <- fit1
results[2, ] <- fit2
results[3, ] <- fit3
results[4, ] <- fit4
results[5, ] <- fit5
results[6, ] <- fit6

results
