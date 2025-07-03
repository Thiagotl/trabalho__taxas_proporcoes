# Loading packages and functions -----------------------------------------------
library(gamlss)
library(dplyr)
library(lubridate)
options(OutDec=",") # option to save plots with "," as decimal separator

# Function for Estimation of Distributions
source("Estimation_gamlss.R")
# Function to plot residual analisys
# source("resid_analisys.R")

# Function to extract fit quality metrics
extract_fit <- function(estimation) {
  c(estimation$P.deviance, estimation$aic, estimation$sbc, Rsq(estimation, type = "Cox Snell"))
}

# Q-Q plot
# ggplot(data = data.frame(y = dados), aes(sample = y)) +
#   stat_qq(color = "black") +
#   stat_qq_line(color = "black") +
#   labs(title = "",
#        x = "Quantis Teóricos",
#        y = "Quantis Amostrados") +
#   theme_minimal(base_size = 14) +
#   theme(
#     panel.grid = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA, size = 1), # Adiciona borda
#     axis.line = element_blank()
#   )


# function to save plot.pdf
# ggsave("qqplot_dist.pdf", width = 6, height = 4, units = "in")


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

# General covariable Matrice

# matrix for both parameters
cov <- dados |>
  dplyr::select(
    # Removing response variable
    -4,
  ) |>
  filter(row_number() != 51) |>
  as.matrix()


# Fitting Best Model -----------------------------------------------------------

# Beta mean (mean / scale)
estimation_betamean <- gamlss(rvar ~ cov_mu, sigma.formula = ~cov_sigma, family = BE(), trace = F, method = RS())
fit1 <- extract_fit(estimation_betamean)

# residual analisys
plot(estimation_betamean)
shapiro.test(estimation_betamean$residuals)
summary(estimation_betamean)
wp(estimation_betamean)

# SIMPLEX (mean / scale)
estimation_SIMPLEX <- gamlss(rvar ~ cov_mu, sigma.formula = ~cov_sigma, family = SIMPLEX(), trace = F, method = RS())
fit2 <- extract_fit(estimation_SIMPLEX)

# residual analisys
plot(estimation_SIMPLEX)
shapiro.test(estimation_SIMPLEX$residuals)
summary(estimation_SIMPLEX)
wp(estimation_SIMPLEX)

# Unit Gamma (mean / precision)

# matrix for mu
cov_mu <- dados |>
  dplyr::select(
    # Removing response variable
    -4,
    # Removing useless covariables
    # -c(1,2,3,9,10,12)
    -c(2, 3, 9, 12)
  ) |>
  filter(row_number() != 51) |>
  as.matrix()

# matrix for sigma
cov_sigma <- dados |>
  dplyr::select(
    # Removing response variable
    -4,
    # Removing useless covariables
    -c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12)
  ) |>
  filter(row_number() != 51) |>
  as.matrix()


estimation_UG <- gamlss(rvar ~ cov_mu, sigma.formula = ~cov_sigma, family = UG(), trace = F, method = RS())
fit3 <- extract_fit(estimation_UG)

# residual analisys
plot(estimation_UG)
shapiro.test(estimation_UG$residuals)

summary(estimation_UG)
wp(estimation_UG)


# Unit Weibull (Quantile / shape)
estimation_UW <- gamlss(rvar ~ cov_mu, sigma.formula = ~cov_sigma, family = UW(), trace = F, method = RS())
fit4 <- extract_fit(estimation_UW)

# residual analisys
plot(estimation_UW)
shapiro.test(estimation_UW$residuals)
summary(estimation_UW)
wp(estimation_UW)


# Kumaraswamy (median / dispersion)
estimation_KW <- gamlss(rvar ~ cov_mu, sigma.formula = ~cov_sigma, family = KW(), trace = F, method = RS())
fit5 <- extract_fit(estimation_KW)

# residual analisys
plot(estimation_KW)
shapiro.test(estimation_KW$residuals)
summary(estimation_KW)
wp(estimation_KW)

# Unit Quantile Chen (Quantile / Shape)
estimation_UQC <- gamlss(rvar ~ cov_mu, sigma.formula = ~cov_sigma, family = UQC(), trace = F, method = RS())
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

# Models -----------------------------------------------------------------------

# Beta (log | logit) OBS: fit failing
# estimation_beta <- fitted_dist(y = rvar, family = "BEo", X = cov)
# fit1 <- extract_fit(estimation_beta)

# Beta mean parameterization (logit | logit)
# estimation_betamean <- fitted_dist(y = rvar, family = "BE", X = cov)
# fit2 <- extract_fit(estimation_betamean)

# Simplex (logit | log)
# estimation_simplex <- fitted_dist(y = rvar, family = "SIMPLEX", X = cov)
# fit3 <- extract_fit(estimation_simplex)

# Unit Gamma (logit | log)
# estimation_ugamma <- fitted_dist(y = rvar, family = "UG", X = cov)
# fit4 <- extract_fit(estimation_ugamma)

# Unit Lindley (logit)
# estimation_ulindley <- fitted_dist(y = rvar, family = "UL", X = cov)
# fit5 <- extract_fit(estimation_ulindley)

# Unit Weibull (logit | log)
# estimation_uweibull <- fitted_dist(y = rvar, family = "UW", X = cov)
# fit6 <- extract_fit(estimation_uweibull)

# Kumaraswamy (logit | log)
# estimation_kumaraswamy <- fitted_dist(y = rvar, family = "KW", X = cov)
# fit7 <- extract_fit(estimation_kumaraswamy)

# Unit Chen (log | log)
# estimation_uchen <- fitted_dist(y = rvar, family = "UC", X = cov)
# fit8 <- extract_fit(estimation_uchen)

# Unit Triangular (logit)
# estimation_utriangular <- fitted_dist(y = rvar, family = "TRI", X = cov)
# fit9 <- extract_fit(estimation_utriangular)

# Unit Gompertz (logit | log)
# estimation_ugompertz <- fitted_dist(y = rvar, family = "UGo", X = cov)
# fit10 <- extract_fit(estimation_ugompertz)

# Unit Quantile Chen (logit | log)
# estimation_uqchen <- fitted_dist(y = rvar, family = "UQC", X = cov)
# fit11 <- extract_fit(estimation_uqchen)

# Reflected Unit Burr XII (logit | log)
# estimation_uBXII <- fitted_dist(y = rvar, family = "RUBXII", X = cov)
# fit12 <- extract_fit(estimation_uBXII)

# Comparing Fits ---------------------------------------------------------------

# names <- c(
#   "Beta", "Beta_m", "Simplex", "UGamma", "ULindley", "UWeibull", "Kumaraswamy",
#   "UChen", "Triangular", "UGomportez", "UQChen", "UBurrXII"
# )
# results <- matrix(NA, ncol = 4, nrow = 12)
# colnames(results) <- c("GD", "AIC", "SBC", "Pseudo-R^2")
# rownames(results) <- names
# 
# results[1, ] <- fit1
# results[2, ] <- fit2
# results[3, ] <- fit3
# results[4, ] <- fit4
# results[5, ] <- fit5
# results[6, ] <- fit6
# results[7, ] <- fit7
# results[8, ] <- fit8
# results[9, ] <- fit9
# results[10, ] <- fit10
# results[11, ] <- fit11
# results[12, ] <- fit12
# 
# results
