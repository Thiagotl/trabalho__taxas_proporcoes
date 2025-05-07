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

library(readr)
Sleep_Efficiency <- read_csv("data_sets/Sleep_Efficiency.csv")
View(Sleep_Efficiency)


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
    -c(1,2,3,4,9,12)
  ) |> 
  as.matrix()

cov_sigma <- dados |> 
  dplyr::select(
    -c(2,3,4,9,10,11,12)
  ) |> 
  as.matrix()

cov <- dados |> 
  dplyr::select(
    -c(2,3,4)
  ) |> 
  as.matrix()

# Unit Chen (logit | log)
# estimation_uchen <- fitted_dist(y = rvar, family = "UC", X = cov)
# fit8 <- extract_fit(estimation_uchen)

estimation_uchen <- gamlss(rvar ~ cov, sigma.formula = ~ 1, family = UC(), trace = F, method = RS())

plot(estimation_uchen)

shapiro.test(estimation_uchen$residuals)
summary(estimation_uchen)

wp(estimation_uchen, )

source("influence.R")

library(gamlss)
dados2<-as.matrix(dados)

summary.gamlss(estimation_uchen)
residuals(estimation_uchen)
prodist(estimation_uchen)

residuals<-resid(estimation_uchen)

X <- model.matrix(estimation_uchen, what = "mu")
W <- estimation_uchen$weights


cook_distance <- (residuals^2 * leverage) / (length(coef(model)) * (1 - leverage))
W_sqrt <- sqrt(W)                     # W^{1/2}
X_weighted <- W_sqrt * X              # W^{1/2} X

cor(X[, -1])

library(MASS)
H <- X_weighted %*% ginv(t(X) %*% (W * X)) %*% t(X_weighted)
lambda <- 1e-6
XTWX <- t(X) %*% (W * X)
XTWX_reg <- XTWX + lambda * diag(ncol(X))
H <- X_weighted %*% solve(XTWX_reg) %*% t(X_weighted)
leverage <- diag(H)

plot(leverage, main = "Leverage: Pseudoinversa vs Regularização")
abline(h = 2 * mean(leverage), col = "red", lty = 2) 

h = 2 * mean(leverage)
which(leverage>h)


cook_distance <- (residuos^2 * leverage) / (length(coef(model)) * (1 - leverage)^2)

#fit <- gamlss(rvar ~ cov, sigma.formula = ~ cov, family = UC(), trace = F, method = CG())


# summary(fit)

# BETA Original (log | logit)
estimation_beta <- fitted_dist(y = rvar, family = "BEo", X = cov)
fit1 <- extract_fit(estimation_beta)

# Beta reparametrizada na média (logit | logit)
estimation_betamean <- fitted_dist(y = rvar, family = "BE", X = cov)
fit2 <- extract_fit(estimation_betamean)
plot(estimation_betamean)

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




# # Unit Triangular (logit)
estimation_utriangular <- fitted_dist(y = rvar, family = "TRI", X = cov)
fit9 <- extract_fit(estimation_utriangular)

# Unit Gompertz (logit | log)
estimation_ugompertz <- fitted_dist(y = rvar, family = "UGo", X = cov)
fit10 <- extract_fit(estimation_ugompertz)

plot(estimation_ugompertz)

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

# ----------------







