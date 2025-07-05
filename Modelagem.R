# Loading packages and functions -----------------------------------------------
library(gamlss)
library(dplyr)
library(ggplot2)
library(lubridate)
options(OutDec=",") # opção para salvar plots com "," como separador decimal

# Function for Estimation of Distributions
source("Estimation_gamlss.R")
# Function to plot residual analisys
# source("resid_analisys.R")

# Function to extract fit quality metrics
extract_fit <- function(estimation) {
  c(estimation$P.deviance, estimation$aic, estimation$sbc, Rsq(estimation, type = "Cox Snell"))
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

colnames(cov)<- c("Age","Gender","Sleep duration","REM",
                  "Deep", "Light", "Awakenings","Caffeine consumption" , 
                  "Alcohol consumption","Smoking status","Exercise frequency")



# Fitting Best Model -----------------------------------------------------------

# Beta mean (mean / scale)
mod_betamean <- gamlss(rvar ~ cov[,c(1,4,5,7,10)], sigma.formula = ~ cov[,c(7)], family = BE(), trace = F, method = RS(),
                    control = gamlss.control(n.cyc = 200,trace = F))

fit1 <- extract_fit(mod_betamean)
summary(mod_betamean)

# residual analisys
shapiro.test(mod_betamean$residuals)
# plot(mod_betamean)
wp(mod_betamean)

# function to save plot.pdf
ggsave("wormplot_beta.pdf", width = 6, height = 4, units = "in")

# Q-Q plot dos resíduos do modelo
ggplot(data = data.frame(resid = residuals(mod_betamean)), aes(sample = resid)) +
  stat_qq(color = "black") +
  stat_qq_line(color = "black") +
  labs(title = "s",
       x = "Quantis Teóricos",
       y = "Quantis Amostrais") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_blank()
  )

# function to save plot.pdf
ggsave("qqplot_beta.pdf", width = 6, height = 4, units = "in")

# SIMPLEX (mean / scale)
estimation_SIMPLEX <- gamlss(rvar ~ cov_mu, sigma.formula = ~cov_sigma, family = SIMPLEX(), trace = F, method = RS())

mod_simplex <- gamlss(rvar ~ cov[, c(4,5,7,10)], sigma.formula = ~ cov[,c(5,7)], family = SIMPLEX(), trace = F, method = RS(),
                      control = gamlss.control(n.cyc = 200,trace = F))

fit2 <- extract_fit(mod_simplex)
summary(mod_simplex)

# residual analisys
shapiro.test(mod_simplex$residuals)
plot(mod_simplex)

wp(mod_simplex)

# function to save plot.pdf
ggsave("wormplot_simplex.pdf", width = 6, height = 4, units = "in")

# Q-Q plot dos resíduos do modelo
ggplot(data = data.frame(resid = residuals(mod_simplex)), aes(sample = resid)) +
  stat_qq(color = "black") +
  stat_qq_line(color = "black") +
  labs(title = "s",
       x = "Quantis Teóricos",
       y = "Quantis Amostrais") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_blank()
  )

# function to save plot.pdf
ggsave("qqplot_simplex.pdf", width = 6, height = 4, units = "in")

# Unit Gamma (mean / precision)
mod_gamma <- gamlss(rvar ~ cov[,-c(2,3,8,11)], sigma.formula = ~ cov[,c(5)], family = UG(), trace = F, method = RS(),
                    control = gamlss.control(n.cyc = 200,trace = F))

summary(mod_gamma)
fit3 <- extract_fit(mod_gamma)

# residual analisys
shapiro.test(mod_gamma$residuals)
plot(mod_gamma)

wp(mod_gamma)

# function to save plot.pdf
ggsave("wormplot_gamma.pdf", width = 6, height = 4, units = "in")

# Q-Q plot dos resíduos do modelo
ggplot(data = data.frame(resid = residuals(mod_gamma)), aes(sample = resid)) +
  stat_qq(color = "black") +
  stat_qq_line(color = "black") +
  labs(title = "s",
       x = "Quantis Teóricos",
       y = "Quantis Amostrais") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_blank()
  )

# function to save plot.pdf
ggsave("qqplot_gamma.pdf", width = 6, height = 4, units = "in")

# Unit Weibull (Quantile / shape)
mod_UW <- gamlss(rvar ~ cov[,c(1,4,5,7,10)], sigma.formula = ~ cov[,c(4,5,7)], family = UW(), trace = F, method = RS(),
                 control = gamlss.control(n.cyc = 200,trace = F))
fit4 <- extract_fit(mod_UW)

summary(mod_UW)

# residual analisys
plot(mod_UW)
shapiro.test(mod_UW$residuals)
wp(mod_UW)

# Kumaraswamy (median / dispersion)
# Com problemas
# mod_KW <- gamlss(rvar ~ cov[,c(1,4,5,7,9,10)], sigma.formula = ~ cov[,5], family = KW(), trace = F, method = RS(),
                 # control = gamlss.control(n.cyc = 200,trace = F))
# fit5 <- extract_fit(mod_KW)

# summary(mod_KW)

# residual analisys
# plot(mod_KW)
# shapiro.test(mod_KW$residuals)
# wp(mod_KW)

# Unit Quantile Chen (Quantile / Shape)
mod_UQC <-  gamlss(rvar ~ cov[,c(1,4,5,7,10)], sigma.formula = ~ cov[,c(5,7)], family = UQC(), trace = F, method = RS(),
                   control = gamlss.control(n.cyc = 200,trace = F))
fit6 <- extract_fit(mod_UQC)

summary(mod_UQC)

# residual analisys
plot(mod_UQC)
shapiro.test(mod_UQC$residuals)
wp(mod_UQC)

# Reflected Unit Burr XII (logit | log)
mod_UBURR <-  gamlss(rvar ~ cov[,-c(2,3,8,9,11)], sigma.formula = ~ 1, family = RUBXII(), trace = F, method = RS(),
                   control = gamlss.control(n.cyc = 200,trace = F))
fit7 <- extract_fit(mod_UBURR)

summary(mod_UBURR)

# residual analisys
plot(mod_UBURR)
shapiro.test(mod_UBURR$residuals)
wp(mod_UBURR)


# Unit Lindley (logit)
mod_Ulind <-  gamlss(rvar ~ cov[,-c(2,3,8,9,11)], family = UL(), trace = F, method = RS(),
                     control = gamlss.control(n.cyc = 200,trace = F))
fit8 <- extract_fit(mod_Ulind)

summary(mod_Ulind)

# residual analisys
plot(mod_Ulind)
shapiro.test(mod_Ulind$residuals)
wp(mod_Ulind)


# Summary of fits
names <- c(
  "Beta", "Simplex", "UGamma", "UWeibull", "Kumaraswamy", "UQChen", "UBURRXII"
)
results <- matrix(NA, ncol = 4, nrow = 7)
colnames(results) <- c("GD", "AIC", "SBC","Pseudo-R2")
rownames(results) <- names

results[1, ] <- fit1
results[2, ] <- fit2
results[3, ] <- fit3
results[4, ] <- fit4
results[5, ] <- fit5
results[6, ] <- fit6
results[7, ] <- fit7
results[8, ] <- fit8

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
