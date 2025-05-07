# Loading packages and functions -----------------------------------------------
suppressPackageStartupMessages(library(gamlss))
suppressPackageStartupMessages(library(dplyr))
library(lmtest)

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

data_soil <- read_csv("data_sets/updated_data.csv")
#View(data_soil)
y<-data_soil$sm_tgt


 

data_set<-data_soil |> 
  mutate(data = ymd(time),
         mes = month(data),
         dia = day(data)) |> 
  filter(mes == 1, 
         dia == 10) |> 
  select(-sm_aux)

cov <- data_set |> 
  dplyr::select(4:6) |> 
  as.matrix()

# --------------

# Use log link
log_link <- make.link("log")

# fit <- gamlss(data_set$sm_tgt ~ cov, sigma.formula = ~ cov, family = UC(), trace = F, method = RS())
# 
# summary(fit)

# BETA Original (log | logit)
# estimation_beta <- fitted_dist(y = data_set$sm_tgt, family = "BEo")
# fit1 <- extract_fit(estimation_beta)

# Beta reparametrizada na média (logit | logit)
estimation_betamean <- fitted_dist(y = data_set$sm_tgt, family = "BE", X = cov)
fit2 <- extract_fit(estimation_betamean)

# Simplex (logit | log)
estimation_simplex <- fitted_dist(y = data_set$sm_tgt, family = "SIMPLEX", X = cov)
fit3 <- extract_fit(estimation_simplex)

# Unit Gamma (logit | log)
estimation_ugamma <- fitted_dist(y =data_set$sm_tgt, family = "UG", X = cov)
fit4 <- extract_fit(estimation_ugamma)
# Unit Lindley (logit)
estimation_ulindley <- fitted_dist(y = data_set$sm_tgt, family = "UL",X = cov)
fit5 <- extract_fit(estimation_ulindley)

# Unit Weibull (logit | log)
estimation_uweibull <- fitted_dist(y = data_set$sm_tgt, family = "UW",X = cov)
fit6 <- extract_fit(estimation_uweibull)

# Kumaraswamy (logit | log)
estimation_kumaraswamy <- fitted_dist(y = data_set$sm_tgt, family = "KW", X = cov)
fit7 <- extract_fit(estimation_kumaraswamy)

# Unit Chen (logit | log)
estimation_uchen <- fitted_dist(y = data_set$sm_tgt, family = "UC", X = cov)
fit8 <- extract_fit(estimation_uchen)
resid.analisys(resid=estimation_uchen$residuals,plot.type=c("plot"))
# resid.analisys(resid=estimation_uchen$residuals,plot.type=c("FAC"),lag.max=30)
resid.analisys(resid=estimation_uchen$residuals,plot.type=c("FACP"),lag.max=30)
bartlett.test(estimation_uchen$residuals)

dwtest(estimation_uchen)
bptest(fit, studentize = TRUE)

resid.analisys(resid=estimation_uchen$residuals,plot.type=c("densidade"))
shapiro.test(estimation_uchen$residuals)
resid.analisys(resid=estimation_uchen$residuals,plot.type=c("envelope"))
resid.analisys(resid=estimation_uchen$residuals,plot.type=c("residuo"))
fitted <- data_set$sm_tgt - estimation_uchen$residuals
resid.analisys(resid=estimation_uchen$residuals,fitted,plot.type=c("resxfit"))
resid.analisys(data=y,fitted,plot.type=c("realxfit"))


wp(estimation_uchen)
plot(estimation_uchen)



# # Unit Triangular (logit)
estimation_utriangular <- fitted_dist(y = data_set$sm_tgt, family = "TRI", X = cov)
fit9 <- extract_fit(estimation_utriangular)

# Unit Gompertz (logit | log)
estimation_ugompertz <- fitted_dist(y = data_set$sm_tgt, family = "UGo", X = cov)
fit10 <- extract_fit(estimation_ugompertz)
#Comandos----
resid.analisys(resid=estimation_ugompertz$residuals,plot.type=c("plot"))
resid.analisys(resid=estimation_ugompertz$residuals,plot.type=c("FAC"),lag.max=30)
resid.analisys(resid=estimation_ugompertz$residuals,plot.type=c("FACP"),lag.max=30)
resid.analisys(resid=estimation_ugompertz$residuals,plot.type=c("densidade"))
resid.analisys(resid=estimation_ugompertz$residuals,plot.type=c("envelope"))
resid.analisys(resid=estimation_ugompertz$residuals,plot.type=c("residuo"))
fitted <- data_set$sm_tgt - estimation_ugompertz$residuals
resid.analisys(resid=estimation_ugompertz$residuals,fitted,plot.type=c("resxfit"))
resid.analisys(data=y,fitted,plot.type=c("realxfit"))

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

