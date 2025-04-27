# Loading packages -------------------------------------------------------------
suppressPackageStartupMessages(library(gamlss))
suppressPackageStartupMessages(library(dplyr))

# Functions for Estimating the Parameters of Distributions
source("Estimation_gamlss.R")
# PDF and CDF Functions of Distributions
source("Dist_functions.R")

ruchen <- function(n, mu, sigma) {
  u <- runif(n)
  return(
    exp(-(log(1 - log(u) / mu)**(1 / sigma)))
  )
}

y <- ruchen(200,1,1)

# BETA Original
par_be <- fitted_dist(y = y, family = "BEo")
fit2 <- c(mod2$W, mod2$A, mod2$KS$statistic, mod2$AIC, mod2$BIC, mod2$HQIC)

# KW
source("KW.R")
par_kw <- fitted_dist(y = y, family = "KW")





# Análise dos Ajustes----
nomes <- c(
  "ACOSKW", "BETA", "KW", "MKW", "MRKW", "UBIRNBAUM", "UBURRXII", "UDAGUMI", "UDAGUMII", "UGAMMA",
  "UGHN", "UGOMPERTZ", "ULINDLEY", "ULINDLEYI", "UTOPPLEONY", "UTOPPLEONYI", "UWEIBULL", "SKW", "SUBURRXII"
)
kappa <- c(1:19)
results <- matrix(0, ncol = 6, nrow = length(kappa))
colnames(results) <- c("W", "A", "KS", "AIC", "BIC", "HQIC")
rownames(results) <- nomes[kappa]

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
results[12, ] <- fit12
results[13, ] <- fit13
results[14, ] <- fit14
results[15, ] <- fit15
results[16, ] <- fit16
results[17, ] <- fit17
results[18, ] <- fit18
results[19, ] <- fit19

# #Retira as linhas com zeros (toda nula)
# results_filtrado <- results[apply(results, 1, function(x) any(x != 0)), ]
#
# #Ordena a matriz pela coluna escolhida dentro do arrange
# results_ordenado <- results_filtrado %>%
#   as.data.frame() %>%  # Converter matriz em data frame
#   arrange(W) %>%      # Ordenar pela segunda coluna
#   as.matrix()          # Converter de volta para matriz
#
# results_ordenado

# Retirando as linhas com NAN
results_filtrado <- results[complete.cases(results), ]

# Retira as linhas com zeros (toda nula) e Ordenar por coluna
results_final <- results_filtrado %>%
  as.data.frame() %>% # Converte a matriz para data frame
  filter(apply(., 1, function(x) any(x != 0))) %>% # Filtra linhas sem zeros
  arrange(AIC) %>% # Ordena pela coluna <-Escolha da Coluna c("W","A","KS","AIC","BIC","HQIC")
  as.matrix() # Filtra a matriz

# Measures of Accuracy
results_final

results_final1 <- data.frame(results_final)


write.xlsx(results_final1, "RR_Mothers_Who_Received_Post_Natal_Check_Up_Within_1_Week_Of_Delivery_Total.xlsx", rowNames = TRUE)
