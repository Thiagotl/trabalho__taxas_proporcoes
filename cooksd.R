cooksd_parallel <- function(fn, formula, data, family, n_cores = detectCores() - 1)
{
  library(foreach)
  library(doParallel)
  library(dplyr)
  library(gamlss)
  source("SUBURRXII_REG.R")
  
  # Configuração do paralelo
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Avalia o modelo completo
  m <- fn(formula, data=data, family=family, trace=FALSE)
  p <- exp(predict(m, newdata=data, data=data, what=c("mu")))
  
  coefficients <- coef(m)
  num_coefficients <- length(coefficients) + 1
  num_observations <- nrow(data)
  
  # Resíduos do modelo completo
  r <- p - m$y
  s2 <- sum(r^2) / (num_observations - num_coefficients)
  
  # Computação paralela do Cook's Distance
  d <- foreach(i = seq_len(num_observations), .combine = "c", .packages = c("dplyr","gamlss")) %dopar% {
    source("SUBURRXII_REG.R")
    # Cria o leave-one-out dataframe
    loo_data <- filter(data, !row_number() %in% i)
    
    # Ajusta o modelo loo
    loo_m <- fn(formula, data=loo_data, family=family, trace=FALSE)
    loo_p <- exp(predict(loo_m, newdata=data, data=loo_data, what=c("mu")))
    
    # Calcula Cook's Distance
    sum((loo_p - p)^2) / (num_coefficients * s2)
  }
  
  # Finaliza a clusterização paralela
  stopCluster(cl)
  
  return(d)
}
#Observações Influentes
which(d_cook>0.006)

#Outliers
b<-boxplot(y)
cond<-(y<b$stats[1,] | y>b$stats[5,])
which(cond)
#------------------------------------------------
#aplicando o cook
f<- y~1 | Ocup_EF + RM+ RD  # SIGMA
f<- y~x1+x6+x25+x28+x41+x51+x52 |1 # MU

d_cook<-cooksd_parallel(fn=gamlss, formula=f, data=dados_filter, family=RSUBXII)
names(dados_filter)
data<-cbind(y=y,dados_filter)
class(data)


data<-cbind(data$x1,data$x6,data$x25,data$x41,data$x51,data$x52)

plot(d_cook)


load("d_cook.RData")

plot(d_cook)

which(d_cook>0.0003)

