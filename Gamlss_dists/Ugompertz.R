loglikUGo <- expression(
  log(mu) + log(sigma) - (sigma + 1) * log(y) - mu * (y^(-sigma) - 1)
)

dldm_expr <- D(loglikUGo, "mu")
dldd_expr <- D(loglikUGo, "sigma")


#### DEFINIÇÃO DA FAMÍLIA UGo PARA GAMLSS ----

UGo <- function(mu.link = "logit", sigma.link = "log") {
  mstats <- checklink("mu.link", "UGo", substitute(mu.link),
                      c("logit", "probit", "cloglog", "cauchit", "log", "own"))
  dstats <- checklink("sigma.link", "UGo", substitute(sigma.link),
                      c("inverse", "log", "identity", "own"))
  
  structure(list(
    family = c("UGo", "Unit-Gompertz"),
    parameters = list(mu = TRUE, sigma = TRUE),
    nopar = 2,
    type = "Continuous",
    control = list(trace = FALSE),
    
    mu.link = as.character(substitute(mu.link)),
    sigma.link = as.character(substitute(sigma.link)),
    
    mu.linkfun = mstats$linkfun,
    sigma.linkfun = dstats$linkfun,
    
    mu.linkinv = mstats$linkinv,
    sigma.linkinv = dstats$linkinv,
    
    mu.dr = mstats$mu.eta,
    sigma.dr = dstats$mu.eta,
    
    dldm = function(y, mu, sigma) eval(dldm_expr),
    
    d2ldm2 = function(y, mu, sigma) {
      dldm <- eval(dldm_expr)
      d2 <- -dldm^2
      ifelse(d2 < -1e-15, d2, -1e-15)
    },
    
    dldd = function(y, mu, sigma) eval(dldd_expr),
    
    d2ldd2 = function(y, mu, sigma) {
      dldd <- eval(dldd_expr)
      d2 <- -dldd^2
      ifelse(d2 < -1e-15, d2, -1e-15)
    },
    
    d2ldmdd = function(y, mu, sigma) {
      dldm <- eval(dldm_expr)
      dldd <- eval(dldd_expr)
      d2 <- -dldm * dldd
      ifelse(is.na(d2), 0, d2)
    },
    
    G.dev.incr = function(y, mu, sigma, ...) {
      -2 * dUGo(y = y, mu = mu, sigma = sigma, log = TRUE)
    },
    
    rqres = expression(
      rqres(pfun = "pUGo", type = "Continuous", y = y, mu = mu, sigma = sigma)
    ),
    
    mu.initial = expression(mu <- rep(0.5, length(y))),
    sigma.initial = expression(sigma <- rep(1, length(y))),
    
    mu.valid = function(mu) all(mu > 0 & mu < 1),
    sigma.valid = function(sigma) all(sigma > 0),
    y.valid = function(y) all(y > 0 & y < 1)
  ), class = c("gamlss.family", "family"))
}

# FUNÇÃO DENSIDADE
dUGo <- function(y, mu, sigma, log = FALSE) {
  if (any(y <= 0 | y >= 1)) {
    return(if (log) rep(-Inf, length(y)) else rep(0, length(y)))
  }
  
  log_fx <- log(mu) + log(sigma) - (sigma + 1) * log(y) - mu * (y^(-sigma) - 1)
  
  if (log) {
    return(log_fx)
  } else {
    return(exp(log_fx))
  }
}

pUGo<-function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE)
{
  cdf<-exp(-mu*(q^(-sigma)-1))

  return(cdf)
}

# QUANTIL
qUGo <- function(u, mu, sigma) {
  ((-log(u) / mu) + 1)^(-1 / sigma)
}


# GERAÇÃO DE AMOSTRAS
rUGo <- function(n, mu, sigma) {
  u <- runif(n, min = 1e-6, max = 1 - 1e-6)
  ((-log(u) / mu) + 1)^(-1 / sigma)
}