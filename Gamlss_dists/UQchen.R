# Log-likelihood function of the re-parameterized unit chen -------------------#
UQC <- expression(
  log(log(tau) * sigma / (1 - exp((-log(mu))^sigma))) - log(y) + (sigma - 1) * log(-log(y)) +
    (-log(y))^sigma + log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(y))^sigma))
)

m1UQC <- D(UQC, "mu")
s1UQC <- D(UQC, "sigma")
ms2UQC <- D(m1UQC, "sigma")

UQC <- function(mu.link = "logit", sigma.link = "log") {
  mstats <- checklink(
    "mu.link", "UQC", substitute(mu.link),
    c("logit", "probit", "cloglog", "cauchit", "log", "own","identity")
  )
  dstats <- checklink(
    "sigma.link", "UQC", substitute(sigma.link),
    c("inverse", "log", "identity", "own")
  )
  structure(
    list(
      family = c("UQC", "Quantile Unit-Chen"),
      parameters = list(mu = TRUE, sigma = TRUE),
      nopar = 2,
      type = "Continuous",
      mu.link = as.character(substitute(mu.link)),
      sigma.link = as.character(substitute(sigma.link)),
      mu.linkfun = mstats$linkfun,
      sigma.linkfun = dstats$linkfun,
      mu.linkinv = mstats$linkinv,
      sigma.linkinv = dstats$linkinv,
      mu.dr = mstats$mu.eta,
      sigma.dr = dstats$mu.eta,
      dldm = function(y, mu, sigma) {
        tau <- 0.5
        dldm <- eval(m1UQC)
        dldm
      },
      d2ldm2 = function(y, mu, sigma) {
        tau <- 0.5
        dldm <- eval(m1UQC)
        d2ldm2 <- -dldm * dldm
        d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2, -1e-15)
        d2ldm2
      },
      dldd = function(y, mu, sigma) {
        tau <- 0.5
        dldd <- eval(s1UQC)
        dldd
      },
      d2ldd2 = function(y, mu, sigma) {
        tau <- 0.5
        dldd <- eval(s1UQC)
        d2ldd2 <- -dldd * dldd
        d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2, -1e-15)
        d2ldd2
      },
      d2ldmdd = function(y, mu, sigma) {
        tau <- 0.5
        dldm <- eval(m1UQC)
        dldd <- eval(s1UQC)
        d2ldmdd <- -(dldm * dldd)
        d2ldmdd <- ifelse(is.na(d2ldmdd) == TRUE, 0, d2ldmdd)
        d2ldmdd
      },
      G.dev.incr = function(y, mu, sigma, w, ...) -2 * log(dUQC(y = y, mu = mu, sigma = sigma)),
      rqres = expression(
        rqres(pfun = "pUQC", type = "Continuous", y = y, mu = mu, sigma = sigma)
      ),
      mu.initial = expression(mu <- rep(mean(y), length(y))),
      sigma.initial = expression(sigma <- rep(mean(y), length(y))),
      mu.valid = function(mu) all(mu > 0 & mu < 1),
      sigma.valid = function(sigma) all(sigma > 0),
      y.valid = function(y) all(y > 0 & y < 1)
    ),
    class = c("gamlss.family", "family")
  )
}

# Cumulative distribution function --------------------------------------------#

pUQC <- function(q, mu, sigma, tau = 0.5) {
  f <- exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(q))**sigma)))
  
  return(f)
}

# Probability density function ------------------------------------------------#

dUQC <- function(y, mu, sigma, tau = 0.5, log = FALSE) {
  f <- log(tau) * sigma / ((1 - exp((-log(mu))^sigma)) * y) * (-log(y))^(sigma - 1) *
    exp((-log(y))^sigma) * exp(log(tau) / (1 - exp((-log(mu))^sigma)) * (1 - exp((-log(y))^sigma)))
  if (log == TRUE) {
    return(log(f))
  }
  return(f)
}

# Quantile Function -----------------------------------------------------------#

qUQC <- function(u, mu, sigma, tau = 0.5) {
  q <- exp(-(log(1 - log(u) / log(tau) * (1 - exp((-log(mu))^sigma))))^(1 / sigma))
  
  return(q)
}

# Random number generator function --------------------------------------------#

rUQC <- function(n, mu, sigma, tau = 0.5) {
  u <- runif(n)
  y <- exp(-(log(1 - log(u) / log(tau) * (1 - exp((-log(mu))^sigma))))^(1 / sigma))
  
  return(y)
}
