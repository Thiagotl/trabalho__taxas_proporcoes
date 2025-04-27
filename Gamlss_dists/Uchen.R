# Log-likelihood function of the unit chen ------------------------------------#
UC <- expression(
  -log(y) + log(mu * sigma) + (sigma - 1) * log(-log(y)) + (-log(y))**sigma +
    mu * (1 - exp((-log(y))**sigma))
)

m1UC <- D(UC, "mu")
s1UC <- D(UC, "sigma")
ms2UC <- D(m1UC, "sigma")

UC <- function(mu.link = "log", sigma.link = "log") {
  mstats <- checklink(
    "mu.link", "UC", substitute(mu.link),
    c("inverse", "log", "identity", "own")
  )
  dstats <- checklink(
    "sigma.link", "UC", substitute(sigma.link),
    c("inverse", "log", "identity", "own")
  )
  structure(
    list(
      family = c("UC", "Unit-Chen"),
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
        dldm <- eval(m1UC)
        dldm
      },
      d2ldm2 = function(y, mu, sigma) {
        dldm <- eval(m1UC)
        d2ldm2 <- -dldm * dldm
        d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2, -1e-15)
        d2ldm2
      },
      dldd = function(y, mu, sigma) {
        dldd <- eval(s1UC)
        dldd
      },
      d2ldd2 = function(y, mu, sigma) {
        dldd <- eval(s1UC)
        d2ldd2 <- -dldd * dldd
        d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2, -1e-15)
        d2ldd2
      },
      d2ldmdd = function(y, mu, sigma) {
        dldm <- eval(m1UC)
        dldd <- eval(s1UC)
        d2ldmdd <- -(dldm * dldd)
        d2ldmdd <- ifelse(is.na(d2ldmdd) == TRUE, 0, d2ldmdd)
        d2ldmdd
      },
      G.dev.incr = function(y, mu, sigma, w, ...) -2 * log(duchen(y = y, mu = mu, sigma = sigma)),
      rqres = expression(
        rqres(pfun = "puchen", type = "Continuous", y = y, mu = mu, sigma = sigma)
      ),
      mu.initial = expression(mu <- rep(mean(y), length(y))),
      sigma.initial = expression(sigma <- rep(mean(y), length(y))),
      mu.valid = function(mu) all(mu > 0),
      sigma.valid = function(sigma) all(sigma > 0),
      y.valid = function(y) all(y > 0 & y < 1)
    ),
    class = c("gamlss.family", "family")
  )
}

# Cumulative distribution function --------------------------------------------#

puchen <- function(q, mu, sigma) {
  return(exp(mu * (1 - exp((-log(q))**sigma))))
}

# Probability density function ------------------------------------------------#

duchen <- function(y, mu, sigma, log = FALSE) {
  f <- mu * sigma * exp(mu * (1 - exp((-log(y))**sigma)) + (-log(y))**sigma) *
    (-log(y))**(sigma - 1) / y

  if (log == TRUE) {
    return(log(f))
  }
  return(
    f
  )
}

# Quantile Function -----------------------------------------------------------#

quchen <- function(u, mu, sigma) {
  return(
    exp(-(log(1 - log(u) / mu)**(1 / sigma)))
  )
}

# Random number generator function --------------------------------------------#

ruchen <- function(n, mu, sigma) {
  u <- runif(n)
  return(
    exp(-(log(1 - log(u) / mu)**(1 / sigma)))
  )
}
