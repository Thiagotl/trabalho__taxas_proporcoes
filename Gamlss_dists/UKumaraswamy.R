kww <- expression(log(
  (1 / (b - a)) * ((sigma * log(tau)) / (log(1 - mu^sigma))) *
    y^(sigma - 1) * (1 - y^sigma)^(((log(tau)) / (log(1 - mu^sigma))) - 1)
))
m1 <- D(kww, "mu")
s1 <- D(kww, "sigma")
ms2 <- D(m1, "sigma")


KW <- function(mu.link = "logit", sigma.link = "log") {
  mstats <- checklink(
    "mu.link", "KW", substitute(mu.link),
    c("logit", "probit", "cloglog", "cauchit", "log", "own")
  )
  dstats <- checklink(
    "sigma.link", "KW", substitute(sigma.link),
    c("inverse", "log", "identity", "own")
  )
  structure(
    list(
      family = c("KW", "Kumaraswamy"),
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
      dldm = function(y, mu, sigma) { # ok
        a <- 0
        b <- 1
        tau <- 0.5
        dldm <- eval(m1)
        dldm
      },
      d2ldm2 = function(y, mu, sigma) {
        a <- 0
        b <- 1
        tau <- 0.5
        dldm <- eval(m1)
        d2ldm2 <- -dldm * dldm
        d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2, -1e-15)
        d2ldm2
      },
      dldd = function(y, mu, sigma) { # ok
        a <- 0
        b <- 1
        tau <- 0.5
        dldd <- eval(s1)
        dldd
      },
      d2ldd2 = function(y, mu, sigma) {
        a <- 0
        b <- 1
        tau <- 0.5
        dldd <- eval(s1)
        d2ldd2 <- -dldd * dldd
        d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2, -1e-15)
        d2ldd2
      },
      d2ldmdd = function(y, mu, sigma) {
        a <- 0
        b <- 1
        tau <- 0.5
        dldm <- eval(m1)
        dldd <- eval(s1)
        d2ldmdd <- -(dldm * dldd)
        d2ldmdd <- ifelse(is.na(d2ldmdd) == TRUE, 0, d2ldmdd)
        d2ldmdd
      },
      G.dev.incr = function(y, mu, sigma, w, ...) -2 * log(dKW(y = y, mu = mu, sigma = sigma)),
      rqres = expression(
        rqres(pfun = "pKW", type = "Continuous", y = y, mu = mu, sigma = sigma)
      ),
      mu.initial = expression(mu <- rep(mean(y), length(y))),
      sigma.initial = expression(sigma <- rep(0.5, length(y))),
      mu.valid = function(mu) all(mu > 0 & mu < 1),
      sigma.valid = function(sigma) all(sigma > 0),
      y.valid = function(y) all(y > 0 & y < 1)
    ),
    class = c("gamlss.family", "family")
  )
}


# density function
dKW <- function(y, mu, sigma, log = FALSE) {
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma <= 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(y <= 0) | any(y >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  a <- 0
  b <- 1
  tau <- 0.5
  fy1 <- (1 / (b - a)) * ((sigma * log(tau)) / (log(1 - mu^sigma))) * y^(sigma - 1) * (1 - y^sigma)^(((log(tau)) / (log(1 - mu^sigma))) - 1)
  if (log == FALSE) fy <- fy1 else fy <- log(fy1)
  fy
}
#------------------------------------------------------------------------------------------ #ok
# cumulative distribution function
pKW <- function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma < 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(q <= 0) | any(q >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  a <- 0
  b <- 1
  tau <- 0.5
  cdf1 <- 1 - (1 - q^sigma)^((log(tau)) / (log(1 - mu^sigma)))
  if (lower.tail == TRUE) cdf <- cdf1 else cdf <- 1 - cdf1
  if (log.p == FALSE) cdf <- cdf else cdf <- log(cdf)
  cdf
}
#------------------------------------------------------------------------------------------ #ok
# quantile function
qKW <- function(u, mu, sigma, a = 0, b = 1) {
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma < 0)) stop(paste("sigma must be positive", "\n", ""))
  if (any(u <= 0) | any(u >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  tau <- 0.5
  q <- a + (b - a) * (1 - (1 - u)^((log(1 - mu^sigma)) / (log(tau))))^(1 / sigma)
  q
}

# inversion method for randon generation
rKW <- function(n, mu, sigma, a = 0, b = 1) {
  tau <- 0.5
  u <- runif(n)
  y <- a + (b - a) * (1 - (1 - u)^((log(1 - mu^sigma)) / (log(tau))))^(1 / sigma)
  y
}
