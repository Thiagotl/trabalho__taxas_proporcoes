# Unit Gamma -------------------------------------------------------------------

# mu in (0,1)
# sigma > 0
# density function
dUG <- function(y, mu, sigma, log = FALSE) {
  fy1 <- 1 / y * dgamma(-log(y), sigma, mu^(1 / sigma) / (1 - mu^(1 / sigma)))
  if (log == FALSE) fy <- fy1 else fy <- log(fy1)
  fy
}

# cumulative distribution function
pUG <- function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
  cdf1 <- 1 - pgamma(-log(q), sigma, mu^(1 / sigma) / (1 - mu^(1 / sigma)))
  if (lower.tail == TRUE) cdf <- cdf1 else cdf <- 1 - cdf1
  if (log.p == FALSE) cdf <- cdf else cdf <- log(cdf)
  cdf
}

# Unit Lindley -----------------------------------------------------------------

# mu in (0,1)
# density function
dUL <- function(y, mu, log = FALSE) {
  fy1 <- ((1 - mu)^2 / (mu * (1 - y)^3)) * exp((-y * (1 - mu)) / (mu * (1 - y)))
  if (log == FALSE) fy <- fy1 else fy <- log(fy1)
  fy
}

# cumulative distribution function
pUL <- function(q, mu, lower.tail = TRUE, log.p = FALSE) {
  cdf1 <- 1 - (1 - (1 - mu) * q / (q - 1)) * exp(-(1 - mu) * q / (mu * (1 - q)))
  if (lower.tail == TRUE) cdf <- cdf1 else cdf <- 1 - cdf1
  if (log.p == FALSE) cdf <- cdf else cdf <- log(cdf)
  cdf
}

# Unit Weibull -----------------------------------------------------------------

# mu in (0,1)
# sigma > 0
# density function
dUW <- function(y, mu, sigma, log = FALSE) {
  fy1 <- sigma * log(2) / (y) * (-log(mu))^(-1) * (log(y) / log(mu))^(sigma - 1) *
    2^(-(log(y) / log(mu))^sigma)
  if (log == FALSE) fy <- fy1 else fy <- log(fy1)
  fy
}
# cumulative distribution function
pUW <- function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
  cdf1 <- .5^((log(q) / log(mu))^sigma)
  if (lower.tail == TRUE) cdf <- cdf1 else cdf <- 1 - cdf1
  if (log.p == FALSE) cdf <- cdf else cdf <- log(cdf)
  cdf
}

# Kumaraswamy ------------------------------------------------------------------

# mu in (0,1)
# sigma > 0
# density function
dKW <- function(y, mu, sigma, log = FALSE) {
  a <- 0
  b <- 1
  fy1 <- (1 / (b - a)) * ((sigma * log(0.5)) / (log(1 - mu^sigma))) * y^(sigma - 1) * (1 - y^sigma)^(((log(0.5)) / (log(1 - mu^sigma))) - 1)
  if (log == FALSE) fy <- fy1 else fy <- log(fy1)
  fy
}

# cumulative distribution function
pKW <- function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
  a <- 0
  b <- 1
  cdf1 <- 1 - (1 - q^sigma)^((log(0.5)) / (log(1 - mu^sigma)))
  if (lower.tail == TRUE) cdf <- cdf1 else cdf <- 1 - cdf1
  if (log.p == FALSE) cdf <- cdf else cdf <- log(cdf)
  cdf
}

# Unit Chen --------------------------------------------------------------------

# mu > 0
# sigma > 0

# density function
dUC <- function(y, mu, sigma, log = FALSE) {
  f <- mu * sigma * exp(mu * (1 - exp((-log(y))**sigma)) + (-log(y))**sigma) *
    (-log(y))**(sigma - 1) / y

  if (log == TRUE) {
    return(log(f))
  }
  return(
    f
  )
}

# cumulative distribution function
pUC <- function(q, mu, sigma) {
  return(exp(mu * (1 - exp((-log(q))**sigma))))
}

# Unit triangular --------------------------------------------------------------

# mu in (0,1)

# density function
dUTR <- function(y, mu, log = FALSE) {
  fy1 <- ifelse(
    y < 0 | y > 1,
    0,
    ifelse(
      y < mu,
      2 * y / mu,
      2 * (1 - y) / (1 - mu)
    )
  )
  if (log == FALSE) fy <- fy1 else fy <- log(fy1)
  return(fy)
}

# cumulative distribution function
pUTR <- function(q, mu, lower.tail = TRUE, log.p = FALSE) {
  Fx <- ifelse(
    q < 0, 0,
    ifelse(
      q < mu,
      (q^2) / mu,
      ifelse(
        q > mu,
        1 - ((1 - q)^2) / (1 - mu),
        1
      )
    )
  )
  if (lower.tail == TRUE) cdf <- Fx else cdf <- 1 - Fx
  if (log.p == FALSE) cdf <- cdf else cdf <- log(cdf)
  return(cdf)
}

# Unit Gompertz ----------------------------------------------------------------

# mu in (0,1)
# sigma > 0
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

pUGo <- function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
  cdf <- exp(-mu * (q^(-sigma) - 1))

  return(cdf)
}