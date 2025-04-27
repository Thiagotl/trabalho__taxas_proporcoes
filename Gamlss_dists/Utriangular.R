TRI<-function (mu.link = "logit")
{
  mstats <- checklink("mu.link", "TRI", substitute(mu.link),
                      c("logit","probit","cloglog","cauchit","log","own","identity"))
  structure(list(family = c("TRI"),
                 parameters = list(mu = TRUE),
                 nopar = 1,
                 type = "Continuous",
                 mu.link = as.character(substitute(mu.link)),
                 mu.linkfun = mstats$linkfun,
                 mu.linkinv = mstats$linkinv,
                 mu.dr = mstats$mu.eta,
                 dldm = function(y, mu) {
                   ifelse(y < mu, -1 / mu, 1 / (1 - mu))
                 },
                 d2ldm2 = function(y,mu) {
                   ifelse(y < mu, -1/mu^2, -1/(1-mu)^2)
                 },
                 G.dev.incr = function(y, mu, ...) -2 * dTRI(y=y,mu=mu,log=TRUE),
                 rqres = expression(
                   rqres(pfun="pTRI",type ="Continuous",y=y,mu=mu)
                 ),
                 mu.initial = expression(mu <- rep(mean(y),length(y))),
                 mu.valid = function(mu) all(mu > 0 & mu < 1),
                 y.valid = function(y) all(y > 0 & y < 1)),
  class = c("gamlss.family", "family"))
}

dTRI <- function(y, mu,log=FALSE) {
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(y <= 0) | any(y >= 1)) stop(paste("y must be between 0 and 1", "\n", ""))
  
  fy1 <- ifelse(
    y < 0 | y > 1,
    0,
    ifelse(
      y < mu,
      2 * y / mu,
      2 * (1 - y) / (1 - mu)
    )
  )
  if(log==FALSE) fy<-fy1 else fy<-log(fy1)
  return(fy)
}

# cumulative distribution function
pTRI <- function(q, mu,lower.tail = TRUE, log.p = FALSE) {
  if (any(mu <= 0)) stop(paste("mu must be positive", "\n", ""))
  if (any(q <= 0) | any(q >= 1)) stop(paste("y must be between 0 and 1", "\n", ""))
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
    if(lower.tail==TRUE) cdf<-Fx else cdf<- 1-Fx
    if(log.p==FALSE) cdf<- cdf else cdf<- log(cdf)
    return(cdf)
}

#inversion method for randon generation
rTRI <- function(n, mu) {
  u <- runif(n)
  x <- ifelse(
    u < mu,
    sqrt(u * mu),
    1 - sqrt((1 - u) * (1 - mu))
  )
  return(x)
}