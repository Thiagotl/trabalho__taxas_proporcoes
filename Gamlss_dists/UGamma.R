UG<-function (mu.link = "logit", sigma.link = "log") 
{
  mstats <- checklink("mu.link", "UG", substitute(mu.link), 
                      c("logit", "probit", "cloglog", "cauchit", "log", "own"))
  dstats <- checklink("sigma.link", "UW", substitute(sigma.link), 
                      c("inverse", "log", "identity", "own"))
  structure(list(family = c("UG", "Unit-Gamma"), 
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
                   dldm <- mu^(1/sigma)/((1-mu^(1/sigma))*mu^(1/sigma+1))*
                     (1+mu^(1/sigma)/((1-mu^(1/sigma))*sigma)*log(y))
                   dldm
                 }, 
                 d2ldm2 = function(y,mu, sigma) {
                   dldm <- mu^(1/sigma)/((1-mu^(1/sigma))*mu^(1/sigma+1))*
                     (1+mu^(1/sigma)/((1-mu^(1/sigma))*sigma)*log(y))
                   d2ldm2 <- -dldm * dldm
                   d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15) 
                   d2ldm2
                 }, 
                 dldd = function(y, mu, sigma) {
                   dldd <- log(-log(y))-1/sigma*mu^(1/sigma)/(1-mu^(1/sigma))*log(mu)*
                     (1+mu^(1/sigma)/((1-mu^(1/sigma))*sigma*mu^(1/sigma))*log(y))-
                     log((1-mu^(1/sigma)))-digamma(sigma)
                   dldd
                 },
                 d2ldd2 = function(y,mu, sigma) {
                   dldd <- log(-log(y))-1/sigma*mu^(1/sigma)/(1-mu^(1/sigma))*log(mu)*
                     (1+mu^(1/sigma)/((1-mu^(1/sigma))*sigma*mu^(1/sigma))*log(y))-
                     log((1-mu^(1/sigma)))-digamma(sigma)
                   d2ldd2 = -dldd * dldd
                   d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)  
                   d2ldd2
                 },
                 d2ldmdd = function(y,mu, sigma) {
                   dldm <- mu^(1/sigma)/((1-mu^(1/sigma))*mu^(1/sigma+1))*
                     (1+mu^(1/sigma)/((1-mu^(1/sigma))*sigma)*log(y))
                   dldd <- log(-log(y))-1/sigma*mu^(1/sigma)/(1-mu^(1/sigma))*log(mu)*
                     (1+mu^(1/sigma)/((1-mu^(1/sigma))*sigma*mu^(1/sigma))*log(y))-
                     log((1-mu^(1/sigma)))-digamma(sigma)
                   d2ldmdd = -(dldm * dldd)
                   d2ldmdd<-ifelse(is.na(d2ldmdd)==TRUE,0,d2ldmdd)
                   d2ldmdd  
                 }, 
                 G.dev.incr = function(y, mu, sigma, w, ...) -2 * log(dUG(y=y, mu=mu, sigma=sigma)), 
                 rqres = expression(
                   rqres(pfun = "pUG",  type = "Continuous", y = y, mu = mu, sigma = sigma)
                 ),
                 
                 mu.initial = expression(mu <- rep(mean(y),length(y))),   
                 sigma.initial = expression(sigma<- rep(2, length(y))),
                 mu.valid = function(mu) all(mu > 0 & mu < 1), 
                 sigma.valid = function(sigma)  all(sigma > 0),
                 y.valid = function(y) all(y > 0 &  y < 1)
  ), 
  class = c("gamlss.family", "family"))
}

# density function
dUG<-function(y, mu = 0.7, sigma = 2.1, log = FALSE)
{
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma <= 0)) stop(paste("sigma must be positive", "\n", "")) 
  if (any(y <= 0) | any(y >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  
  fy1 <- 1/y*dgamma(-log(y),sigma,mu^(1/sigma)/(1-mu^(1/sigma)))
  if(log==FALSE) fy<-fy1 else fy<-log(fy1)
  fy
}

# cumulative distribution function
pUG<-function(q, mu = 0.7, sigma = 2.1, lower.tail = TRUE, log.p = FALSE){
  if (any(mu <= 0) | any(mu >= 1)) stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", "")) 
  if (any(q <= 0) | any(q >= 1)) stop(paste("x must be between 0 and 1", "\n", ""))
  cdf1<-  1-pgamma(-log(q),sigma,mu^(1/sigma)/(1-mu^(1/sigma)))
  if(lower.tail==TRUE) cdf<-cdf1 else cdf<- 1-cdf1
  if(log.p==FALSE) cdf<- cdf else cdf<- log(cdf)
  cdf
}

# quantile function
qUG<-function(u,mu,sigma)
{
  q<- exp(-qgamma(1-u,sigma,mu^(1/sigma)/(1-mu^(1/sigma))))
  q
}	

# inversion method for randon generation
rUG<-function(n,mu,sigma)
{
  u<- runif(n)
  y<- qUG(u,mu =mu, sigma =sigma)
  y
}