# Loading gamlss distribution functions
source("Gamlss_dists//Uchen.R")
source("Gamlss_dists//UGamma.R")
source("Gamlss_dists//Ugompertz.R")
source("Gamlss_dists//UKumaraswamy.R")
source("Gamlss_dists//ULindley.R")
source("Gamlss_dists//UTriangular.R")
source("Gamlss_dists//UWeibull.R")

fitted_dist <- function(y, family) {
  # Define list of families and their arguments
  families <- list(
    UG = list(fun = UG, args = list(mu.link = "identity", sigma.link = "identity")),
    UL = list(fun = UL, args = list(mu.link = "identity")),
    UW = list(fun = UW, args = list(mu.link = "identity", sigma.link = "identity")),
    KW = list(fun = KW, args = list(mu.link = "identity", sigma.link = "identity")),
    UC = list(fun = UC, args = list(mu.link = "identity", sigma.link = "identity")),
    TRI = list(fun = TRI, args = list(mu.link = "identity")),
    UGo = list(fun = UGo, args = list(mu.link = "identity", sigma.link = "identity")),
    BEo = list(fun = BEo, args = list(mu.link = "identity", sigma.link = "identity")),
    BE = list(fun = BE, args = list(mu.link = "identity", sigma.link = "identity")),
    SIMPLEX = list(fun = SIMPLEX, args = list(mu.link = "identity", sigma.link = "identity"))
  )
  
  if (!family %in% names(families)) stop("Family not recognized.")
  
  fam <- do.call(families[[family]]$fun, families[[family]]$args)
  
  result <- suppressWarnings(try(
    gamlss::gamlss(y ~ 1, sigma.formula = ~1, family = fam, trace = FALSE), 
    silent = TRUE
  ))
  
  if (inherits(result, "try-error")) {
    warning("Model fitting failed.")
    return(NULL)
  } else {
    return(result)
  }
}
