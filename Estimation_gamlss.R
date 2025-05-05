# Loading gamlss distribution functions
source("Gamlss_dists//Uchen.R")
source("Gamlss_dists//UGamma.R")
source("Gamlss_dists//Ugompertz.R")
source("Gamlss_dists//UKumaraswamy.R")
source("Gamlss_dists//ULindley.R")
source("Gamlss_dists//Utriangular.R")
source("Gamlss_dists//UWeibull.R")

fitted_dist <- function(y, family, X = NULL) {
  # Define list of families and their arguments
  families <- list(
    UG = list(fun = UG, args = list(mu.link = "logit", sigma.link = "log")),
    UL = list(fun = UL, args = list(mu.link = "logit")),
    UW = list(fun = UW, args = list(mu.link = "logit", sigma.link = "log")),
    KW = list(fun = KW, args = list(mu.link = "logit", sigma.link = "log")),
    UC = list(fun = UC, args = list(mu.link = "log", sigma.link = "log")),
    TRI = list(fun = TRI, args = list(mu.link = "logit")),
    UGo = list(fun = UGo, args = list(mu.link = "logit", sigma.link = "log")),
    BEo = list(fun = BEo, args = list(mu.link = "log", sigma.link = "logit")),
    BE = list(fun = BE, args = list(mu.link = "logit", sigma.link = "logit")),
    SIMPLEX = list(fun = SIMPLEX, args = list(mu.link = "logit", sigma.link = "log"))
  )

  if (!family %in% names(families)) stop("Family not recognized.")

  fam <- do.call(families[[family]]$fun, families[[family]]$args)

  if (is.null(X)){
  result <- suppressWarnings(try(
    gamlss::gamlss(y ~ 1, sigma.formula = ~1, family = fam, trace = FALSE),
    silent = TRUE
  ))
  }else{
    result <- suppressWarnings(try(
      gamlss::gamlss(y ~ X, sigma.formula = ~X, family = fam, trace = FALSE),
      silent = TRUE
    ))
  }

  if (inherits(result, "try-error")) {
    warning("Model fitting failed.")
    return(NULL)
  } else {
    return(result)
  }
}
