source("Gamlss_dists//Uchen.R", "Gamlss_dists//UGamma")

fitted_dist <- function(y, family) {
  # Define list of families and their arguments
  families <- list(
    UG = list(fun = UG, args = list(mu.link = "identity", sigma.link = "identity")),
    UL = list(fun = UL, args = list(mu.link = "identity")),
    UW = list(fun = UW, args = list(mu.link = "identity", sigma.link = "identity")),
    KW = list(fun = KW, args = list(mu.link = "identity", sigma.link = "identity")),
    UC = list(fun = UC, args = list(mu.link = "identity", sigma.link = "identity")),
    TRI = list(fun = TRI, args = list(mu.link = "identity")),
    Ugo = list(fun = Ugo, args = list(mu.link = "identity", sigma.link = "identity")),
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
    return(summary(result))
  }
}