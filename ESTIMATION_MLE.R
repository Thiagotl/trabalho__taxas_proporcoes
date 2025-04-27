fitted_dist <- function(y, family) {
  if (family == "UG") { # Unit Gamma
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = UG(mu.link = "identity", sigma.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "UL") { # Unit Lindley
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = UL(mu.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "UW") { # Unit Weibull
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = UW(mu.link = "identity", sigma.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "KW") { # Kumaraswamy
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = KW(mu.link = "identity", sigma.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "UW") { # Unit Weibull
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = UW(mu.link = "identity", sigma.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "UC") { # Unit Chen
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = UC(mu.link = "identity", sigma.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "TRI") { # Unit Triangular
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = TRI(mu.link = "identity", ), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "Ugo") { # Unit Gompertz
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = Ugo(mu.link = "identity", sigma.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "BEo") { # Original Beta
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = BEo(mu.link = "identity", sigma.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "BE") { # Beta Mean parametrization
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = BE(mu.link = "identity", sigma.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  if (family == "SIMPLEX") { # Beta Mean parametrization
    result <- suppressWarnings(try(gamlss::gamlss(y ~ 1,
      sigma.formula = ~1,
      family = SIMPLEX(mu.link = "identity", sigma.link = "identity"), trace = F
    ), T))
    parameters <- summary(result)
  }
  return(parameters)
}
