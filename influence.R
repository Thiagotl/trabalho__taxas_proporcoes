influence.gamlss <- function(model, data, method="cook", plot=TRUE, print=FALSE,
                             hist=FALSE, bootstrap=FALSE, B=1000, seed=357815)
{
  #### function: LD.aux #### 
  # Description:
  # Provides the likelihood distance for a given gamlss model and i-th
  # bootstrap sample
  LD.aux <- function(model, i)
  {
    N <- model$N
    call <- model$call
    if (is.na(match("data", names(model$call)))) 
      stop("no data are declared")
    call.Formula <- deparse(call$formula)
    call.Family <- deparse(call$family)
    call.Data <- deparse(call$data)
    call.Model <- paste("gamlss(formula=", call.Formula, ",family=", 
                        call.Family, ",data=",call.Data, "[-jj,],",
                        "trace=FALSE)", sep="")
    
    LD <- vector(mode="numeric", length=N)
    for (j in 1:N)
    {
      assign("ii", i, envir = sys.frame())
      assign("jj", j, envir = sys.frame())
      model.i <- eval(parse(text=call.Model))
      logL <-as.numeric(-logLik(model))
      logL0 <-as.numeric(-logLik(model.i))
      LD[j] <- 2*(logL-logL0)
    }
    
    return(LD)
  }
  
  #### function LD.bootstrap ####
  # Description: 
  # Uses non-parametric bootstrap to obtain references values
  # for the likelihood distance for a gamlss model
  LD.bootstrap <- function(model, data, B=1000, seed=357814)
  {
    N <- model$N
    boot.Samples <- vector(mode="list", length=B)
    
    set.seed(seed=seed)
    for (i in seq_len(B))
    {
      if(N<99)
      {
        boot.Samples[[i]] <- data[sample(seq_len(N), N, replace=TRUE), ]
      }
      else
      {
        boot.Samples[[i]] <- data[sample(seq_len(100), 100, replace=TRUE), ]
      } 
    }
    
    assign("boot.Samples", boot.Samples, envir = globalenv())
    call <- model$call
    call.Formula <- deparse(call$formula)
    call.Family <- deparse(call$family)
    call.Data <- deparse(call$data)
    call.Model <- paste("gamlss(formula = ", call.Formula, ",family = ", 
                        call.Family, ",data = ","boot.Samples[[ii]],", 
                        "trace = FALSE)", sep = "")
    
    boot.Models <- vector(mode="list", length=B)
    for (i in 1:B) 
    {
      assign("ii", i, envir=sys.frame())
      boot.Models[[i]] <- eval(parse(text=call.Model))
    }
    
    boot.LD = vector(mode="list", length=B)
    for(i in 1:B)
    {
      boot.LD[[i]] <- LD.aux(model=boot.Models[[i]], i)
      cat("Progress:", (i/B)*100, "%", "\r")
      if(i == B) cat("\nDone!\n")
    }
    
    return(unlist(boot.LD))
  }
  
  #### function: I ####
  # Description: 
  # Provides the 'I' (observed information matrix) for a given gamlss model
  # not available for nu and tau (i have to include this)
  I <- function(model)
  {
    if(is.null(model$sigma.coefficients))
    {
      logL <- gen.likelihood(model)
      I <- optimHess(c(coef(model)), logL)  
    }
    else
    {
      logL <- gen.likelihood(model)
      I <- optimHess(c(coef(model), coef(model, "sigma")), logL)
    }
    
    return(-I)
  }
  
  #### function: CD.i ####
  # Description: 
  # Provides the ith cook distance for a given gamlss model
  CD.i <- function(model, i)
  {
    if (!is.gamlss(model)) stop("It is not a gamlss object")
    N <- model$N
    call <- model$call
    if (is.na(match("data", names(model$call)))) stop("no data are declared")
    call.Formula <- deparse(call$formula)
    call.Family <- deparse(call$family)
    call.Data <- deparse(call$data)
    call.Model <- paste("gamlss(formula = ", call.Formula, ",family = ", 
                        call.Family, ",data = ",call.Data, "[-ii,],", 
                        "trace = FALSE)", sep = "")
    
    assign("ii", i, envir = sys.frame())
    model.i <- eval(parse(text=call.Model))
    if(is.null(model$sigma.coefficients))
    {
      theta <- t(c(coef(model)))
      theta.i <- t(c(coef(model.i)))
      CD <- (theta.i-theta)%*%-I(model)%*%t(theta.i-theta)  
    }
    else
    {
      theta <- t(c(coef(model), coef(model, "sigma")))
      theta.i <- t(c(coef(model.i), coef(model.i, "sigma")))
      CD <- (theta.i-theta)%*%-I(model)%*%t(theta.i-theta) 
    }
    
    return(CD)
  }
  
  #### function: cook.aux ####
  # Description:
  # Provides the cook's distance for a given model and bootstrap sample
  cook.aux <- function(model, boot.Sample)
  {
    I.aux <- function(model, boot.Sample)
    {
      if(is.null(model$sigma.coefficients))
      {
        logL <- gen.likelihood(model)
        I <- optimHess(c(coef(model)), logL)  
      }
      else
      {
        logL <- gen.likelihood(model)
        I <- optimHess(c(coef(model), coef(model, "sigma")), logL)
      }
      return(-I)
    }
    
    N <- model$N
    call <- model$call
    call.Formula <- deparse(call$formula)
    call.Family <- deparse(call$family)
    call.Data <- deparse(call$data)
    call.Model <- paste("gamlss(formula=", call.Formula, ",family=", 
                        call.Family, ",data=", call.Data, "[-jj,],", 
                        "trace=FALSE)", sep="")
    
    CD <- vector(mode="numeric", length=N)
    for(j in 1:N)
    {
      assign("jj", j, envir=sys.frame())
      model.i <- eval(parse(text=call.Model))
      
      if(is.null(model$sigma.coefficients))
      {
        theta <- t(c(coef(model)))
        theta.i <- t(c(coef(model.i)))
        CD[j] <- (theta.i-theta)%*%-I.aux(model)%*%t(theta.i-theta)  
      }
      else
      {
        theta <- t(c(coef(model), coef(model, "sigma")))
        theta.i <- t(c(coef(model.i), coef(model.i, "sigma")))
        CD[j] <- (theta.i-theta)%*%-I.aux(model)%*%t(theta.i-theta) 
      }
    }
    
    return(CD)
  }
  
  #### function: cook.bootstrap ####
  # Description: 
  # Provides the cook's distance for B generated bootstrap samples
  cook.bootstrap <- function(model, data, B=1000, seed=357814)
  {
    boot.Samples <- vector(mode="list", length=B)
    N <- model$N
    
    set.seed(seed=seed)
    for (i in seq_len(B))
    {
      if(N<99)
      {
        boot.Samples[[i]] <- data[sample(seq_len(N), N, replace=TRUE), ]
      }
      else
      {
        boot.Samples[[i]] <- data[sample(seq_len(100), 100, replace=TRUE), ]
      } 
    }
    
    call <- model$call
    call.Formula <- deparse(call$formula)
    call.Family <- deparse(call$family)
    call.Data <- deparse(call$data)
    call.Model <- paste("gamlss(formula=", call.Formula, ",family=", 
                        call.Family, ",data=","boot.Sample,", 
                        "trace=FALSE)", sep="")
    
    boot.Models <- vector(mode="list", length=B)
    for (i in 1:B) 
    {
      boot.Sample <- boot.Samples[[i]]
      boot.Models[[i]] <- eval(parse(text=call.Model))
      rm(boot.Sample)
    }
    
    boot.CD <- vector(mode="list", length=B)
    for(i in 1:B)
    {
      boot.Sample <-boot.Samples[[i]]
      boot.CD[[i]] <- cook.aux(model=boot.Models[[i]], 
                               boot.Sample=boot.Sample)
      cat("Progress:", (i/B)*100, "%", "\r")
      if(i == B) cat("\nDone!\n")
    }
    
    return(unlist(boot.CD))
  }
  
  #### function: kim.aux ####
  # Description:
  # Provides the kim's measure for a given model and bootstrap sample
  kim.aux <- function(model)
  {
    if(!is.null(model$mu.s))
    {
      N <- model$N
      p <- length(model$mu.coefficients)
      variable <- colnames(model$mu.s)
      X <- as.data.frame(model$mu.x)
      X.s <- X[,variable]
      X <- X[ , -which(names(X) %in% variable)]
      X <- X[,-1]
      y <- model$y
      family <- model$family[1]
      
      Bbase <- function(x,  ndx=20, deg=3)
      {
        if(length(x)<99)
        {
          ndx = 10
          deg = 3
        }
        tpower <- function(x, t, p) {(x - t) ^ p * (x > t)}
        xl <- min(x, na.rm = TRUE)
        xr <- max(x, na.rm = TRUE)
        xmin <- xl - 0.01 * (xr - xl) 
        xmax <- xr + 0.01 * (xr - xl)
        dx <- (xmax - xmin) / ndx # DS increment 
        knots <- seq(xmin - deg * dx, xmax + deg * dx, by = dx)
        P <- outer(x, knots, tpower, deg) # calculate the power in the knots
        n <- dim(P)[2]
        D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg)
        B <- (-1) ^ (deg + 1) * P %*% t(D) 
        attr(B, "knots") <- knots[-c(1:(deg-1), (n-(deg-2)):n)]
        B   
      }
      
      pbknots <- getSmo(model)$knots
      B <- Bbase(X.s)
      D2 <- diff(diag(length(pbknots)), diff=2)
      G <- t(D2)%*%D2
      lambda <- getSmo(model)$lambda
      aux <- gamlss(y~pb(X.s), family = family, trace=FALSE)
      W <- diag(aux$mu.wt)
      S <- B%*%solve(t(B)%*%W%*%B+lambda*G)%*%t(B)%*%W 
      
      I <- diag(model$N)
      X.tilde <- (I-S)%*%as.matrix(X)
      y.tilde <- (I-S)%*%y
      e <- (I-hat.matrix(model)$hat)%*%y 
      h.ii <- diag(hat.matrix(model)$hat)
      hat.trace <- sum(h.ii)
      
      e.tilde <- (I-hat.matrix(model)$hat.tilde)%*%y.tilde
      h.tilde.ii <- diag(hat.matrix(model)$hat.tilde)
      
      e.star <- (I-hat.matrix(model)$hat.star)%*%y
      h.star.ii <- diag(hat.matrix(model)$hat.star)
      hat.star.trace <- sum(h.star.ii)
      
      sigma2 <- ((t(e)%*%e)/(N-hat.trace))
      
      betas.influence <- vector(mode="numeric", length=N)
      smoother.influence <- vector(mode="numeric", length=N)
      y.influence <- vector(mode="numeric", length=N)
      for (i in 1:N)
      {
        betas.influence[i] <- ((1/(p*sigma2))*(((e.tilde[i]^2 )
                                                *h.tilde.ii[i])/((1-h.tilde.ii[i])^2))) 
        
        smoother.influence[i] <- (((h.star.ii[i]*e.star[i])^2)/
                                    (((1-h.star.ii[i])^2)*sigma2*hat.star.trace))
        
        y.influence[i] <- ((1/((sigma2)*hat.trace))
                           *(((e[i]^2)*h.ii[i])/((1-h.ii[i])^2)))
      }
      return(list(betas.influence=betas.influence,
                  smoother.influence=smoother.influence, 
                  y.influence=y.influence))  
    }
    else
    {
      N <- model$N
      p <- length(model$mu.coefficients)
      e <- as.matrix(model$residuals)
      hat <- hat.matrix(model)
      h.ii <- diag(hat)
      hat.trace <- sum(h.ii)
      sigma2 <- ((t(e)%*%e)/(N-hat.trace))
      y.influence <- vector(mode="numeric", length=N)
      r2 <- vector(mode="numeric", length=N)
      for (i in 1:N)
      {
        r2 <- ((e[i]^2)/(sigma2*(1-h.ii[i])))
        y.influence[i] <- ((1/((sigma2)*hat.trace))
                           *(((e[i]^2)*h.ii[i])/((1-h.ii[i])^2)))
      }
      
      plot(y.influence, pch = 19, las = 1, ylab = "Cook's Distance")
      
      return(y.influence)
    }
  }
  
  #### function: kim.bootstrap ####
  kim.bootstrap <- function(model, data, B=1000, seed=357814)
  {
    boot.Samples <- vector(mode="list", length=B)
    N <- model$N
    
    set.seed(seed=seed)
    for (i in seq_len(B))
    {
      if(N<99)
      {
        boot.Samples[[i]] <- data[sample(seq_len(N), N, replace=TRUE), ]
      }
      else
      {
        boot.Samples[[i]] <- data[sample(seq_len(100), 100, replace=TRUE), ]
      }
    }
    
    call <- model$call
    call.Formula <- deparse(call$formula)
    call.Family <- deparse(call$family)
    call.Data <- deparse(call$data)
    if(is.null(call$method))
    {
      call.Method <- "RS(100)"
    }
    else
    {
      call.Method <- deparse(call$method)
      if(call.Method=="mixed")
      {
        call.Method <- paste(call.Method,"(100,100)")
      }
      if(call.Method=="RS"||call.Method=="CG")
      {
        call.Method <- paste(call.Method,"(300)")
      }
    }
    call.Model <- paste("gamlss(formula=", call.Formula, ",family=", 
                        call.Family, ",data=","boot.Sample,","method=",
                        call.Method,",trace=FALSE)", sep="")
    
    boot.Models <- vector(mode="list", length=B)
    for (i in 1:B) 
    {
      boot.Sample <- boot.Samples[[i]]
      boot.Models[[i]] <- eval(parse(text=call.Model))
      rm(boot.Sample)
    }
    if(!is.null(model$mu.s))
    {
      boot.betas <- vector(mode="list", length=B)
      boot.smoother <- vector(mode="list", length=B)
      boot.y <- vector(mode="list", length=B)
      for (i in 1:B) 
      {
        boot.Sample <- boot.Samples[[i]]
        boot.betas[[i]] <- kim.aux(model=boot.Models[[i]])$betas.influence
        boot.smoother[[i]] <- kim.aux(model=boot.Models[[i]])$smoother.influence
        boot.y[[i]] <- kim.aux(model=boot.Models[[i]])$y.influence
        cat("Progress:", (i/B)*100, "%", "\r")
        if(i == B) cat("\nDone!\n")
      }
      return(list(boot.betas=unlist(boot.betas),
                  boot.smoother=unlist(boot.smoother),
                  boot.y=unlist(boot.y)))
    }
    else
    {
      boot.kim <- vector(mode="list", length=B)
      for (i in 1:B) 
      {
        boot.Sample <- boot.Samples[[i]]
        p <- length(boot.Sample$mu.coefficients)
        e <- as.matrix(boot.Sample$residuals)
        hat <- hat.matrix(boot.Sample)
        h.ii <- diag(hat)
        hat.trace <- sum(h.ii)
        sigma2 <- ((t(e)%*%e)/(N-hat.trace))
        y.influence <- vector(mode="numeric", length=N)
        r2 <- vector(mode="numeric", length=N)
        for (j in 1:N)
        {
          r2 <- ((e[j]^2)/(sigma2*(1-h.ii[j])))
          y.influence[i] <- ((1/((sigma2)*hat.trace))
                             *(((e[j]^2)*h.ii[j])/((1-h.ii[j])^2)))
        }
        boot.kim[i] <- y.influence
      }
      return(unlist(boot.kim))
    }
  }
  
  #### function: GAIC.bootstrap ####
  # Description: 
  # Provides the global deviances for B generated bootstrap samples
  GAIC.bootstrap <- function(model, data, B=1000, seed=357814)
  {
    deviance.aux <- function(model, boot.Sample)
    {
      N <- model$N
      call <- model$call
      call.Formula <- deparse(call$formula)
      call.Family <- deparse(call$family)
      call.Data <- deparse(call$data)
      call.Model <- paste("gamlss(formula=", call.Formula, ",family=", 
                          call.Family, ",data=", call.Data, "[-jj,],", 
                          "trace=FALSE)", sep="")
      
      GD <- vector(mode="numeric", length=N)
      for(j in 1:N)
      {
        assign("jj", j, envir=sys.frame())
        model.i <- eval(parse(text=call.Model))
        GD[j] <- GAIC(model)-GAIC(model.i)
      }
      
      return(GD)
    }
    
    boot.Samples <- vector(mode="list", length=B)
    N <- model$N
    
    set.seed(seed=seed)
    for (i in seq_len(B))
      boot.Samples[[i]] <- data[sample(seq_len(N), N, replace=TRUE), ]
    
    call <- model$call
    call.Formula <- deparse(call$formula)
    call.Family <- deparse(call$family)
    call.Data <- deparse(call$data)
    call.Model <- paste("gamlss(formula=", call.Formula, ",family=", 
                        call.Family, ",data=","boot.Sample,", 
                        "trace=FALSE)", sep="")
    
    boot.Models <- vector(mode="list", length=B)
    for (i in 1:B) 
    {
      boot.Sample <- boot.Samples[[i]]
      boot.Models[[i]] <- eval(parse(text=call.Model))
      rm(boot.Sample)
    }
    
    boot.CD <- vector(mode="list", length=B)
    for(i in 1:B)
    {
      boot.Sample <-boot.Samples[[i]]
      boot.CD[[i]] <- deviance.aux(model=boot.Models[[i]], 
                                   boot.Sample=boot.Sample)
      cat("Progress:", (i/B)*100, "%", "\r")
      if(i == B) cat("\nDone!\n")
    }
    return(unlist(boot.CD))
  }
  #### function: Bbase ####
  # Description:
  # provides de B matrix of basis for the univariate P-Splines
  Bbase <- function(x, ndx=20, deg=3)
  {
    if(length(x)<99)
    {
      ndx = 10
      deg = 3
      tpower <- function(x, t, p) {(x - t) ^ p * (x > t)}
      xl <- min(x, na.rm = TRUE)
      xr <- max(x, na.rm = TRUE)
      xmin <- xl - 0.01 * (xr - xl) 
      xmax <- xr + 0.01 * (xr - xl)
      dx <- (xmax - xmin) / ndx # DS increment 
      knots <- seq(xmin - deg * dx, xmax + deg * dx, by = dx)
      P <- outer(x, knots, tpower, deg)# calculate the power in the knots
      n <- dim(P)[2]
      D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg) # 
      B <- (-1) ^ (deg + 1) * P %*% t(D) 
      attr(B, "knots") <- knots[-c(1:(deg-1), (n-(deg-2)):n)]
      B  
    }
    else
    {
      tpower <- function(x, t, p) {(x - t) ^ p * (x > t)}
      xl <- min(x, na.rm = TRUE)
      xr <- max(x, na.rm = TRUE)
      xmin <- xl - 0.01 * (xr - xl) 
      xmax <- xr + 0.01 * (xr - xl)
      dx <- (xmax - xmin) / ndx # DS increment 
      knots <- seq(xmin - deg * dx, xmax + deg * dx, by = dx)
      P <- outer(x, knots, tpower, deg)# calculate the power in the knots
      n <- dim(P)[2]
      D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg) # 
      B <- (-1) ^ (deg + 1) * P %*% t(D) 
      attr(B, "knots") <- knots[-c(1:(deg-1), (n-(deg-2)):n)]
      B   
    }
  }
  
  #### Function: hat.matrix ####
  # Description: 
  # This function takes a gamlss model and extract 3 components:
  # hat, hat.star and hat.tilde, if the model has no smothers terms,
  # the function only returns the hat matrix
  # by now consider only models with location parameter
  hat.matrix <- function(model)
  {
    if (!is.gamlss(model)) 
      stop("Error: It is not a gamlss object.")
    if(!is.null(model$mu.s))
    {
      if (length(colnames(model$mu.s))>1)
        stop("Error: This method only compute the hat matrix for 
             a single spline.")
      if(length(colnames(model$mu.x))==2)
      {        
        y <- model$y
        variable <- colnames(model$mu.s)
        X <- as.data.frame(model$mu.x)
        X.s <- X[,variable]
        pbknots <- getSmo(model)$knots
        B <- Bbase(X.s)
        D2 <- diff(diag(length(pbknots)), diff=2)
        G <- t(D2)%*%D2
        lambda <- getSmo(model)$lambda
        aux <- gamlss(y~pb(X.s), family=model$call$family, trace=FALSE)
        W <- diag(aux$mu.wt)
        S <- B%*%solve(t(B)%*%W%*%B+lambda*G)%*%t(B)%*%W
        return(S)
      }
      else
      {
        y <- model$y
        variable <- colnames(model$mu.s)
        X <- as.data.frame(model$mu.x)
        X.s <- X[,variable]
        X <- X[ , -which(names(X) %in% variable)]
        X <- X[,-1]
        B <- Bbase(X.s)
        pbKnots <- getSmo(model)$knots
        D2 <- diff(diag(length(pbKnots)), diff=2)
        G <- t(D2)%*%D2
        lambda <- getSmo(model)$lambda
        aux <- gamlss(y~pb(X.s), family = model$call$family, trace=FALSE)
        W <- diag(aux$mu.wt)
        S <- B%*%solve(t(B)%*%W%*%B+lambda*G)%*%t(B)%*%W
        I <- diag(model$N)
        X.tilde <- (I-S)%*%as.matrix(X)
        y.tilde <- (I-S)%*%y
        hat.tilde <- t(I-S)%*%X.tilde%*%solve(t(X.tilde)
                                              %*%X.tilde)%*%t(X.tilde)%*%(I-S)
        hat.star <- S%*%(I-hat.tilde)
        hat <- S + (I-S)%*%hat.tilde
        return(list(hat=hat, hat.tilde=hat.tilde, hat.star=hat.star)) 
      }
    }
    else
    {
      X <- model$mu.x
      hat <- X%*%solve(t(X)%*%X)%*%t(X)
      return(hat)
    }
  }  
  
  N <- model$N
  call <- model$call
  if(is.na(match("data", names(model$call)))) 
    stop("no data are declared")
  if(!(method=="cook"||method=="LD"||method=="kim"||method=="pena"
       ||method=="GAIC"||method=="DF"))
    stop("this method is unvaliable, try: cook, LD, kim, pena, GAIC or DF")
  call.Formula <- deparse(call$formula)
  call.Family <- deparse(call$family)
  call.Data <- deparse(call$data)
  call.Model <- paste("gamlss(formula = ", call.Formula, ",family = ", 
                      call.Family, ",data = ", call.Data, "[-ii,],", 
                      "trace = FALSE)", sep = "")
  IM <- vector(mode="numeric", length=N)
  #### Method: likelihood.distance ####
  if(method=="LD")
  {
    for (i in 1:N)
    {
      assign("ii", i, envir=sys.frame())
      model.i <- eval(parse(text=call.Model))
      logL <- as.numeric(-logLik(model))
      logL0 <- as.numeric(-logLik(model.i))
      IM[i] <- 2*(logL-logL0)
    }
    if(print)
    {
      cat("i", "Likelihood Distance", "\n")
      for (i in 1:N)
        cat(i, IM[i],"\n")
    }
    
    if (plot&&bootstrap==F) 
    {
      plot(IM, pch=19, las=1, ylab="Likelihood Distance")
    }
    
    if(bootstrap)
    {
      if(hist)
      { 
        boot <- LD.bootstrap(model, data, B, seed)
        cut.lb <- quantile(boot, 0.025)
        cut.ub <- quantile(boot, 0.95)
        hist(boot, breaks=50, col="gray", freq=FALSE, main="", 
             xlab="Distribution of bootstraps Likelihood Distance")
        cat("\n","Consider influential points if likelihood distance
          are out of[", cut.lb,",",cut.ub,"]")
      }
      else
      {
        boot <- LD.bootstrap(model, data, B, seed)
        cut.lb <- quantile(boot, 0.025)
        cut.ub <- quantile(boot, 0.975)
        if(plot)
        {
          plot(IM, pch=19, las=1,  ylab="Likelihood Distance")
          if(cut.lb<0)
            abline(h=cut.lb, lty=2)
          abline(h=cut.ub, lty=2)
          identify(IM, labels=1:N) 
          cat("\n","Consider influential points if likelihood distance
          are out of[", cut.lb,",",cut.ub,"]")
        }
      }
      return(list(IM=IM,cut.lb=cut.lb,cut.ub=cut.ub))
    }
    return(IM)
  }
  #### Method: cook ####
  else if(method=="cook")
  {
    for (i in 1:N) 
    {
      IM[i] <- CD.i(model=model, i)  
    }
    if(print)
    {
      cat("i", "cookdistance", "\n")
      for (i in 1:N)
        cat(i, IM[i],"\n")
    }
    if (plot) 
    {
      if(bootstrap)
      {
        if(hist)
        {
          boot <- cook.bootstrap(model, data, B, seed=seed)
          cut <- quantile(boot, 0.95)
          hist(boot, breaks=50, col="gray", freq=FALSE, main="", 
               xlab="Distribution of bootstraps Cook's Distance")
          cat("\n", "Consider influential points if likelihood distance
            are bigger then", cut, "\n") 
        }
        else
        {
          boot <- cook.bootstrap(model, data, B, seed=seed)
          cut <- quantile(boot, 0.95)
          plot(IM, pch=19, las=1, ylab="Cook's Distance")
          abline(h=cut, lty=2)
          cat("\n", "Consider influential points if cook's distance
            are bigger then", cut, "\n") 
          identify(IM, labels=1:N) 
        }
        return(cut)
      }
      else
      {
        plot(IM, pch=19, las=1, ylab="Cook's Distance")
        abline(h = 4/N, lty = 2)
        cat("\n", "cutoff:", 4/N, "\n")
      }
    }
    return(IM)
  } 
  #### Method: GAIC ####
  else if(method =="GAIC")
  {
    for (i in 1:N)
    {
      assign("ii", i, envir=sys.frame())
      model.i <- eval(parse(text=call.Model))
      IM[i] <- GAIC(model)-GAIC(model.i)
    }
    if(print)
    {
      cat("i", "GAIC", "\n")
      for (i in 1:N)
        cat(i, IM[i],"\n")
    }
    
    if (plot) 
    {
      if(bootstrap)
      {
        if(hist)
        { 
          boot <- GAIC.bootstrap(model, data, B, seed)
          cut <- quantile(boot, 0.95)
          hist(boot, breaks=50, col="gray", freq=FALSE, main="", 
               xlab="Distribution of bootstraps of GAIC")
          cat("\n","Consider influential points if GAIC are bigger
              then", cut)
        }
        else
        {
          boot <- GAIC.bootstrap(model, data, B, seed)
          cut <- quantile(boot, 0.95)
          plot(IM, pch=19, las=1,  ylab="GAIC")
          abline(h=cut, lty=2)
          identify(IM, labels=1:N)
          cat("\n","Consider influential points if GAIC are bigger
              then", cut)
        }
        return(cut)
      }
      else
      {
        plot(IM, pch=19, las=1, ylab="GAIC")
      }
    }
    return(IM)
  }
  #### Method: kim ####
  else if(method=="kim")
  {
    if(!is.null(model$mu.s))
    {
      N <- model$N
      p <- length(model$mu.coefficients)
      variable <- colnames(model$mu.s)
      X <- as.data.frame(model$mu.x)
      X.s <- X[,variable]
      X <- X[ , -which(names(X) %in% variable)]
      X <- X[,-1]
      y <- model$y
      family <- model$family[1]
      
      Bbase <- function(x,  ndx=20, deg=3)
      {
        if(length(x)<99)
        {
          ndx = 10
          deg = 3
        }
        tpower <- function(x, t, p) {(x - t) ^ p * (x > t)}
        xl <- min(x, na.rm = TRUE)
        xr <- max(x, na.rm = TRUE)
        xmin <- xl - 0.01 * (xr - xl) 
        xmax <- xr + 0.01 * (xr - xl)
        dx <- (xmax - xmin) / ndx # DS increment 
        knots <- seq(xmin - deg * dx, xmax + deg * dx, by = dx)
        P <- outer(x, knots, tpower, deg) # calculate the power in the knots
        n <- dim(P)[2]
        D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg)
        Z <- (-1) ^ (deg + 1) * P %*% t(D) 
        attr(Z, "knots") <- knots[-c(1:(deg-1), (n-(deg-2)):n)]
        Z   
      }
      
      pbknots <- getSmo(model)$knots
      Z <- Bbase(X.s)
      D2 <- diff(diag(length(pbknots)), diff=2)
      G <- t(D2)%*%D2
      lambda <- getSmo(model)$lambda
      aux <- gamlss(y~pb(X.s), family = family, trace=FALSE)
      W <- diag(aux$mu.wt)
      S <- Z%*%solve(t(Z)%*%W%*%Z+lambda*G)%*%t(Z)%*%W 
      
      I <- diag(model$N)
      X.tilde <- (I-S)%*%as.matrix(X)
      y.tilde <- (I-S)%*%y
      e <- (I-hat.matrix(model)$hat)%*%y 
      h.ii <- diag(hat.matrix(model)$hat)
      hat.trace <- sum(h.ii)
      
      e.tilde <- (I-hat.matrix(model)$hat.tilde)%*%y.tilde
      h.tilde.ii <- diag(hat.matrix(model)$hat.tilde)
      
      e.star <- (I-hat.matrix(model)$hat.star)%*%y
      h.star.ii <- diag(hat.matrix(model)$hat.star)
      hat.star.trace <- sum(h.star.ii)
      
      sigma2 <- ((t(e)%*%e)/(N-hat.trace))
      
      betas.influence <- vector(mode="numeric", length=N)
      smoother.influence <- vector(mode="numeric", length=N)
      y.influence <- vector(mode="numeric", length=N)
      for (i in 1:N)
      {
        betas.influence[i] <- ((1/(p*sigma2))*(((e.tilde[i]^2 )
                                                *h.tilde.ii[i])/((1-h.tilde.ii[i])^2))) 
        
        smoother.influence[i] <- (((h.star.ii[i]*e.star[i])^2)/
                                    (((1-h.star.ii[i])^2)*sigma2*hat.star.trace))
        
        y.influence[i] <- ((1/((sigma2)*hat.trace))
                           *(((e[i]^2)*h.ii[i])/((1-h.ii[i])^2)))
      }
      if(plot)
      {
        if(bootstrap)
        {
          boot <- kim.bootstrap(model, data, B, seed)
          cut.betas <- quantile(boot$boot.betas, 0.95)
          cut.smoother <- quantile(boot$boot.smoother, 0.95)
          cut.y <- quantile(boot$boot.y, 0.95)
          par(mfrow=c(1,3))
          plot(betas.influence, pch=19, las=1, 
               ylab = "Influence on coefficients")
          abline(h=cut.betas, lty=2)
          identify(betas.influence, labels=1:N)
          plot(smoother.influence, pch=19, las=1, 
               ylab = "Influence on smoothers")
          abline(h=cut.smoother, lty=2)
          identify(smoother.influence, labels=1:N)
          plot(y.influence, pch=19, las=1, 
               ylab = "Influence on y")
          abline(h=cut.y, lty=2)
          identify(y.influence, labels=1:N)
          cat("reference value betas: ", cut.betas,
              "reference value smoother:", cut.smoother,
              "referene value response variable:", cut.y, "\n")
          par(mfrow=c(1,1))  
          return(list(cut.y=cut.y,cut.smoother=cut.smoother,cut.betas=cut.betas))
        }
        else
        {
          par(mfrow=c(1,3))
          plot(betas.influence, pch=19, las=1, 
               ylab = "Influence on coefficients")
          identify(betas.influence, labels=1:N)
          plot(smoother.influence, pch=19, las=1, 
               ylab = "Influence on smoothers")
          identify(smoother.influence, labels=1:N)
          plot(y.influence, pch=19, las=1, 
               ylab = "Influence on y")
          identify(y.influence, labels=1:N)
          par(mfrow=c(1,1))   
        }
      }
      if(print)
      {
        cat("i", "Influence on betas", "\n")
        for (i in 1:N)
          cat(i, betas.influence[i], "\n")
        cat("i", "Influence on smoothers", "\n")
        for (i in 1:N)
          cat(i, smoother.influence[i], "\n")
        cat("i", "Influence on y", "\n")
        for (i in 1:N)
          cat(i, y.influence[i], "\n")
      }
      return(list(betas.influence=betas.influence,
                  smoother.influence=smoother.influence, 
                  y.influence=y.influence))  
    }
    else
    {
      N <- model$N
      p <- length(model$mu.coefficients)
      e <- as.matrix(model$residuals)
      hat <- hat.matrix(model)
      h.ii <- diag(hat)
      hat.trace <- sum(h.ii)
      sigma2 <- ((t(e)%*%e)/(N-hat.trace))
      y.influence <- vector(mode="numeric", length=N)
      r2 <- vector(mode="numeric", length=N)
      for (i in 1:N)
      {
        r2 <- ((e[i]^2)/(sigma2*(1-h.ii[i])))
        y.influence[i] <- ((1/((sigma2)*hat.trace))
                           *(((e[i]^2)*h.ii[i])/((1-h.ii[i])^2)))
      }
      
      plot(y.influence, pch = 19, las = 1, ylab = "Cook's Distance")
      
      return(y.influence)
    } 
  }
  #### Method: pena ####
  else if(method=="pena")
  {
    if(is.null(model$mu.s))
    {
      models <- vector(mode="list", length=N)
      S <- matrix(0, N, N)
      p <- length(model$mu.coefficients)
      X <- model$mu.x
      betas = t(t(model$mu.coefficients))
      Y.fitted = X%*%betas
      for (i in 1:N)
      {
        assign("ii", i, envir=sys.frame())
        model.i <- eval(parse(text=call.Model))
        models[[i]] <- model.i
      }
      Y.ij <- matrix(0, N, N)
      for (i in 1:N)
      {
        betas.i <- t(t(models[[i]]$mu.coefficients))
        Y.i <- X%*%betas.i
        Y.ij[,i] <- Y.i 
      }
      for (i in 1:N) 
      {
        for(j in 1:N)
        {
          S[i,j] <- Y.fitted[i] - Y.ij[i,j]
        }
      }
      
      sig.2 <- (t(model$residuals)%*%model$residuals)/(N-p)
      h.ii <- diag(hat.matrix(model))
      for(j in 1:N)
      {
        si <- as.matrix(S[,j])
        IM[j] <- (t(si)%*%si)/(p*sig.2*h.ii[i])  
      }
      if(print)
      {
        cat("i", "Peña's Measure", "\n")
        for (i in 1:N)
          cat(i, IM[i], "\n")
      }
      if(plot)
      {
        plot(IM, pch=19, las=1, ylab="Penã's Measure")
        MAD <- (median(abs(IM-median(IM))))/0.6745
        cut <- median(IM+4.5*MAD) 
        abline(h=cut, lty=2)
        identify(IM, labels=1:N) 
        cat("Consider influential points if Peñas measure are bigger then: ", 
            cut)
      }
    }
    else
    {
      models <- vector(mode="list", length=N)
      S <- matrix(0, N, N)
      p <- length(model$mu.coefficients)
      X <- model$mu.x
      betas = t(t(model$mu.coefficients))
      Y.fitted = X%*%betas
      for (i in 1:N)
      {
        assign("ii", i, envir=sys.frame())
        model.i <- eval(parse(text=call.Model))
        models[[i]] <- model.i
      }
      Y.ij <- matrix(0, N, N)
      for (i in 1:N)
      {
        betas.i <- t(t(models[[i]]$mu.coefficients))
        Y.i <- X%*%betas.i
        Y.ij[,i] <- Y.i 
      }
      for (i in 1:N) 
      {
        for(j in 1:N)
        {
          S[i,j] <- Y.fitted[i] - Y.ij[i,j]
        }
      }
      
      sig.2 <- (t(model$residuals)%*%model$residuals)/(N-p)
      if(length(colnames(model$mu.x))==2)
      {
        h.ii <- diag(hat.matrix(model))
      }
      else
      {
        h.ii <- diag(hat.matrix(model)$hat) 
      }
      for(j in 1:N)
      {
        si <- as.matrix(S[,j])
        IM[j] <- (t(si)%*%si)/(sum(h.ii)*sig.2*h.ii[i])  
      }
      MAD <- (median(abs(IM-median(IM))))/0.6745
      cut <- median(IM+4.5*MAD)
      if(print)
      {
        cat("i", "Peña's Measure", "\n")
        for (i in 1:N)
          cat(i, IM[i], "\n")
        cat("Consider influential points if Peñas measure are bigger then: ", 
            cut)
      }
      if(plot)
      {
        plot(IM, pch=19, las=1, ylab="Penã's Measure")
        abline(h=cut, lty=2)
        identify(IM, labels=1:N)
        cat("Consider influential points if Peñas measure are bigger then: ", 
            cut)
      }
    }  
    return(list(IM=IM,cut=cut))
  }
  #### Method: DF 'diferença de valores ajustados' ####
  else if(method=="DF")
  {
    y <- model$y
    models <- vector(mode="list", length=N)
    for (i in 1:N)
    {
      assign("ii", i, envir=sys.frame())
      model.i <- eval(parse(text=call.Model))
      models[[i]] <- model.i
    }
    if (!is.gamlss(model)) 
      stop("Error: It is not a gamlss object.")
    Y.fitted = fitted.values(model)
    for (i in 1:N)
    {
      Y.fitted.i <- predict(models[[i]], newdata=data, type="response")
      IM[i] <- (Y.fitted[i] - y)^2
    } 
    identify(IM, labels=1:N)
    plot(IM, pch = 19, las = 1, ylab = "Difference of fitted values")
    
    return(IM)
  }
}