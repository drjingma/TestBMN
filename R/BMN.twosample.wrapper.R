BMN.twosample.wrapper <-
  function(X,
           Y,
           lambda1,
           lambda2,
           alpha.global = 0.05,
           multiTest = TRUE,
           alpha.multi = 0.10,
           verbose = FALSE) {
    nX <- nrow(X)
    nY <- nrow(Y)
    
    if (ncol(X) != ncol(Y)) {
      stop("The dimensions of the two data sets do not match!")
    }
    p <- ncol(X)
    thetaX <- BMN.logistic(X, lambda1, bic = F)$theta
    fitX <- debias(X, thetaX)
    check_theta_X <- fitX$theta
    check_s_X <- fitX$s
    
    thetaY <- BMN.logistic(Y, lambda2, bic = F)$theta
    fitY <- debias(Y, thetaY)
    check_theta_Y <- fitY$theta
    check_s_Y <- fitY$s
    
    # Compute the test statistic
    diag(check_s_X) <- rep(1, p)
    diag(check_s_Y) <- rep(1, p)
    W <-(check_theta_X - check_theta_Y) ^ 2 / (check_s_X / nX + check_s_Y / nY)
    diag(W) <- rep(0, p)
    M <- max(W)
    
    if (verbose) {
      print(M)
    }
    qalpha = -2*log (-(8*3.14159)^(1/2)*log (1-alpha.global))
    qalpha = qalpha + 4 * log(p) - log(log(p))
    if (verbose) {
      print(qalpha)
    }
    if (M > qalpha) {
      rej = TRUE
    } else {
      rej = FALSE
    }
    
    pval <- pgumbel(M - 4 * log(p) + log(log(p)),
                    loc = -log(8 * pi),
                    scale = 2,
                    lower.tail = F)
    
    ## Multiple testing
    if (multiTest){
      W = sqrt(W)
      tau <- find.tau(W, alpha.multi)
      diffNetwork <- W*(W>tau) #may not be symmetric sometimes
    }
    
    return(
      list(
        statistic = M,
        pvalue = pval,
        reject = rej,
        W = W,
        thetaXInit = thetaX,
        thetaYInit = thetaY,
        thetaX = check_theta_X,
        thetaY = check_theta_Y,
        network = diffNetwork
      )
    )
  }
