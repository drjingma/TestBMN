#' @importFrom evd pgumbel

BMN.onesample.wrapper <-
  function(X,
           lambda,
           alpha.global = 0.05,
           multiTest = TRUE,
           alpha.multi = 0.10,
           verbose = FALSE) {
    nX <- nrow(X)
    p <- ncol(X)
    thetaX <- BMN.logistic(X, lambda, bic = F)$theta
    fitX <- debias(X, thetaX)
    check_theta_X <- fitX$theta
    check_s_X <- fitX$s
    
    # Compute the test statistic
    diag(check_s_X) <- rep(1, p)
    W <- check_theta_X ^ 2 / (check_s_X / nX)
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
    W = sqrt(W)
    diffNetwork <- NULL
    if (multiTest){
      tau <- find.tau(W, alpha.multi)
      diffNetwork <- W*(W>tau) #may not be symmetric sometimes
    }
    
    return(
      list(
        statistic = M,
        pvalue = pval,
        reject = rej,
        W = W,
        thetaInit = thetaX,
        theta = check_theta_X,
        network = diffNetwork)
    )
  }
