#' @export
BMN.logistic <-
  function(X,
           lambda,
           gamma = 0.25,
           bic = TRUE,
           verbose = FALSE,
           eps = 1e-08) {
    n = dim(X)[1]
    p = dim(X)[2]
    
    if (length(lambda)!=p){
      stop('The tuning parameter should be of the same length as the number of variables!')
    }
    
    theta = matrix(0, p, p)
    bic.val = rep(0, p)
    
    ## To get the empirical covariance matrix
    for (r in 1:p) {
      if (verbose) {
        cat("Estimating a binary Markov network for node ", r, ".....\n")
      }
      Y = factor(X[, r])
      Xmat = X[, -r]
      beta = glmnet(Xmat,Y,lambda = lambda[r],family = "binomial",intercept = FALSE)$beta
      theta[r, -r] = as.numeric(beta) / 2
      
      nbhr = sum(abs(theta[r,-r]) > eps)
      if (bic) {
        bic.val[r] = 2 * logit.loss(X, r, theta) + nbhr * (log(n) + 2 * gamma * log(p - 1))
      }
    }
    
    ## Symmetrization
    theta = (theta + t(theta)) / 2
    diag(theta) <- 0
    adj = (abs(theta) > eps)
    
    # Return theta, the adjacency matrix, EBIC value if bic=TRUE otherwise zero, the tuning parameter lambda
    return(list(
      theta = theta,
      adj = adj,
      EBIC = bic.val,
      lambda = lambda
    ))
  }
