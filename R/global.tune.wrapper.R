global.tune.wrapper <-
  function(X,
           Lambda,
           gamma = 0.25,
           verbose = FALSE,
           eps = 1e-08) {
    n = dim(X)[1]
    p = dim(X)[2]
    bic.val = matrix(0, nrow(Lambda), ncol(Lambda))
    
    for (k in 1:nrow(Lambda)) {
      if (verbose) {
        cat("Choosing Lambda in iteration ", k, ".....\n")
      }
      res = BMN.logistic(X, Lambda[k,], gamma = gamma)
      bic.val[k,] = res$EBIC
    }
    lam.Index <- apply(bic.val, 2, which.min)
    
    out <- list(EBIC = bic.val, lambdaOpt = Lambda[lam.Index])
    return(out)
  }
