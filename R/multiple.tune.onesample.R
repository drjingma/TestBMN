#' @export
multiple.tune.onesample <-
  function(X,
           B,
           verbose = FALSE) {
    n = nrow(X)
    p = ncol(X)
    empcov = cov(X)
    approx_error = rep(0, length(B))
    for (b in 1:length(B)) {
      if (verbose) {
        cat("Choosing Lambda in iteration ", b, ".....\n")
      }
      lambda = (B[b] / 20) * sqrt(diag(empcov) * log(p) / n)
      
      test = testOneBMN(X, lambda)
      tmp.error = 0
      for (l in 1:10) {
        tmp = l * pnorm(sqrt(log(p)), lower.tail = F) / 10
        val = sum((abs(test$W) >= qnorm(1 - tmp))) / (tmp * p * (p - 1))
        tmp.error = tmp.error + (val-1) ^ 2
      }
      approx_error[b] = tmp.error
    }
    
    out <- list(error = approx_error, b = B[which.min(approx_error)])
    return(out)
    
  }
