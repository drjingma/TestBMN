#' @export
multiple.tune.twosample <-
  function(dat,
           B,
           verbose = FALSE) {
    if (length(dat) != 2) {
      stop("The input data should be a list with two n by p matrices!")
    }
    n = sapply(dat, nrow)
    p = dim(dat[[2]])[2]
    ncond = length(dat)
    lambda = vector("list", ncond)
    empcov = lapply(dat, cov)
    approx_error = rep(0, length(B))
    for (b in 1:length(B)) {
      if (verbose) {
        cat("Choosing Lambda in iteration ", b, ".....\n")
      }
      for (k in 1:ncond) {
        lambda[[k]] = (B[b] / 20) * sqrt(diag(empcov[[k]]) * log(p) / n[k])
      }
      
      test = testTwoBMN(dat, lambda)
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
