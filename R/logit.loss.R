logit.loss <-
function(X, r, theta) {
  n <- nrow(X)
  u <- X[,-r] %*% theta[r,-r]
  val = log(exp(u) + exp(-u)) - X[, r] * u
  
  return(mean(val))
}
