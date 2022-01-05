debias <-
function(X, theta) {
  n <- nrow(X)
  p <- ncol(X)
  
  score <- array(0, c(n, p, p))
  check_theta <- matrix(0, p, p) #debiased estimate
  check_s <- matrix(0, p, p) #variance
  for (r in 1:p) {
    for (t in 1:p) {
      if (t == r) {
        next
      }
      # Compute the score vector
      u <-  X[,-r] %*% theta[r,-r]
      dot_f <- (exp(u) - exp(-u)) / (exp(u) + exp(-u))
      ddot_f <- (4 * exp(2 * u)) / (1 + exp(2 * u)) ^ 2
      if (p==3){
        ut = X[,-c(r, t)] * theta[t, -c(r, t)]
        ur = X[,-c(r, t)] * theta[r, -c(r, t)]
      } else{
        ut = X[,-c(r, t)] %*% theta[t, -c(r, t)]
        ur = X[,-c(r, t)] %*% theta[r, -c(r, t)]
      }
       
      score[, r, t] <- (X[, t] * exp(-X[, t] * ut) * cosh(X[, t] * theta[r, t] + ur)) / (exp(-ut) * cosh(theta[r, t] + ur) +
                                                                                           exp(ut) * cosh(-theta[r, t] + ur))
      
      # Compute the debiased estimate
      numerator <- mean(score[, r, t] * (X[, r] - dot_f))
      denominator <- mean(score[, r, t] * X[, t] * ddot_f)
      check_theta[r, t] <- theta[r, t] + numerator / denominator
      
      # Compuate variance of theta_rt
      tmp <- score[, r, t] ^ 2 * ddot_f
      check_s[r, t] <- 1 / (4 * mean(tmp))
    }
    ### ---CAUTIOUS! NO NEED TO SYMMETRIZE THE DEBIASED ESTIMATE!!!---
    # check_s = (check_s + t(check_s)) / 2
    # check_theta = (check_theta + t(check_theta)) / 2
    diag(check_theta) <- 0
  }
  return(list(v = score, theta = check_theta, s = check_s))
}
