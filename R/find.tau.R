find.tau <-
function(W, alpha=0.1) {
  p = nrow(W)
  tau = seq(0, sqrt(4 * log(p) - 2 * log(log(p))), length.out = 100)
  R <- sapply(tau, function(a) max(c(sum(abs(W) > a) / 2, 1)))
  
  val_tau <- pnorm(tau, lower.tail = F) * p * (p - 1) / R
  if (min(val_tau) <= alpha) {
    tau0 = tau[min(which((val_tau - alpha <= 0) == TRUE))]
  } else {
    tau0 = 2 * sqrt(log(p))
  }
  return(tau0)
}
