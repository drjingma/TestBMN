#' @export

BMN.samples <-
function(theta, numSamples, burnIn, skip){
  p = dim(theta)[1]
  x = matrix(numeric(p * numSamples), nrow = numSamples)
  x[1,] = nextSample(rep(0, p), theta, burnIn)
  if (numSamples > 1)
  {
    for (i in 2:numSamples)
    {
      x[i,] = nextSample(x[i - 1,], theta, skip)
    }
  }
  return(x)
}
