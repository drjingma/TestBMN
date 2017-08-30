nextSample <-
function(x, Theta, iters)
{
  for (i in 1:iters)
  {
    x = GibbsIteration(x, Theta)
  }
  return(x)
}
