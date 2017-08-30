GibbsIteration <-
function(x, Theta)
{
  n = length(x)
  for (i in 1:n)
  {
    x[i] = 1
    foo = exp(sum(Theta[i,] * x))
    p = foo / (foo + 1 / foo)
    x[i] = rbinom(1, 1, prob = p) * 2 - 1
  }
  return(x)
}
