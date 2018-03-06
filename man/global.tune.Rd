\name{global.tune}
\alias{global.tune}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Model selection for global testing}
\description{
The function \code{global.tune} selects the optimal tuning parameters for the global testing problem \eqn{H_0:\Theta=0}{H_0: \Theta=0} or \eqn{H_0:\Theta_1=\Theta_2}{H_0: \Theta_1=\Theta_2}.
}
\usage{
global.tune(X, Lambda, gamma = 0.25)
}
\arguments{
  \item{X}{The \eqn{n \times p}{n x p} data matrix.}
  \item{Lambda}{The \eqn{nlambda \times p}{nlambda x p} matrix of tuning parameters, each row representing one candidate vector of tuning parameter.}
  \item{gamma}{A control parameter used in evaluating the extended version of Bayesian information criterion (EBIC), which can be any constant between 0 and 1. Default is 0.25. See `Details'.}
}
\details{
Model selection for global testing is done via the extended version of Bayesian information criterion (EBIC). The function \code{global.tune} should be called twice in two-sample testing problems such that the optimal tuning parameters for each population can be selected.
}
\value{
  \item{EBIC}{The extended version of Bayesian information criterion (\eqn{nlambda \times p}{nlambda x p}) for model selection.}
  \item{lambdaOpt}{The selected optimal tuning parameter (\eqn{1 \times p}{1 x p}).}
}
\references{
Barber, R. F., & Drton, M. (2015). High-dimensional Ising model selection with Bayesian information criteria. Electronic Journal of Statistics, 9(1), 567-607.
}
\author{
Jing Ma
}

\seealso{
\code{\link{BMN.logistic}}
}
\examples{
library(glmnet)

set.seed(1)

p = 50    # number of variables
n = 100   # number of observations per replicate
n0 = 1000 # burn in tolerance
rho_high = 0.5  # signal strength 
rho_low = 0.1   # signal strength 
ncond = 2       # number of conditions to compare
eps = 8/n       # tolerance for extreme proportioned observations
q = (p*(p - 1))/2

##---(1) Generate the network  
g_sf = sample_pa(p, directed=FALSE)
Amat = as.matrix(as_adjacency_matrix(g_sf, type="both"))

##---(2) Generate the Theta  
weights = matrix(0, p, p)
upperTriangle(weights) = runif(q, rho_low, rho_high) * (2*rbinom(q, 1, 0.5) - 1)
weights = weights + t(weights)
Theta = weights * Amat
dat = BMN.samples(Theta, n, n0, skip=1)
tmp = sapply(1:p, function(i) as.numeric(table(dat[,i]))[1]/n )
while(min(tmp)<eps || abs(1-max(tmp)<eps)){
  dat = BMN.samples(Theta, n, n0, skip=10)
  tmp = sapply(1:p, function(i) as.numeric(table(dat[,i]))[1]/n )
}
empcov = cov(dat)
LambdaMat = outer(seq(1,15), sqrt(0.01 * diag(empcov) * log(p)/n))
tune = global.tune(dat, LambdaMat)
lambda = tune$lambdaOpt
}
