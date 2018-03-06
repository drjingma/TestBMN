\name{multiple.tune.onesample}
\alias{multiple.tune.onesample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model selection for one-sample multiple testing
}
\description{
The function \code{multiple.tune.onesample} selects the optimal tuning parameters for the multiple testing problem \eqn{H_0: \theta_{r,t}=0}{H_0: \theta_{r,t}=0} for all \eqn{1\le r < t \le p}{1 \le r < t \le p}.
}
\usage{
multiple.tune.onesample(X, B, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{The \eqn{n \times p}{n x p} data matrix.}
  \item{B}{The set of candidate integer multipliers.}
  \item{verbose}{Whether to print out intermediate iteration steps. Default is \code{FALSE}.}
}
\details{
The false discovery rate control in the multiple testing problem 
\deqn{H_0: \theta_{r,t}=0, 1\le r < t \le p}{H_0: \theta_{r,t}=0, 1\le r < t \le p,} is based on approximating the number of false discoveries by \eqn{{2-2\Phi(t)}*p*(p-1)/2 }{{2-2\Phi(t)}*p*(p-1)/2}. Thus the optimal tuning parameter is selected with the principle of making the above approximation error to be as small as possible. The candidate tuning parameters are selected using a data-driven approach, therefore the user only needs to specify a candidate set of integer multipliers. Details on how the approximation error is defined are available in Xia et al. (2014).
}
\value{
\item{error }{The empirical version of the approximation error, evaluated over the range of integer multipliers in \code{B}.}
\item{b }{The optimal integer multiplier.}
}

\references{
Xia, Y., Cai, T., & Cai, T. T. (2015). Testing differential networks with applications to the detection of gene-gene interactions. Biometrika, 102(2), 247-266.
}
\author{
Jing Ma
}

\seealso{
\code{\link{testOneBMN}}
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

tune = multiple.tune.onesample(dat, 1:20, verbose = TRUE)

}
