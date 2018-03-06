\name{BMN.samples}
\alias{BMN.samples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sampling data using Gibbs sampling for use in examples
}
\description{
The function \code{BMN.samples} samples from a binary Markov network using Gibbs sampling. The resulting data matrix has binary measurements \eqn{\{-1,1\}}{\{-1,1\}}.
}
\usage{
BMN.samples(theta, numSamples, burnIn, skip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{The \eqn{p \times p}{p x p} partial correlation matrix corresponding to the binary Markov network.}
  \item{numSamples}{Number of samples to return.}
  \item{burnIn}{Number of samples to discard as burn in.}
  \item{skip}{Number of samples to discard in-btween returned samples.}
}
\details{
The function \code{BMN.samples} works similarly as \link[BMN]{BMNSamples} in the BMN-package. The only difference between the two versions is that \code{BMN.samples} implemented here returns \eqn{\{-1,1\}}{\{-1,1\}} as the binary outcome, whereas the one in the BMN-package returns \eqn{\{-1,1\}}{\{0,1\}} as the outcome. 
}
\value{
An \eqn{n \times p}{n x p} data matrix consisting of \eqn{\{-1,1\}}{\{-1,1\}}.
}

\author{
Jing Ma
}


\seealso{
\link[BMN]{BMNSamples}
}
\examples{
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

}
