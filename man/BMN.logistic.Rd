\name{BMN.logistic}
\alias{BMN.logistic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of a binary Markov network using nodewise logistic regressions
}
\description{
This function aims to estimate the partial correlation matrix associated with a binary Markov network using the nodewise logistic regression approach proposed by Ravikumar et al. (2010).
}
\usage{
BMN.logistic(X, lambda, gamma = 0.25, bic = TRUE, verbose = FALSE, eps = 1e-08)
}

\arguments{
  \item{X}{The \eqn{n \times p}{n x p} data matrix.}
  \item{lambda}{A vector of tuning parameters. The length of \code{lambda} should be \eqn{p}.}
  \item{gamma}{A tuning parameter required in evaluating the extended version of Bayesian information criterion (EBIC), which can be any constant between 0 and 1. Default is 0.25. See `Details'.}
  \item{bic}{Whether to compute the EBIC. Default is TRUE.}
  \item{verbose}{Whether to print out intermediate iterations for every nodewise regression. Default is FALSE.}
  \item{eps}{Numeric scalar \eqn{>=0}, indicating the tolerance level for differentiating zero and non-zero edges: entries \eqn{<} \code{eps} will be set to 0. }
}
\details{
The function \code{BMN.logistic} fits \eqn{p}{p} \eqn{\ell_1}{\ell_1}-regularized logistic regressions to the data \eqn{X} to recover the partial correlation matrix of the binary Markov network. Internally, the function \code{glmnet} is called for each node \eqn{j=1,\ldots,p} using the \eqn{j}-th column of data \eqn{X} as the response and all remaining variables as the predictors to estimate the neighborhood of node \eqn{j}. The \eqn{j}-th component of \code{lambda} is used as the penalization parameter for the \eqn{j}-th logisitc regression. Finally, the results from \eqn{p} regressions are aggregated to obtain the symmetric partial correlation matrix. 

Model selection for each of the \eqn{p}{p} regressions in \code{BMN.logistic} is done by minimizing the EBIC (Barber and Darton, 2015), where the additional parameter \code{gamma} corresponds to some prior belif on the set of considered models. For details, please refer to Barber and Darton (2015) and Zak-Szatkowska and Bogdan (2011).

}
\value{
    \item{theta}{The partial correlation matrix (\eqn{p\times p}{p x p}) of the binary Markov network.}
    \item{adj}{The adjacency matrix (\eqn{p\times p}{p x p}) of the binary Markov network.}
    \item{EBIC}{The extended version of Bayesian information criterion (\eqn{1\times p}{1 x p}) for model selection.}
    \item{lambda}{The tuning parameter used (\eqn{1\times p}{1 x p}).}
}
\references{
Ravikumar, P., Wainwright, M. J., & Lafferty, J. D. (2010). High-dimensional Ising model selection using l1-regularized logistic regression. The Annals of Statistics, 38(3), 1287-1319.

Zak-Szatkowska, M., & Bogdan, M. (2011). Modified versions of the Bayesian information criterion for sparse generalized linear models. Computational Statistics & Data Analysis, 55(11), 2908-2924.

Barber, R. F., & Drton, M. (2015). High-dimensional Ising model selection with Bayesian information criteria. Electronic Journal of Statistics, 9(1), 567-607.
}
\author{
Jing Ma
}

\seealso{
\code{BMN::BMNPseudo}, \code{\link{glmnet}}
}
\examples{
library(glmnet)

set.seed(1)

p = 50    # number of variables
n = 100   # number of observations per replicate
n0 = 1000 # burn in tolerance
rho_high = 0.5  # signal strength 
rho_low = 0.1   # signal strength 
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

lambda = rep(0.1, p)
fit = BMN.logistic(dat, lambda)

}
