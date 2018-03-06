\name{testOneBMN}
\alias{testOneBMN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
One-sample testing of a binary Markov network
}
\description{
\code{testOneBMN} tests whether a binary Markov network is empty, i.e. whether the corresponding partial correlation matrix \eqn{\Theta=0}{\Theta=0}. In addition, \code{testOneBMN} simultaneously tests whether \eqn{\theta_{rt}=0}{\theta_{rt}=0} for each pair of edge \eqn{(r,t)}{(r,t)} to recover the Markov network with false discovery rate control.
}
\usage{
testOneBMN(X, lambda, alpha.global = 0.05, multiTest = TRUE, alpha.multi = 0.1)
}

\arguments{
  \item{X}{The \eqn{n \times p}{n x p} data matrix.}
  \item{lambda}{The \eqn{1\times p}{1 x p} vector of tuning parameters to be used in \code{BMN.logistic} for estimating an initial partial correlation matrix.
}
  \item{alpha.global}{The significance level for global testing. Default is 0.05.}
  \item{multiTest}{Whether to conduct multiple testing of pairwise edges. Default is \code{TRUE}.}
  \item{alpha.multi}{The level of false discovery rate in multiple testing, necessary if \code{multiTest=TRUE}. Default is 0.10.}
}
\details{
The function \code{testOneBMN} implements the one-sample testing of a binary Markov network. It calculates the standardized pairwise statistic \eqn{W_{r,t}}{W_{r,t}} for each pair of edge \eqn{(r,t)} using the data \code{X} together with the tuning parameter \code{lambda}. The test statistic for the global test \eqn{\Theta=0} is
\deqn{M_{n,p}=\max_{1\le r < t \le p}~W_{r,t}^2}{%
M_{n,p}=max_{1\le r < t \le p}  W_{r,t}^2.
} 
The null \eqn{\Theta=0}{\Theta=0} is rejected if 
\deqn{M_{n,p}-4\log(p)+\log(\log(p))\ge q_{\alpha}}{%
M_{n,p}-4\log p+\log(\log p)\ge q_\alpha, 
} 
where \eqn{q_{\alpha}}{q_\alpha} is the \eqn{1-\alpha}{1-\alpha} quantile of the type I extreme value distribution with cumulative distribution function \eqn{\exp{-(8\pi)^{-1/2}e^{-z/2}}}{exp{-(8\pi)^{-1/2}e^{-z/2}}}. To recover the underlying Markov network, \code{testOneBMN} considers the multiple testing problem
\deqn{H_{0,r,t}: \theta_{r,t}=0, 1\le r < t \le p}{H_{0,r,t}: \theta_{r,t}=0,1\le r < t \le p.}
The test statistic for each individual hypothesis \eqn{H_{0,r,t}}{H_{0,r,t}} is \eqn{W_{r,t}}{W_{r,t}}. The multiple testing procedure in \code{testOneBMN} rejects the null \eqn{H_{0,r,t}}{H_{0,r,t}} if \eqn{|W_{r,t}|>\tau}{|W_{r,t}|>\tau}, where the threshold \eqn{\tau}{\tau} is carefully chosen to ensure false discovery rate control at the pre-specified level. 

Note the tuning parameter \code{lambda} should be carefully chosen to ensure that the initial estimate of the partial correlation matrix is reasonably good. In particular, the optimal tuning parameters required in global testing and multiple testing may not be the same. See \code{\link{global.tune}} and \code{\link{multiple.tune.onesample}} for how to choose \code{lambda}. More details on the testing procedures are available in Cai et al. (2017+).

\code{testOneBMN} calls \code{\link{BMN.logistic}} to estimate an initial partial correlation matrix using nodewise \eqn{\ell_1}{\ell_1}-regularized logistic regressions. As a result, \code{testOneBMN} returns a warning message if "one multinomial or binomial class has 1 or 0 observations" occurs in any of the \eqn{p}{p} logisitc regressions. The user should double check the data matrix \code{X} to avoid such situations before applying \code{testOneBMN} again. 
}
\value{
\item{statistic}{The test statistic.}
\item{pvalue }{The \eqn{p}-value for testing the null \eqn{\Theta=0}{\Theta=0}.}
\item{reject}{Whether to reject the null \eqn{\Theta=0}{\Theta=0}.}
\item{W}{A \eqn{p \times p}{p x p} matrix consisting of the standardized pairwise statistics.}
\item{thetaInit}{The initial estimated partial correlation matrix using \code{BMN.logistic}.}
\item{theta}{The de-sparsified partial correlation matrix.}
\item{network}{The estimated network with false discovery rate control at the pre-specified level \code{alpha.multi}.}
}
\references{
Cai, T. T., Li, H., Ma, J. & Xia, Y. (2017+). A testing framework for detecting differential micorbial networks.
}
\author{
Jing Ma
}

\seealso{
\code{\link{testTwoBMN}}, \code{\link{global.tune}}, \code{\link{multiple.tune.onesample}}, \code{\link{BMN.logistic}}
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

lambda = rep(0.05, p)
test1 = testOneBMN(dat, lambda)
}
