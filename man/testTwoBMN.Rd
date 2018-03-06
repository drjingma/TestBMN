\name{testTwoBMN}
\alias{testTwoBMN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two-sample testing of binary Markov networks
}
\description{\code{testTwoBMN} tests whether two binary Markov networks are the same, i.e.  whether the corresponding partial correlation matrices \eqn{\Theta_1=\Theta_2}{\Theta_1=\Theta_2}.}

\usage{
testTwoBMN(dat, lambda, alpha.global = 0.05, multiTest = TRUE, alpha.multi = 0.1)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{A list of length 2, consisting of the two data matrices generated from two populations. The data matrix from the \eqn{k}{k}-th population should be of dimension \eqn{n_k \times p}{n_k x p} for \eqn{k=1,2}.}
  \item{lambda}{A list of length 2, consisting of the two \eqn{1\times p}{1 x p} vectors of tuning parameters to be used in \code{BMN.logistic} for estimating the initial partial correlation matrix in each population.}
  \item{alpha.global}{The significance level for global testing. Default is 0.05.}
  \item{multiTest}{Whether to conduct multiple testing of pairwise edges. Default is \code{TRUE}.}
  \item{alpha.multi}{The level of false discovery rate in multiple testing, necessary if \code{multiTest=TRUE}. Default is 0.10.}
}
\details{
The function \code{testTwoBMN} implements the two-sample testing of binary Markov networks. It calculates the standardized pairwise statistic \eqn{W_{r,t}}{W_{r,t}} for each pair of edge \eqn{(r,t)} using the data matrices in \code{dat} together with the corresponding tuning parameters \code{lambda}. The test statistic for \eqn{\Theta_1=\Theta_2}{\Theta_1=\Theta_2} is
\deqn{M_{n,p}=\max_{1\le r < t \le p}~W_{r,t}^2}{%
M_{n,p}=max_{1\le r < t \le p}  W_{r,t}^2.
} 
The null \eqn{\Theta_1=\Theta_2}{\Theta_1=\Theta_2} is rejected if 
\deqn{M_{n,p}-4\log(p)+\log(\log(p))\ge q_{\alpha}}{%
M_{n,p}-4\log p+\log(\log p)\ge q_\alpha, 
} 
where \eqn{q_{\alpha}}{q_\alpha} is the \eqn{1-\alpha}{1-\alpha} quantile of the type I extreme value distribution with cumulative distribution function \eqn{\exp{-(8\pi)^{-1/2}e^{-z/2}}}{exp{-(8\pi)^{-1/2}e^{-z/2}}}. To recover the differential Markov network, \code{testTwoBMN} considers the multiple testing problem
\deqn{H_{0,r,t}: \theta_{r,t,1}-\theta_{r,t,2}=0, 1\le r < t \le p}{H_{0,r,t}: \theta_{r,t,1}-\theta_{r,t,2}=0, 1\le r < t \le p.}
The test statistic for each individual hypothesis \eqn{H_{0,r,t}}{H_{0,r,t}} is \eqn{W_{r,t}}{W_{r,t}}. The multiple testing procedure in \code{testOneBMN} rejects the null \eqn{H_{0,r,t}}{H_{0,r,t}} if \eqn{|W_{r,t}|>\tau}{|W_{r,t}|>\tau}, where the threshold \eqn{\tau}{\tau} is carefully chosen to ensure false discovery rate control at the pre-specified level. 

Note the tuning parameters \code{lambda} should be carefully chosen to ensure that the initial estimate of the partial correlation matrix is reasonably good. In particular, the optimal tuning parameters required in global testing and multiple testing may not be the same. See \code{\link{global.tune}} and \code{\link{multiple.tune.twosample}} for how to choose \code{lambda}. More details on the testing procedures are available in Cai et al. (2017+).

\code{testTwoBMN} calls \code{BMN.logistic} to estimate the initial partial correlation matrices using nodewise l1-regularized logistic regressions before calling \code{debias}. As a result, \code{testTwoBMN} returns a warning message if "one multinomial or binomial class has 1 or 0 observations" occurs in any of the \eqn{p}{p} logisitc regressions. The user should double check the data matrices in \code{dat} to avoid such situations before applying \code{testTwoBMN} again. 
}
\value{
\item{statistic}{The test statistic.}
\item{pvalue}{The \eqn{p}-value for testing the null \eqn{\Theta_1=\Theta_2}{\Theta_1=\Theta_2}.}
\item{reject}{Whether to reject the null \eqn{\Theta_1=\Theta_2}{\Theta_1=\Theta_2}.}
\item{W}{A \eqn{p \times p}{p x p} matrix consisting of the standardized pairwise statistics.}
\item{thetaXInit}{The initial estimated partial correlation matrix based on \code{X} using \code{BMN.logistic}.}
\item{thetaX}{The de-sparsified partial correlation matrix for the first population.}
\item{thetaYInit}{The initial estimated partial correlation matrix based on \code{Y} using \code{BMN.logistic}.}
\item{thetaY}{The de-sparsified partial correlation matrix for the second population.}
\item{network}{The estimated differential network with false discovery rate control at the pre-specified level \code{alpha.multi}.}
}
\references{
Cai, T. T., Li, H., Ma, J. & Xia, Y. (2017+). A testing framework for detecting differential micorbial networks.

}
\author{
Jing Ma
}

\seealso{
\code{\link{testOneBMN}}, \code{\link{global.tune}}, \code{\link{multiple.tune.twosample}}, \code{\link{BMN.logistic}}
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
Theta = vector("list", ncond)
weights = matrix(0, p, p)
upperTriangle(weights) <- runif(q, rho_low, rho_high) * (2*rbinom(q, 1, 0.5) - 1)
weights = weights + t(weights)
Theta[[1]] = weights * Amat

##---(3) Generate the difference matrix
Delta = matrix(0, p, p)
upperTriangle(Delta) <- runif(q, 1, 2)*sqrt(log(p)/n) * 
                       (match(1:q, sample(q, 5), nomatch = 0)>0)*(2*rbinom(q, 1, 0.5) - 1)
Delta = Delta + t(Delta)

Theta[[2]] = Theta[[1]] + Delta
Theta[[1]] = Theta[[1]] - Delta

##---(4) Simulate data and choose tuning parameter
dat = vector("list", ncond)
lambda = vector("list", ncond)
for (k in 1:ncond){
  dat[[k]] = BMN.samples(Theta[[k]], n, n0, skip=1)
  tmp = sapply(1:p, function(i) as.numeric(table(dat[[k]][,i]))[1]/n )
  while(min(tmp)<eps || abs(1-max(tmp)<eps)){
    dat[[k]] = BMN.samples(Theta[[k]], n, n0, skip=10)
    tmp = sapply(1:p, function(i) as.numeric(table(dat[[k]][,i]))[1]/n )
  }
  empcov = cov(dat[[k]])
  LambdaMat = outer(seq(1,15), sqrt(0.01 * diag(empcov) * log(p)/n))
  tune = global.tune(dat[[k]], LambdaMat)
  lambda[[k]] = tune$lambdaOpt
}

##---(5) Two-sample testing
test = testTwoBMN(dat, lambda)

}
