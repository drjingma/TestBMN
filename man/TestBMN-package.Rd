\name{TestBMN-package}
\alias{TestBMN-package}
\alias{TestBMN}
\docType{package}
\title{
\packageTitle{TestBMN}
}
\description{
The TestBMN-package provides a testing framework for detecting differential binary Markov networks in the case of two-sample testing, or one binary Markov network in the case of one-sample testing.
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{TestBMN}
\packageIndices{TestBMN}

Binary Markov networks are commonly used to model the conditional independence relationships between the components of a random vector. Specifically, let \eqn{X}{X} be a \eqn{p}{p}-dimensional random vector. The pairwise relationships in \eqn{X}{X} can be captured by an undirected graph \eqn{G=(V,E)}{G=(V,E)}, whose vertex set \eqn{V=\{1,\ldots,p\}}{V=\{1,\ldots,p\}} and edge set \eqn{E}{E} corresponds to conditional dependence. The binary Markov network associated with \eqn{G}{G} has the joint distribution
\deqn{p(X) \propto \exp\left\{\sum_{(r,t)\in E} \theta_{r,t} X_r X_t \right\}}{p(X) \propto \exp\left\{\sum_{(r,t)\in E} \theta_{r,t} X_r X_t \right\},}
subject to a normalizing constant. 

Given \eqn{n_1}{n_1} i.i.d. observations from one population and \eqn{n_2}{n_2} i.i.d. observations from another popultation, it is often of interest to test whether the two underlying Markov networks are the same. Let \eqn{\Theta_1}{\Theta_1} and \eqn{\Theta_2}{\Theta_2} be the two unknown partial correlation matrices from the two populations. It suffices to test 
\deqn{H_0: \Theta_1=\Theta_2 \mbox{ v.s. } H_1: \Theta_1 \ne \Theta_2.}{H_0: \Theta_1=\Theta_2 \mbox{ v.s. } H_1: \Theta_1 \ne \Theta_2.}

If the global null \eqn{H_0}{H_0} is rejected, it becomes of interest to test 
\deqn{H_{0,r,t}: \theta_{r,t,1}-\theta_{r,t,2}=0 \mbox{ v.s. } H_{1,r,t}: \theta_{r,t,1}-\theta_{r,t,2}\ne 0, 1\le r < t \le p}{H_{0,r,t}: \theta_{r,t,1}-\theta_{r,t,2}=0 \mbox{ v.s. } H_{1,r,t}: \theta_{r,t,1}-\theta_{r,t,2}\ne 0, 1\le r < t \le p,}
so as to recover the differential network \eqn{\Theta_1 - \Theta_2}{\Theta_1 - \Theta_2}.

The TestBMN-package provides a rigorous framework for the above two-sample global and multiple testing problems via \code{\link{testTwoBMN}}. The global testing procedure in \code{\link{testTwoBMN}} is particularly powerful against alternatives where the differential network \eqn{\Theta_1 - \Theta_2}{\Theta_1 - \Theta_2} is sparse. The multiple testing procedure \code{\link{testTwoBMN}} provides asymptotic control of the false discovery proportion and the false discovery rate under suitable conditions. In addition, one can similarly conduct one-sample global and multiple testing problems via \code{\link{testOneBMN}}, if the goal is to estimate a binary Markov network with false discovery rate control.

More details about the method implemented in the TestBMN-package are available in Cai et al. (2017+). 
}
\author{
Jing Ma <crystal.jing.ma@gmail.com>
}
\references{
Cai, T. T., Li, H., Ma, J. & Xia, Y. (2018+). Differential Markov random fields analysis with applications to detecting differential micorbial communities.

Xia, Y., Cai, T., & Cai, T. T. (2015). Testing differential networks with applications to the detection of gene-gene interactions. Biometrika, 102(2), 247-266.

Ravikumar, P., Wainwright, M. J., & Lafferty, J. D. (2010). High-dimensional Ising model selection using l1-regularized logistic regression. The Annals of Statistics, 38(3), 1287-1319.
}

\keyword{Markov networks}

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
upperTriangle(weights) = runif(q, rho_low, rho_high) * (2*rbinom(q, 1, 0.5) - 1)
weights = weights + t(weights)
Theta[[1]] = weights * Amat

##---(3) Generate the difference matrix
Delta = matrix(0, p, p)
upperTriangle(Delta) = runif(q, 1, 2)*sqrt(log(p)/n) * 
                   (match(1:q, sample(q, q*0.1), nomatch = 0)>0)*(2*rbinom(q, 1, 0.5) - 1)
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
}

tune = multiple.tune.twosample(dat, 1:20, verbose = TRUE)
for (k in 1:ncond){
  empcov <- cov(dat[[k]])
  lambda[[k]] = (tune$b/20) * sqrt( diag(empcov) * log(p)/n )
}

##---(5) Two-sample testing
test = testTwoBMN(dat, lambda, alpha.multi = 0.20)

}
