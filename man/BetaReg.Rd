\name{BetaReg}
\alias{BetaReg}
\title{BetaReg family for boosting beta regression}
\usage{
BetaReg(mu = NULL, phirange =  c(.001, 1000))
}
\description{
\code{BetaReg} implements a \code{mboost} family object to boost beta regression.
}
\arguments{
\item{mu}{ starting value for location paramer.}
\item{phirange}{ range for the optimization of scale parameter phi.}

}
\details{
\code{BetaReg} implements 'classical' beta regression for model-based boosting.  Location parameter \code{mu} is modeled by additive predictor, scale parameter phi is simultaneously optimized as a scalar and treated as nuisance.
}
\author{Andreas Mayr <mayr@uni-bonn.de>
}
\references{
Mayr A, Weinhold L, Hofner B, Titze S, Gefeller O, Schmid M (2018). The betaboost package - a software tool for modeling bounded outcome variables in potentially high-dimensional data. International Journal of Epidemiology, doi: 10.1093/ije/dyy093.

Schmid M, Wickler F, Maloney KO, Mitchell R, Fenske N, & Mayr A. (2013). Boosted beta regression. PLoS ONE, 8(4), e61623.
}
\examples{
require(gamlss.dist)
# simple simulated example
set.seed(1234)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
y <- rBE(n = 100, mu = plogis(x1 + x2),
                  sigma = plogis(x3 + x4))
data <- data.frame(y ,x1, x2, x3, x4)
data <- data[!data$y \%in\% c(0,1),]

# 'classic' beta regression
b1 <- betaboost(formula = y ~ x1 + x2, data = data,
                iterations = 120)
coef(b1)

# compare to mboost
b2 <- glmboost(y ~ x1  + x2, data = data, family = BetaReg())
coef(b2)

# different values due to different defaults for step length and mstop


# same model with mboost
b3 <- glmboost(y ~ x1  + x2, data = data, family = BetaReg(), 
               control = boost_control(mstop = 120, nu = 0.01))
coef(b3)
coef(b1)

}
