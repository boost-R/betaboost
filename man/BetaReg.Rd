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
\examples{
rnorm(120)
}
