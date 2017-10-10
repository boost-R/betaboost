\name{confint.betaboost}

\alias{confint}
\alias{confint.betaboost}

\title{
  Pointwise Bootstrap Confidence Intervals
}
\description{
  Compute pointwise bootstrap confidence intervals
}
\usage{
\method{confint}{betaboost}(object, ...)
}

\arguments{
  \item{object}{
    a fitted model object of class \code{betaboost} for which the confidence
    intervals should be computed.
  }
  \item{\dots}{
    additional arguments. See \code{\link{confint.mboost}} for further details.
  }
}
\details{
  Use a nested boostrap approach to compute pointwise confidence
  intervals for the predicted partial functions or regression
  parameters. The approach is further described in Hofner et al. (2016).
  
  Note that confidence intervals are currently only provided for beta regression
  models with constant precision parameter (i.e., \code{phi} cannot be modeled
  as a function of covariates).
}
\value{
  An object of class \code{glmboost.ci} or \code{mboost.ci} with special
  \code{print} and/or \code{plot} functions.
}
\references{
  Benjamin Hofner, Thomas Kneib and Torsten Hothorn (2016), 
  A Unified Framework of Constrained Regression. 
  \emph{Statistics & Computing}, \bold{26}, 1--14.
}
\author{
  Benjamin Hofner <benjamin.hofner@pei.de>
}
\seealso{
  \code{\link{confint.mboost}}
}
\keyword{methods}
