\name{betaboost}
\alias{betaboost}
\title{
Function for boosting beta regression
}
\description{
Wrapper function to use mode-based boosting via mboost or gamboostLSS to fit BetaRegression.
}
\usage{
 betaboost(formula, phi.formula = NULL, data = list(), sl = 0.1,
           iterations = 100, form.type = c("gamboost", "classic"),
           start.mu = NULL, start.phi = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
description of the model to be fit for location parameter.
}
  \item{phi.formula}{
description of the model to be fit for precission parameter.
}
  \item{data}{
a data frame containing the variables.
}
  \item{iterations}{
number of boosting iterations to be used.
}
  \item{sl}{
used step-length, default is 0.1
}
\item{form.type}{
  formula type: either \code{gamboost} (y ~ bols(x1) + bbs(x2)), specifying base-learners or \code{classic} (y ~ x1 + s(x2)). 
  }

\item{start.mu}{
  offset value for mu, must be >0 and <1; will be estimated from the outcome if none is specified (default). 
  }  

\item{start.phi}{
  offset value for phi, must be >0; will be estimated from the outcome if none is specified (default). 
  }    
  \item{...}{
Additional arguments passed to mboost or gamboostLSS fitting functions.
  }
}
\details{
A wrapper function...
}
\value{
A boosting object
}
\references{
Schmid, M., Wickler, F., Maloney, K. O., Mitchell, R., Fenske, N., & Mayr, A. (2013). Boosted beta regression. PLoS ONE, 8(4), e61623.
}
\seealso{
\code{\link{glmboost}}
}
\examples{

require(gamlss.dist)
# simple simulated example
set.seed(1234)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
y <- rBE(n = 100, mu = plogis(x1 +x2),
                  sigma = plogis(x3 + x4))
data <- data.frame(y ,x1, x2, x3, x4)
data <- data[!data$y \%in\% c(0,1),]

# 'classic' beta regression
betaboost(formula = y ~ x1 + x2, data = data,
          iterations = 120)

# beta regression including modeled precission parameter
betaboost(formula = y ~ x1 + x2, 
          phi.formula = y ~ x3 + x4, 
          data = data, iterations = 120)


}
