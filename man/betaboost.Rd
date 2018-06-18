\name{betaboost}
\alias{betaboost}
\title{
Function for boosting beta regression
}
\description{
Wrapper function to use mode-based boosting via mboost or gamboostLSS to fit beta regression.
}
\usage{
 betaboost(formula = NULL, phi.formula = NULL, data = list(), sl = 0.01,
           iterations = 100, form.type = c("classic", "betaboost"),
           start.mu = NULL, start.phi = NULL, 
           stabilization = c("none", "MAD", "L2"), 
           y = NULL, x = NULL, mat.parameter = c("mean", "both"), 
           mat.effect = c("linear", "smooth"),    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
description of the model to be fit for location parameter (\code{mu}).
}
  \item{phi.formula}{
description of the model to be fit for precision parameter (\code{phi}).
}
  \item{data}{
a data frame containing the variables.
}
  \item{iterations}{
number of boosting iterations to be used.
}
  \item{sl}{
step-length, default is 0.01
}
\item{form.type}{
  formula type: either \code{gamboost} (\code{y ~ bols(x1) + bbs(x2)}) using the \pkg{mboost} interface for specifying base-learners, or \code{classic} (\code{y ~ x1 + s(x2)}). 
  }

\item{start.mu}{
  offset value for \code{mu}, must be \eqn{> 0} and \eqn{< 1}; will be estimated from the outcome if none is specified (default). 
  }  

\item{start.phi}{
  offset value for \code{phi}, must be \eqn{> 0}; will be estimated from the outcome if none is specified (default). 
  }
\item{stabilization}{
governs if the negative gradient should be standardized in each boosting step. It can be either \code{"none"}, \code{"MAD"} or \code{"L2"}. Only applicable when besides \code{mu} also \code{phi} is modeled (extended beta regression).
}  
\item{y}{
response vector when no \code{formula} is specified.
}
\item{x}{
matrix of explanatory variables when no \code{formula} is specfied, per default they are included as linear effects: can be changed to smooth via \code{mat.effects}.
}
\item{mat.effect}{
controls what type of effect the entries in matrix \code{x} have on the response. It can be either \code{linear} or \code{smooth}, while linear is the default. Only applicable if no \code{formula} is provided, but \code{y} and \code{x}.
}

\item{mat.parameter}{
controls for which parameters the entries in matrix \code{x} are included. It can be either \code{mean} (classical beta regression) or \code{both} (extended beta regression), while mean is the default. Only applicable if no \code{formula} is provided, but \code{y} and \code{x}.
}
  \item{...}{
Additional arguments passed to \pkg{mboost} or \pkg{gamboostLSS} fitting functions.
  }
}
\details{
A wrapper function to fit beta regression via different boosting functions. 
}
\value{
A boosting object.
}
\references{
Mayr A, Weinhold L, Hofner B, Titze S, Gefeller O, Schmid M (2018). The betaboost package - a software tool for modeling bounded outcome variables in potentially high-dimensional data. International Journal of Epidemiology, doi: 10.1093/ije/dyy093.

Schmid M, Wickler F, Maloney KO, Mitchell R, Fenske N, & Mayr A. (2013). Boosted beta regression. PLoS ONE, 8(4), e61623.
}
\seealso{
The original function \code{\link{gamboostLSS}} and \code{\link{gamboost}} from the model-based boosting framework.
}
\examples{

#---------- data example 

data(QoLdata)

## Model for mu  
b1 <- betaboost(formula = QoL ~ arm + pain, data = QoLdata, 
                iterations = 500)

# Coeficients
coef(b1, off2int = TRUE)

# Phi
nuisance(b1)

## Model for mu and phi
b2 <- betaboost(formula = QoL ~ arm + pain, data = QoLdata, 
                iterations = 1000,
                phi.formula = QoL ~ arm + pain)


# Coeficients
coef(b2, off2int = TRUE)

#--------- simple simulated example

require(gamlss.dist)
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
b3 <- betaboost(formula = y ~ x1 + x2, data = data,
                iterations = 120)
coef(b3)

# beta regression including modeled precision parameter
b4 <- betaboost(formula = y ~ x1 + x2, 
                phi.formula = y ~ x3 + x4, 
                data = data, iterations = 120)


# with smooth effects for x1 and x3
b5 <- betaboost(formula = y ~ s(x1) + x2, 
                phi.formula = y ~ s(x3) + x4, form.type = "classic",
                data = data, iterations = 120)          


# using matrix interface
b6 <- betaboost(y = data$y, x = data[,2:5], iterations = 200, 
                mat.parameter = "both")          


}
