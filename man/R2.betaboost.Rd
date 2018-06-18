\name{R2.betaboost}
\alias{LH.betaboost}
\alias{R2.betaboost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computing pseudo R^2 for betaboost models. 
}
\description{
Computes different pseudo R^2 for betaboost models
}
\usage{
R2.betaboost(model, data, newdata = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A boosting model object for beta regression.
}
  \item{data}{
Underlying data frame
}
  \item{newdata}{
test-data (optional), if omitted R^2 is computed on \code{data}  (training-data)
}
}
\references{
Mayr A, Weinhold L, Hofner B, Titze S, Gefeller O, Schmid M (2018). The betaboost package - a software tool for modeling bounded outcome variables in potentially high-dimensional data. International Journal of Epidemiology, doi: 10.1093/ije/dyy093.
}
\examples{

# simple simulated example
require(gamlss.dist)
set.seed(1234)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
y <- rBE(n = 100, mu = plogis(x1 +x2),
         sigma = plogis(x3 + x4))
data <- data.frame(y ,x1, x2, x3, x4)
data <- data[!data$y\%in\%c(0,1),]
rm(x1,x2,x3,x4,y)

b1 <- betaboost(formula = y ~ x1 + x2, 
                phi.formula = y ~ x3 + x4, 
                data = data, form.type = "classic",
                iterations = 120)
R2.betaboost(b1, data = data)

}
