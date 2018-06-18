\name{predict.betaboost}

\alias{predict}
\alias{predict.betaboost}

\title{
  Predictions for betaboost models
}
\description{
  Make predictions for betaboost models
}
\usage{
\method{predict}{betaboost}(object, newdata = NULL,
        type = c("link", "response", "class"), which = NULL,
        aggregate = c("sum", "cumsum", "none"), ...)
}

\arguments{
  \item{object}{
    a fitted model object of class \code{betaboost} for which the predictions 
    should be made.
  }
  \item{newdata}{ optional; A data frame in which to look for variables with
    which to predict or with which to plot the marginal prediction intervals.}
  \item{type}{ the type of prediction required.  The default is on the scale
    of the predictors; the alternative \code{"response"} is on
    the scale of the response variable.  Thus for a
    binomial model the default predictions are on the log-odds scale
    (probabilities on logit scale) and \code{type = "response"} gives
    the predicted probabilities.  The \code{"class"} option returns
    predicted classes.}
  \item{which}{ a subset of base-learners to take into account when computing
     predictions or coefficients. If \code{which} is given
     (as an integer vector or characters corresponding
     to base-learners), a list or matrix is returned. In \code{plot_PI}
     the argument \code{which} must be specified and it must be given as
     a character string containing the name of the variable.}
   \item{aggregate}{ a character specifying how to aggregate predictions
     or coefficients of single base-learners. The default
     returns the prediction or coefficient for the final number of
     boosting iterations. \code{"cumsum"} returns a
     matrix with the predictions for all iterations
     simultaneously (in columns). \code{"none"} returns a
     list with matrices where the \eqn{j}th columns of the
     respective matrix contains the predictions
     of the base-learner of the \eqn{j}th boosting
     iteration (and zero if the base-learner is not
     selected in this iteration).}
  \item{\dots}{
    additional arguments. Currently, only \code{parameter} is supported. 
    See \code{\link{predict.mboostLSS}} for further details.
  }
}
\details{
  The \code{predict} function can be used for predictions for the
  distribution parameters depending on new observations.
}
\author{
  Benjamin Hofner <benjamin.hofner@pei.de>
}
\seealso{
  \code{\link{predict.mboost}} and \code{\link{predict.mboostLSS}}
}
\examples{

## load data
data(QoLdata)

## define test data
test  <- QoLdata[1:10,]
train <- QoLdata[11:nrow(QoLdata),]

## fit model on training data
b1 <- betaboost(formula = QoL ~ arm + pain, data = train, 
                iterations = 500)

## predict on test data
predict(b1, newdata = test, type = "response")

## nuissance parameter phi
nuisance(b1)

## the same, but modelling also phi 
b2 <- betaboost(formula = QoL ~ arm + pain, data = train, 
                iterations = 1000,
                phi.formula = QoL ~ arm + pain)

## now also estimates for phi
predict(b2, newdata = test, type = "response")

}
\keyword{methods}
