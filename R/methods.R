## specific methods

confint.betaboost <- function(object, ...) {
  if (inherits(object, "mboostLSS"))
    stop("confidence intervals for beta regression with modeled precision parameter currently not implemented")
  NextMethod("confint", object)
}

predict.betaboost <- function(object, newdata = NULL,
                              type = c("link", "response", "class"), which = NULL,
                              aggregate = c("sum", "cumsum", "none"), ...) {
  NextMethod("predict", object)
}
