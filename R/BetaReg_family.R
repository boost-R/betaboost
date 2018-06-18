

# mboost family for boosting 'classical' beta regression
# location parameter 'mu' is modeled by additive predictor
# precision parameter 'phi' is estimated as a scalar
# uses the parametrization as 'betareg' package by Zeileis et. al.
# The parametrization in Mikis 'gamlss' package for BE() differs
# slightly, sigma = 1/sqrt(phi + 1)


BetaReg <- function(mu = NULL, phirange =  c(.001, 1000)){
  
  phi <- 1 # just to initialize, overwritten in first step
  
  # loss is negative log likelihood; f is additive predictor (eta)
  # logit link -> plogis(f) = mu
  loss_mu <- function(phi, y, f) {
    - 1 * (lgamma(phi) - lgamma(plogis(f) * phi) -
             lgamma((1 - plogis(f)) * phi) + (plogis(f) * phi - 1) * log(y) +
             ((1 - plogis(f)) * phi - 1) * log(1 - y))
  }
  # to optimize phi
  risk_phi <- function(phi, y, fit, w = 1) {
    sum(w * loss_mu(y = y, f = fit, phi = phi))
  }
  
  # for output
  risk <- function( y, f, w = 1) {
    sum(w * loss_mu(y = y, f = f, phi = phi))
  }
  
  check_y <- function(y){
    if(!is.numeric(y) || !is.null(dim(y)) ) stop("y is not a numeric vector")
    if(any(y >= 1)) stop("y >=1 but trying to do beta-regression")
    if(any(y <= 0)) stop("y <=0 but trying to do beta-regression")
    return(y)
  }
  
  # ngradient is first derivative of log likelihood w.r.t. f
  ngradient <- function(y, f, w = 1) {
    # estimate phi
    phi <<- optimize(risk_phi, interval = phirange, y = y,
                     fit = f, w = w)$minimum
    # compute partial derivative
    ngr <-  exp(f)/(1 + exp(f))^2 * (phi * (qlogis(y) - (digamma(plogis(f) * phi) -
                                                           digamma((1 - plogis(f)) * phi))))
    return(ngr)
  }
  # starting value: mean(y)
  offset <- function(y, w) {
    if (!is.null(mu)) {
      RET <- qlogis(mu)
    }
    else {
      RET <- qlogis(mean(y))
    }
    return(RET)
  }
  # use the Family constructor of mboost
  mboost::Family(ngradient = ngradient, risk = risk, offset = offset,
                 response = function(f) plogis(f),
                 check_y = check_y,
                 nuisance = function() return(phi[length(phi)]),
                 name = "Beta-Regression (logit link)")
}
