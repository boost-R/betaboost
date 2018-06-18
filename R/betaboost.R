
# formula 
# when ~ x1 + x2 + x3 -> glmboost LSS
# when ~ x1 + s(x2)   -> transform -> gambooost LSS
# when ~ bols(x1) + bbs(x2) -> gamboost LSS

betaboost <- function(formula = NULL, phi.formula = NULL, data = list(), sl = 0.01,
                      iterations = 100, form.type = c("classic", "betaboost"), 
                      start.mu = NULL, start.phi = NULL, 
                      stabilization = c("none", "MAD", "L2"),
                      y = NULL, x = NULL, mat.parameter = c("mean", "both"), 
                      mat.effect = c("linear", "smooth"), ...)
{
  no.phi <- is.null(phi.formula)
  if(any(c(!is.null(start.mu), !is.null(start.phi))) 
     & no.phi){ 
    start.mu <- NULL
    start.phi <- NULL
    warning("starting values will be ignored when only mu is modelled")
  }
  stabilization <- match.arg(stabilization)
  if(stabilization != "none" & no.phi){ 
    warning("stabilization will be ignored when only mu is modelled")
  }
  
  if(is.null(formula) & (is.null(x) | is.null(y))){
    stop("if no formula is provided, y and x must be specified")
  }
  
  if(!is.null(formula) & (!is.null(x) | !is.null(y))){
    y <- NULL
    x <- NULL
    warning("if formula is provided, y and x will be ingored")
  }
  if(!is.null(x) & !is.matrix(x)){
    x <- as.matrix(x)
  }
    
  
  if(any(start.mu <=0) | any(start.mu >= 1)) {
    start.mu <- NULL
    warning("start.mu must be >0 and <1; will be ignored")
  }
  if(any(start.phi <=0)) {
    start.phi <- NULL
    warning("start.phi must be >0 !; will be ignored")
  }
  # deal with formula
  if(!is.null(formula)){
  oformula <- formula
  labs <- attr(terms.formula(oformula, data = data), "term.labels")
  labs.phi <- ifelse(no.phi, "",  
                     attr(terms.formula(phi.formula, 
                                        data = data), "term.labels"))
  }else{
    if(mat.effect[1] == "linear"){
      if(mat.parameter[1] == "mean"){
    obj <- glmboost(y = y, x = cbind("intercept" = rep(1, nrow(x)), x), 
                    control = boost_control(mstop = iterations, nu = sl),
                    family = BetaReg(), ...)}
      if(mat.parameter[1] == "both"){
        dat <- as.data.frame(cbind(y, x))
        mformula <- paste(paste(names(dat)[1], " ~ ", sep = ""), 
                          paste(names(dat)[-1], collapse = " + "))
        obj <- glmboostLSS(formula = as.formula(mformula), data = dat,
                        control = boost_control(mstop = iterations, nu = sl),
                        families = BetaLSS())
        }
      }
  if(mat.effect[1] == "smooth"){
      if(mat.parameter[1] == "mean"){
        dat <- as.data.frame(cbind(y, x))
        mformula <- paste(paste(names(dat)[1], " ~ ", sep = ""), 
                          paste(names(dat)[-1], collapse = " + "))
        obj <- gamboost(formula = as.formula(mformula), data = dat, 
                    control = boost_control(mstop = iterations, nu = sl),
                    family = BetaReg(), ...)}
      if(mat.parameter[1] == "both"){
        dat <- as.data.frame(cbind(y, x))
        mformula <- paste(paste(names(dat)[1], " ~ ", sep = ""), 
                          paste(names(dat)[-1], collapse = " + "))
        obj <- gamboostLSS(formula = as.formula(mformula), data = dat,
                        control = boost_control(mstop = iterations, nu = sl),
                        families = BetaLSS())
        }
      }
    
  class(obj) <- c("betaboost", class(obj))
  return(obj)  
    
  }  
  
    if(form.type[1] != "gamboost")
  {
    # check if smooth terms are included
    ns <- sapply(c(labs, labs.phi), function(x) grepl(substr(x, 1, 2), pattern = "s\\(")) 
    anysmooth <- any(ns)
    mformula <- make_mboostform(oformula, data = data)
    if(!no.phi) mphi.formula <- make_mboostform(phi.formula, data = data)
  }
  if(form.type[1] == "gamboost")
  {
    # check if any base-learners defined
    ns <- sapply(c(labs, labs.phi), function(x) grepl(substr(x, nchar(x)-1, nchar(x)), 
                                                      pattern = ")") ) 
    anysmooth <- any(ns)
    mformula <- add_bolsform(oformula, data = data)
    if(!no.phi) mphi.formula <- add_bolsform(phi.formula, data = data)
  }
  
  # check if y is in range
  # mf <- model.frame(oformula, data = data)
  # y <- model.response(mf)
  # if(any(y == 0) || any(y == 1)) stop("response may not be 0 or 1 for beta regression") 
  # if(any(y < 0) || any(y > 1)) stop("response may only be between 0 and 1 for beta regression") 
  
  
  if(no.phi){
    if(!anysmooth){
      obj <- glmboost(mformula, data = data,
                      control = boost_control(mstop = iterations, nu = sl),
                      family = BetaReg(), ...)
    }
    if(anysmooth){
      obj <- gamboost(mformula, data = data,
                      control = boost_control(mstop = iterations, nu = sl),
                      family = BetaReg(), ...)
    }
  }else{
    # mf2 <- model.frame(phi.formula, data = data)
    # y2 <- model.response(mf2)
    # if(! identical(y, y2)) stop("response for both formulas must be the same") 
    if(!anysmooth){
      if(any(!is.null(start.mu), !is.null(start.phi), stabilization != "none")){
        
        obj <- glmboostLSS(formula = list(mu = formula(mformula), 
                                          phi = formula(mphi.formula)), data = data,
                           control = boost_control(mstop = iterations, nu = sl),
                           families = BetaLSS(mu = start.mu, phi = start.phi, 
                                              stabilization), ...)
      }else{
        obj <- glmboostLSS(formula = list(mu = formula(mformula), 
                                          phi = formula(mphi.formula)), data = data,
                           control = boost_control(mstop = iterations, nu = sl),
                           families = BetaLSS(), ...)
        
      }
      
    }
    if(anysmooth)
    {
      if(any(!is.null(start.mu), !is.null(start.phi), stabilization != "none")){
        
        obj <- gamboostLSS(formula = list(mu = formula(mformula), 
                                          phi = formula(mphi.formula)), data = data,
                           control = boost_control(mstop = iterations, nu = sl),
                           families = BetaLSS(mu = start.mu, phi = start.phi, 
                                              stabilization), ...)
      }
      else{
        obj <- gamboostLSS(formula = list(mu = formula(mformula), 
                                          phi = formula(mphi.formula)), data = data,
                           control = boost_control(mstop = iterations, nu = sl),
                           families = BetaLSS(), ...)
        
        
      }
    }
  }
  
  class(obj) <- c("betaboost", class(obj))
  return(obj)
}

