
# formula 
# when ~ x1 + x2 + x3 -> glmboost LSS
# when ~ x1 + s(x2)   -> transform -> gambooost LSS
# when ~ bols(x1) + bbs(x2) -> gamboost LSS

betaboost <- function(formula, phi.formula = NULL, data = list(), 
                      iterations = 100, form.type = c("gamboost", "classic"))
{
   no.phi <- is.null(phi.formula)
   
   # deal with formula
   oformula <- formula
   labs <- attr(terms.formula(oformula), "term.labels")
   labs.phi <- ifelse(no.phi, "",  
                      attr(terms.formula(phi.formula), "term.labels"))
     
  if(form.type[1] != "gamboost")
  {
    # check if smooth terms are included
    ns <- sapply(c(labs, labs.phi), function(x) grepl(substr(x, 1, 2), pattern = "s\\(")) 
    anysmooth <- any(ns)
    mformula <- make_mboostform(oformula)
    if(!no.phi) mphi.formula <- make_mboostform(phi.formula)
  }
  if(form.type[1] == "gamboost")
  {
    # check if any base-learners defined
    ns <- sapply(c(labs, labs.phi), function(x) grepl(substr(x, nchar(x)-1, nchar(x)), 
                                                      pattern = ")") ) 
    anysmooth <- any(ns)
    mformula <- add_bolsform(oformula)
    if(!no.phi) mphi.formula <- add_bolsform(phi.formula)
  }
   
# check if y is in range
# mf <- model.frame(oformula, data = data)
# y <- model.response(mf)
# if(any(y == 0) || any(y == 1)) stop("response may not be 0 or 1 for beta regression") 
# if(any(y < 0) || any(y > 1)) stop("response may only be between 0 and 1 for beta regression") 


 if(no.phi){
   if(!anysmooth){
   obj <- glmboost(mformula, data = data,
                 control = boost_control(mstop = iterations),
                 family = BetaReg())
   }
   if(anysmooth){
   obj <- gamboost(mformula, data = data,
                   control = boost_control(mstop = iterations),
                   family = BetaReg())
   }
 }else{
  # mf2 <- model.frame(phi.formula, data = data)
  # y2 <- model.response(mf2)
  # if(! identical(y, y2)) stop("response for both formulas must be the same") 
   if(!anysmooth){
  obj <- glmboostLSS(formula = list(mu = formula(mformula), 
                                    phi = formula(mphi.formula)), data = data,
                    control = boost_control(mstop = iterations),
                    families = BetaLSS())
   }
   if(anysmooth)
   {
     obj <- gamboostLSS(formula = list(mu = formula(mformula), 
                                       phi = formula(mphi.formula)), data = data,
                        control = boost_control(mstop = iterations),
                        families = BetaLSS())
     
   }
 }

 return(obj)
}

