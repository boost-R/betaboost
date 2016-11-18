
betaboost <- function(formula, phi.formula = NULL, data = list(), 
                      iterations = 100)
{
 
 # check if y is in range
 oformula <- formula
# mf <- model.frame(oformula, data = data)
# y <- model.response(mf)
# if(any(y == 0) || any(y == 1)) stop("response may not be 0 or 1 for beta regression") 
# if(any(y < 0) || any(y > 1)) stop("response may only be between 0 and 1 for beta regression") 
 
 # check if smooth terms are included
 labs <- attr(terms.formula(formula), "term.labels")
 ns <- sapply(labs, function(x) grepl(substr(x, 1, 2), pattern = "s\\(") ) 
 anysmooth <- any(ns)
 
 if(is.null(phi.formula)){
   if(!anysmooth){
   obj <- glmboost(oformula, data = data,
                 control = boost_control(mstop = iterations),
                 family = BetaReg())
   }
   if(anysmooth){
   mformula <- make_mboostform(oformula)
   obj <- gamboost(mformula, data = data,
                   control = boost_control(mstop = iterations),
                   family = BetaReg())
   }
 }else{
   mf2 <- model.frame(phi.formula, data = data)
   y2 <- model.response(mf2)
   if(! identical(y, y2)) stop("response for both formulas must be the same") 
   
      
  obj <- glmboostLSS(formula = list(mu = formula(oformula), 
                                    phi = formula(phi.formula)), data = data,
                    control = boost_control(mstop = iterations),
                    families = BetaLSS())
  }


 return(obj)
}

