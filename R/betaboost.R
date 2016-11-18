
betaboost <- function(formula, data = list(), iterations = 100)
{

 obj <- glmboost(formula, data = data,
                 control = boost_control(mstop = iterations),
                 family = BetaReg())


 return(obj)
}
