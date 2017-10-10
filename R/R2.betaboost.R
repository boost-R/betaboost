##-------------
#
# 
#
# 
##--------------


R2.betaboost <- function(model, data, newdata = NULL){
  
  if(!any(class(model) %in% c("mboost", "mboostLSS"))) stop("no valid boosting model")
  
  
  testdata <- !is.null(newdata)
  mst <- mstop(model)
  
  if(testdata) df <- newdata 
  if(!testdata) df <- data
  
  if(any(class(model) %in% "mboost"))
  {
    
    which.response  <-  which(apply(data, function(x) identical(as.numeric(x), as.numeric(model$response)), 
                                    MARGIN = 2))
    name.response <- names(data)[which.response]
    
    # base-model
    model0 <- betaboost(model$response ~ 1, data = data, iterations = mst)
    
    # prediction
    
    predicted0 <-predict(model0, newdata = df, type = "link")
    predicted1 <-predict(model,  newdata = df, type = "link")
    predicted1r <-predict(model,  newdata = df, type = "response")
    
    # log-likelihoods:
    l0 <- sum(as.numeric(LH.betaboost(phi = model0$nuisance()[[mst]], 
                                      y = df[,name.response], f = predicted0, 
                                      type = "link")))
    l1 <- sum(as.numeric(LH.betaboost(phi = model$nuisance()[[mst]], 
                                      y = df[,name.response], f = predicted1, 
                                      type = "link")))
  }
  
  if(any(class(model) %in% "mboostLSS"))
  {
    which.response  <-  which(apply(data, function(x) identical(as.numeric(x), as.numeric(model$mu$response)), 
                                    MARGIN = 2))
    name.response <- names(data)[which.response]
    
    
    # base-model
    #model0 <- glmboostLSS(model$mu$response ~ 1, families = BetaLSS(),
    #                    data = data)
    #model0 <- model0[mst]
    model0 <- betaboost(model$mu$response ~ 1, data = data, iterations = max(mst))
    
    # prediction mu
    #predicted0  <- predict(model0$mu, newdata = df, type = "link")
    predicted0  <- predict(model0, newdata = df, type = "response")
    predicted1  <- predict(model$mu,  newdata = df, type = "response")
    predicted1r <- predict(model$mu,  newdata = df, type = "response")
    
    # prediction phi
    #phi0  <- predict(model0$phi, newdata = df, type = "response")
    phi0 <- model0$nuisance()[[max(mst)]]
    phi1  <- predict(model$phi,  newdata = df, type = "response")
    
    # log-likelihoods:
    l0 <- sum(as.numeric(LH.betaboost(phi = phi0, 
                                      y = df[,name.response], f = predicted0, 
                                      type = "response")))
    l1 <- sum(as.numeric(LH.betaboost(phi = phi0, 
                                      y = df[,name.response], f = predicted1, 
                                      type = "response")))
  }  
  
  
  # Maddala's pseudo R squared
  R2_maddala  <- 1-(exp((2/nrow(df))*(l0-l1)))
  
  # Correlation pseudo R (aus betareg package):
  R2_link     <-as.numeric(cor(predicted1, log(df[,name.response]/(1 - df[,name.response]))))^2
  R2_response <-as.numeric(cor(predicted1r, df[,name.response]))^2
  
  return(list(R2_maddala = R2_maddala, 
              R2_link = R2_link, R2_response = R2_response))
}

LH.betaboost <- function(phi, y, f, type = c("link", "response")) {
  
  if(type[1] == "response")
  {
    f <-  log(f/(1 - f))
  }
  
  
  (lgamma(phi) - lgamma(plogis(f) * phi) - 
      lgamma((1 - plogis(f)) * phi) + (plogis(f) * phi - 1) * log(y) + 
      ((1 - plogis(f)) * phi - 1) * log(1 - y))
}
