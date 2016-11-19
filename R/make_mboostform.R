#
#
#
# 


make_mboostform <- function(formula)
{
  form <- as.formula(formula)
  labs <- attr(terms.formula(form), "term.labels")
  
  ns <- sapply(labs, function(x) grepl(substr(x, 1, 2), pattern = "s\\(") ) 
  if(! any(ns)) return(form)
  else{
    labs[ns] <-   gsub(x = labs[ns], "s\\(", "bbs\\(")
    labs[!ns] <- paste("bols(", labs[!ns], ", intercept = TRUE)", sep = "")
  }
  rform <- paste(paste(all.vars(form[[2]]), " ~ ", sep = ""), 
                 paste(labs, collapse = " + "))
  return(as.formula(rform))
}