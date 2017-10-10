#
#
#
# 

# should also deal with mixed formulas

make_mboostform <- function(formula, data = NULL)
{
  form <- as.formula(formula)
  labs <- attr(terms.formula(form, data = data), "term.labels")
  
  ns <- sapply(labs, function(x) grepl(substr(x, 1, 2), pattern = "s\\(") ) 
  if(! any(ns)) return(form)
  else{
    labs[ns] <-   gsub(x = labs[ns], "s\\(", "bbs\\(")
    labs[!ns] <- paste("bols(", labs[!ns], ")", sep = "")
  }
  rform <- paste(paste(all.vars(form[[2]]), " ~ ", sep = ""), 
                 paste(labs, collapse = " + "))
  return(as.formula(rform))
}

add_bolsform <-  function(formula, data = NULL)
{
  
  form <- as.formula(formula)
  labs <- attr(terms.formula(form, data = data), "term.labels")
  ns <- sapply(labs, function(x) grepl(substr(x, nchar(x)-1, nchar(x)), 
                                       pattern = ")") ) 
  if(! any(ns)) return(form)
  else{
    labs[!ns] <- paste("bols(", labs[!ns], ")", sep = "")
  } 
  rform <- paste(paste(all.vars(form[[2]]), " ~ ", sep = ""), 
                 paste(labs, collapse = " + "))
  return(as.formula(rform))
}