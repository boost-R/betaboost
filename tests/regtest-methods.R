## test methods 
library(betaboost)
require(gamlss.dist)

# simple simulated example
set.seed(1234)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)
y <- rBE(n = n, mu = plogis(x1 +x2),
         sigma = plogis(x3 + x4))
data <- data.frame(y ,x1, x2, x3, x4)
data <- data[!data$y %in% c(0,1),]
rm(x1,x2,x3,x4,y)


mod1 <- betaboost(formula = y ~ x1 + x2, data = data, form.type = "classic",
                  iterations = 120, sl = 0.1)
mod2 <- betaboost(formula = y ~ x1 + x2, phi.formula = y ~ x3 + x4, 
                  data = data, form.type = "classic",
                  iterations = 120, sl = 0.1)

# confidence intervals
suppressWarnings(confint(mod1, B.mstop = 2, B = 10))
try(confint(mod2, B.mstop = 2, B = 10))

# predictions
predict(mod1)
predict(mod2)
predict(mod2, parameter = "mu")
predict(mod2, parameter = "phi")
