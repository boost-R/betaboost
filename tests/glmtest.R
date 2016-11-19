library(betaboost)
require(gamlss.dist)

# simple simulated example
set.seed(1234)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
y <- rBE(n = 100, mu = plogis(x1 +x2),
         sigma = plogis(x3 + x4))
data <- data.frame(y ,x1, x2, x3, x4)
data <- data[!data$y%in%c(0,1),]
rm(x1,x2,x3,x4,y)

# just modelling mu

b1 <- betaboost(formula = y ~ x1 + x2, data = data, form.type = "classic",
      iterations = 12)
b1b <- betaboost(formula = y ~ x1 + x2, data = data, form.type = "gamboost",
                iterations = 12)
b1c <- betaboost(formula = y ~ x1 + x2, data = data, 
                 iterations = 12)
g1 <- glmboost(y ~ x1 + x2, data = data, family = BetaReg(),
               control = boost_control(mstop = 12))
stopifnot(identical(coef(b1),coef(b1b), coef(b1c), coef(g1)))

# now with smooth

b2 <- betaboost(formula = y ~ x1 + s(x2), data = data, form.type = "classic",
                iterations = 120)
b2b <- betaboost(formula = y ~ bols(x1, intercept = TRUE) + bbs(x2), data = data, form.type = "gamboost",
                 iterations = 120)
b2c <- betaboost(formula = y ~ bols(x1) + bbs(x2), data = data, 
                 iterations = 120)
g2 <- gamboost(y ~ bols(x1) + bbs(x2), data = data, family = BetaReg(),
               control = boost_control(mstop = 120))
stopifnot(identical(coef(b2),coef(b2b)))
stopifnot(identical(coef(b2c),coef(g2)))


# now modelling also phi

b3 <- betaboost(formula = y ~ x1 + x2, 
                phi.formula = y ~ x3 + x4, 
                data = data, form.type = "classic",
                iterations = 120)

g3 <- glmboostLSS(list(mu = y ~ x1 + x2, phi = y ~ x3 + x4), 
                  families = BetaLSS(), 
                  data = data)
stopifnot(identical(coef(b3, off2int = TRUE),  
                    coef(g3[120], off2int = TRUE)))

# now with smooth

b4 <- betaboost(formula = y ~ s(x1) + x2, 
                phi.formula = y ~ x3 + s(x4), 
                data = data, form.type = "classic",
                iterations = 120)

g4 <- gamboostLSS(list(mu = y ~ bbs(x1) + bols(x2, intercept = TRUE), 
                       phi = y ~ bols(x3, intercept = TRUE) + bbs(x4)), 
                  families = BetaLSS(), 
                  data = data)
stopifnot(identical(coef(b4),  
                    coef(g4[120])))


# Errprs 

try(betaboost(formula = y ~ x1, phi.formula = x1 ~ x2, 
         data = data))


