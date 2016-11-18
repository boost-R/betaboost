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

b1 <- betaboost(formula = y ~ x1 + x2, data = data,
      iterations = 120)
g1 <- glmboost(y ~ x1 + x2, data = data, family = BetaReg(),
               control = boost_control(mstop = 120))
stopifnot(identical(coef(b1), coef(g1)))


b2 <- betaboost(formula = y ~ x1 + x2, 
                phi.formula = y ~ x3 + x4, 
                data = data,
                iterations = 120)

g2 <- glmboostLSS(list(mu = y ~ x1 + x2, phi = y ~ x3 + x4), 
                  families = BetaLSS(), 
                  data = data)

stopifnot(identical(coef(b2, off2int = TRUE),  
                    coef(g2[120], off2int = TRUE)))

try(betaboost(formula = y ~ x1, phi.formula = x1 ~ x2, 
         data = data))



