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
      iterations = 120, sl = 0.1)
rb1 <- R2.betaboost(model = b1, data = data)
b1b <- betaboost(formula = y ~ x1 + x2, data = data, form.type = "gamboost",
                iterations = 120, sl = 0.1)
b1c <- betaboost(formula = y ~ x1 + x2, data = data, 
                 iterations = 120, sl = 0.1)
g1 <- glmboost(y ~ x1 + x2, data = data, family = BetaReg(),
               control = boost_control(mstop = 120))
rg1 <- R2.betaboost(model = g1, data = data)
stopifnot(identical(coef(b1),coef(b1b), coef(b1c), coef(g1)))


# now with smooth

b2 <- betaboost(formula = y ~ x1 + s(x2), data = data, form.type = "classic",
                iterations = 120, sl = 0.1)
rb2 <- R2.betaboost(model = b2, data = data)

b2b <- betaboost(formula = y ~ bols(x1) + bbs(x2), data = data, form.type = "gamboost",
                 iterations = 120, sl = 0.1)
b2c <- betaboost(formula = y ~ x1 + s(x2), data = data, 
                 iterations = 120, sl = 0.1)
g2 <- gamboost(y ~ bols(x1) + bbs(x2), data = data, family = BetaReg(),
               control = boost_control(mstop = 120))
rg2 <- R2.betaboost(model = g2, data = data)

stopifnot(identical(coef(b2),coef(b2b)))
stopifnot(identical(coef(b2c),coef(g2)))
cbind(rb1, rg1, rb2, rg2)


# now modelling also phi

b3 <- betaboost(formula = y ~ x1 + x2, 
                phi.formula = y ~ x3 + x4, 
                data = data,
                iterations = 120, sl = 0.1)
R2.betaboost(b3, data = data)

g3 <- glmboostLSS(list(mu = y ~ x1 + x2, phi = y ~ x3 + x4), 
                  families = BetaLSS(), 
                  data = data)
stopifnot(identical(coef(b3, off2int = TRUE),  
                    coef(g3[120], off2int = TRUE)))

# method 

b3b <- betaboost(formula = y ~ x1 + x2, 
                phi.formula = y ~ x3 + x4, 
                data = data, 
                iterations = 400, method = "noncyclic", sl = 0.1)
R2.betaboost(b3, data = data)

g3b<- glmboostLSS(list(mu = y ~ x1 + x2, phi = y ~ x3 + x4), 
                  families = BetaLSS(), 
                  data = data, method = "noncyclic")
stopifnot(identical(round(unlist(coef(b3b, off2int = TRUE)),2),  
                    round(unlist(coef(g3b[400], off2int = TRUE)),2)))

# now with smooth

b4 <- betaboost(formula = y ~ s(x1) + x2, 
                phi.formula = y ~ x3 + s(x4), 
                data = data,
                iterations = 120, sl = 0.1)

g4 <- gamboostLSS(list(mu = y ~ bbs(x1) + bols(x2), 
                       phi = y ~ bols(x3) + bbs(x4)), 
                  families = BetaLSS(), 
                  data = data)
stopifnot(identical(coef(b4),  
                    coef(g4[120])))

# change step length

# now change step-length
b3 <- betaboost(formula = y ~ x1 + x2, 
                phi.formula = y ~ x3 + x4, 
                data = data, iterations = 120, 
                method = "cyclic", sl = 0.01)
coef(b3, off2int = TRUE)


# different values
b4 <- betaboost(formula = y ~ x1 + x2, 
                phi.formula = y ~ x3 + x4, 
                data = data, iterations = 120, 
                method = "cyclic", sl = c(mu = 0.01, phi = .1))
coef(b4, off2int = TRUE)

# change offset 

b4 <- betaboost(formula = y ~ x1 + x2, 
                phi.formula = y ~ x3 + x4, 
                data = data, iterations = 120, 
                start.mu = 0.5, start.phi = 1,
                method = "cyclic", sl = c(mu = 0.01, phi = .1))
coef(b4, off2int = TRUE)
stopifnot(b4$mu$offset == qlogis(0.5) & 
          b4$phi$offset == log(1))

# should be ignored if only mu is modelled
b4 <- betaboost(formula = y ~ x1 + x2, start.mu = 0.1, 
                data = data, iterations = 120, sl = 0.1)
# should trigger a warning
stopifnot(identical(coef(b1, off2int = TRUE),
  coef(b4, off2int = TRUE)))


## matrix interface: linear

b5 <- betaboost(formula = NULL, data = data, 
                iterations = 120, sl = 0.1, y= data$y, x = data$x1, mat.effect = "linear")
g5 <- glmboost(y ~ x1, data = data, control = boost_control(mstop = 120), 
               family = BetaReg())
stopifnot(identical(as.numeric(coef(b5)), as.numeric(coef(g5))))
b6 <- betaboost(formula = NULL, data = data, 
                iterations = 120, sl = 0.1, y= data$y, x = data$x1, 
                mat.effect = "linear", mat.parameter = "both")
g6 <- glmboostLSS(y ~x1, data = data, control = boost_control(mstop = 120), 
                  families = BetaLSS())
stopifnot(identical(as.numeric(unlist(coef(g6))), as.numeric(unlist(coef(b6)))))
b6b <- betaboost(formula = NULL, data = data, 
                 iterations = 120, sl = 0.1, y= data$y, x = as.matrix(cbind(data$x1, data$x2, data$x3)), 
                 mat.effect = "linear", mat.parameter = "both")
g6b <- glmboostLSS(y ~x1 + x2 +x3, data = data, control = boost_control(mstop = 120), 
                  families = BetaLSS())


## matrix interface: nonlinear

b7 <- betaboost(formula = NULL, data = data,  mat.parameter = "mean",
                iterations = 120, sl = 0.1, y= data$y, x = data$x1, mat.effect = "smooth")
g7 <- gamboost(y ~ x1, data = data, control = boost_control(mstop = 120), 
               family = BetaReg())
stopifnot(identical(as.numeric(unlist(coef(b7))), as.numeric(unlist(coef(g7)))))
b8 <- betaboost(formula = NULL, data = data, 
                iterations = 120, sl = 0.1, y= data$y, x = data$x1, 
                mat.effect = "smooth", mat.parameter = "both")
g8 <- gamboostLSS(y ~x1, data = data, control = boost_control(mstop = 120), 
                  families = BetaLSS())
stopifnot(identical(as.numeric(unlist(coef(g8))), as.numeric(unlist(coef(b8)))))




## matrix interface: nonlinear and multiple x

b9 <- betaboost(formula = NULL, data = data, form.type = "classic", mat.parameter = "mean",
                iterations = 120, sl = 0.1, y= data$y, x = cbind(data$x1, data$x2, data$x3), mat.effect = "smooth")
g9 <- gamboost(y ~ x1 + x2 +x3, data = data, control = boost_control(mstop = 120), 
               family = BetaReg())
stopifnot(identical(as.numeric(unlist(coef(b9))), as.numeric(unlist(coef(g9)))))
b10 <- betaboost(formula = NULL, data = data, form.type = "classic",
                iterations = 12, sl = 0.1, y= data$y, x = cbind(data$x1, data$x2, data$x3), 
                mat.effect = "smooth", mat.parameter = "both")
g10 <- gamboostLSS(y ~x1 + x2 + x3, data = data, control = boost_control(mstop = 12), 
                  families = BetaLSS())
stopifnot(identical(as.numeric(unlist(coef(g10))), as.numeric(unlist(coef(b10)))))

##-------- data test
# check if dataset loads

data(dataqol2)
## take one time-point
dataqol <- dataqol2[dataqol2$time ==0,]
## remove missings
dataqol <- dataqol[complete.cases(dataqol[,c("QoL", "arm", "pain")]),]
## rescale outcome to [0,1]
dataqol$QoL <- dataqol$QoL/100

data(QoLdata)
identical(dataqol, QoLdata )


# fit simple model
beta1 <- betaboost(QoL ~ pain + arm, data = dataqol)
coef(beta1, off2int = TRUE)

beta2 <- betaboost(QoL ~ pain + arm, data = QoLdata)
coef(beta2, off2int = TRUE)
stopifnot(identical(coef(beta1), coef(beta2)))
#------------------

# check for y 

try(betaboost(QoL*2 ~ pain + arm, data = QoLdata), silent = TRUE)

try(betaboost(I(QoL-1)~ pain + arm, data = QoLdata), silent = TRUE)

try(betaboost(QoL*2 ~ pain + arm, phi.formula = QoL*2 ~ pain, 
              data = QoLdata), silent = TRUE)

try(betaboost(I(QoL-1)~ pain + arm, data = QoLdata), silent = TRUE)




# Errors 
try(betaboost(formula = y ~ x1, phi.formula = x1 ~ x2, 
         data = data))
b2d <- betaboost(formula = y ~ x1 + bbs(x2), data = data, form.type = "gamboost",
                 iterations = 120)

