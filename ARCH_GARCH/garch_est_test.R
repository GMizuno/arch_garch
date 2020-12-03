setwd("C:/Users/Gabriel/OneDrive/TCC/Codigos/R/ARCH_GARCH")
source("garch_est.R")

set.seed(1)
w <- .5; alpha1 <- .13; beta1 <- .86
g1 <- garch11(1000, alpha1, beta1, w)
g1 <- g1$rt

opt1 <- optim(par = log(rep(.1, 3)), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1, trace = TRUE),
              rt = g1, hessian = TRUE)
opt1$par
exp(opt1$par)

opt2 <- optim(par = c(0,0,0), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1),
              rt = g1, hessian = TRUE)
opt2$par
exp(opt2$par)

opt3 <- optim(par = c(5,-5,-5), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1),
              rt = g1, hessian = TRUE)
opt3$par
exp(opt3$par)

opt4 <- optim(par = c(1.4167085, -1.7879985, -0.1927022), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1),
              rt = g1, hessian = TRUE)
opt4$par
exp(opt4$par)

opt5 <- optim(par = c(10, -10, -10), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1),
              rt = g1, hessian = TRUE)
opt5$par
exp(opt5$par)