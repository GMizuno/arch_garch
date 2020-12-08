setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\ARCH_GARCH)")
source("garch_est.R")
source("garch11.R")
set.seed(1)

pars <- c(.5, .13, .86); n <- 1000
g1 <- garch11(n, pars)
g1 <- g1$rt

opt1 <- optim(par = log(rep(.1, 3)), llike_garch_exp, method = "BFGS",
              control = list(fnscale = -1), rt = g1, n = n, hessian = TRUE)
est <- exp(opt1$par); est
