setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\ARCH_GARCH)")
source("garch_est.R")
source("garch11.R")
require(numDeriv)

# Gerando a serie ---------------------------------------------------------
set.seed(1)
n <- 1000
pars <- c(.5, .13, .86)
g1 <- garch11(n, pars)
g1 <- g1$rt

# Exemplo 1 ---------------------------------------------------------------
opt1 <- optim(par = log(rep(.1, 3)), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1),rt = g1, n = 1000, 
              hessian = TRUE)
opt1$par
exp(opt1$par)
pars
opt1$hessian

llike_garch(g1, exp(opt1$par), n)
hessian(llike_garch, exp(opt1$par), rt = g1, n = n)

# Exemplo 2 ---------------------------------------------------------------
opt2 <- optim(par = c(0,0,0), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1), rt = g1, n = 1000,
              hessian = TRUE)
opt2$par
exp(opt2$par)
opt2$hessian

llike_garch(g1, exp(opt2$par), n)
hessian(llike_garch, exp(opt2$par), rt = g1, n = n)

# Exemplo 3 ---------------------------------------------------------------
opt3 <- optim(par = c(5,-5,-5), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1), rt = g1, n = 1000,
              hessian = TRUE)
opt3$par
exp(opt3$par)
opt3$hessian

llike_garch(g1, exp(opt3$par), n)
hessian(llike_garch, exp(opt3$par), rt = g1, n = n)

# Exemplo 4 ---------------------------------------------------------------
opt4 <- optim(par = c(1.4167085, -1.7879985, -0.1927022), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1), rt = g1, n = 1000,
              hessian = TRUE)
opt4$par
exp(opt4$par)

opt4$hessian

llike_garch(g1, exp(opt4$par), n)
hessian(llike_garch, pars, rt = g1, n = n)

# Exemplo 5 ---------------------------------------------------------------
opt5 <- optim(par = c(10, -10, -10), llike_garch_exp, 
              method = "BFGS", control = list(fnscale = -1), rt = g1, n = 1000,
              hessian = TRUE)
opt5$par
exp(opt5$par)
opt5$hessian

llike_garch(g1, exp(opt5$par), n)
hessian(llike_garch, exp(opt5$par), rt = g1, n = n)
