setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\AR-GARCH)")
source("ar1-garch11_est.R")
source("ar1-garch11_sim.R")
require(magrittr); require(numDeriv)

# Exemplo 1 (arquivo 4)---------------------------------------------------------------
pars <- c(-.003, -.053, .028, .06, .935); n <- 1000

s1 <- AR1_Garch11(n, pars)
pars_init <- c(.1, .1, log(c(.028, .06, .935)))

opt <- optim(pars_init, llike_ar_garch_exp, method = 'BFGS',
             control = list(fnscale = -1), rt = s1$rt, n = n, hessian = TRUE) 

par <- c(opt$par[1:2], exp(opt$par[3:5])) %>% round(3); par 
pars
opt$counts

llike_ar_garch_exp(par, rt = s1$rt, n = n)
hessian(func = llike_ar_garch, x = par, rt = s1$rt, n = n)
opt$hessian

# Exemplo 2 (arquivo 2)---------------------------------------------------------------
pars <- c(0, 0.121, .035, .163, .802); n <- 1000

s2 <- AR1_Garch11(n, pars)
pars_init <- c(0, .1, log(c(.035, .163, .802)))

opt <- optim(pars_init, llike_ar_garch_exp, method = 'BFGS',
             control = list(fnscale = -1), rt = s2$rt, n = n, hessian = TRUE) 

par <- c(opt$par[1:2], exp(opt$par[3:5])) %>% round(3); par 
pars
opt$counts

llike_ar_garch_exp(par, rt = s1$rt, n = n)
hessian(func = llike_ar_garch, x = par, rt = s2$rt, n = n)
opt$hessian

# Exemplo 3 (arquivo 3)---------------------------------------------------------------
pars <- c(-.05, 0.226, .324, .124, .777); n <- 1000

s3 <- AR1_Garch11(n, pars)
pars_init <- c(.1, .1, log(rep(.1, 3)))

opt <- optim(pars_init, llike_ar_garch_exp, method = 'BFGS',
             control = list(fnscale = -1), rt = s3$rt, n = n, hessian = TRUE) 

par <- c(opt$par[1:2], exp(opt$par[3:5])) %>% round(3); par 
pars
opt$counts

llike_ar_garch_exp(par, rt = s3$rt, n = n)
hessian(func = llike_ar_garch, x = par, rt = s3$rt, n = n)
opt$hessian


