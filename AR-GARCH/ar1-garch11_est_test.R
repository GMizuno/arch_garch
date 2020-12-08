setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\AR-GARCH)")
source("ar1-garch11_est.R")
source("ar1-garch11_sim.R")

# Exemplo 1 (arquivo 4)---------------------------------------------------------------
pars <- c(-.003, -.053, .028, .06, .935); n <- 1000

s1 <- AR1_Garch11(pars, n)
pars_init <- c(.1, .1, 1, 1, 1)
llike_ar_garch(pars, s1$rt, n = n)

opt <- optim(pars_init, llike_ar_garch, method = 'BFGS',
             control = list(fnscale = -1), rt = s1$rt, n = n) 

par<- c(opt$par[1:2], exp(opt$par[3:5])); par
pars
opt$counts

# Exemplo 2 (arquivo 2)---------------------------------------------------------------
pars <- c(0, 0.121, .035, .163, .802); n <- 1000

s2 <- AR1_Garch11(pars, n)
pars_init <- c(0, .1, log(c(.035, .163, .802)))
llike_ar_garch(pars, s2$rt, n = n)

opt <- optim(pars_init, llike_ar_garch, method = 'BFGS',
             control = list(fnscale = -1), rt = s2$rt, n = n) 

par <- c(opt$par[1:2], exp(opt$par[3:5])); par
pars
opt$counts

# Exemplo 3 (arquivo 3)---------------------------------------------------------------
pars <- c(-.05, 0.226, .324, .124, .777); n <- 1000

s3 <- AR1_Garch11(pars, n)
pars_init <- c(.1, .1, log(rep(.1, 3)))
llike_ar_garch(pars, s3$rt, n = n)

opt <- optim(pars_init, llike_ar_garch, method = 'BFGS',
             control = list(fnscale = -1), rt = s3$rt, n = n) 

par<- c(opt$par[1:2], exp(opt$par[3:5])); par
pars
opt$counts

