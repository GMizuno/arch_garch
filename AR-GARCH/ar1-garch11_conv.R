setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\AR-GARCH)")
source('ar1-garch11_sim.R')
source('ar1-garch11_est.R')
source('ggplot_graficos.R')
require(purrr)

# Monte Carlo -------------------------------------------------------------
gerando <- function(n, par, pars_init){
  # Simula os dados
  rt <- AR1_Garch11(n, pars)
  
  # Faz a otimizacao
  opt <- optim(par = pars_init, fn = llike_ar_garch, method = "BFGS",
               control = list(fnscale=-1), rt = rt$rt, n = n)
  
  opt_par <- opt$par
  # Guarda em um data frame
  return(data.frame(phi0 = opt_par[1],phi1 = opt_par[2],
                    omega = exp(opt_par[1]), alpha = exp(opt_par[2]), 
                    beta = exp(opt_par[3]), ite = opt$counts[[2]]))
}

pad <- function(data){
  media <- apply(data, 2, mean)
  desv <- apply(data, 2, sd)
  data_pad <- apply(data, 1, function(x) (x-media)/desv) %>% t() %>% as.data.frame()
  return(data_pad)
}

# M <- 100; n <- 1000 -----------------------------------------------------------------
set.seed(1)
M <- 100; n <- 1000

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'phi0')
histo(MC_pad, 'phi1')
histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'phi0') 
shapiro.test(MC_pad$phi0)
tseries::jarque.bera.test(MC_pad$phi0)

QQplot(MC_pad, 'phi1')
shapiro.test(MC_pad$phi1)
tseries::jarque.bera.test(MC_pad$phi1)

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)

# M <- 100; n <- 1500 -----------------------------------------------------------------
M <- 100; n <- 1500 
pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'phi0')
histo(MC_pad, 'phi1')
histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'phi0') 
shapiro.test(MC_pad$phi0)
tseries::jarque.bera.test(MC_pad$phi0)

QQplot(MC_pad, 'phi1')
shapiro.test(MC_pad$phi1)
tseries::jarque.bera.test(MC_pad$phi1)

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)
# M <- 100; n <- 2000 -----------------------------------------------------------------
M <- 100; n <- 2000 
pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'phi0')
histo(MC_pad, 'phi1')
histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'phi0') 
shapiro.test(MC_pad$phi0)
tseries::jarque.bera.test(MC_pad$phi0)

QQplot(MC_pad, 'phi1')
shapiro.test(MC_pad$phi1)
tseries::jarque.bera.test(MC_pad$phi1)

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)
# M <- 200; n <- 1000 -----------------------------------------------------------------
set.seed(1)
M <- 200; n <- 1000

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'phi0')
histo(MC_pad, 'phi1')
histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'phi0') 
shapiro.test(MC_pad$phi0)
tseries::jarque.bera.test(MC_pad$phi0)

QQplot(MC_pad, 'phi1')
shapiro.test(MC_pad$phi1)
tseries::jarque.bera.test(MC_pad$phi1)

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)

# M <- 200; n <- 1500 -----------------------------------------------------------------
M <- 200; n <- 1500 

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'phi0')
histo(MC_pad, 'phi1')
histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'phi0') 
shapiro.test(MC_pad$phi0)
tseries::jarque.bera.test(MC_pad$phi0)

QQplot(MC_pad, 'phi1')
shapiro.test(MC_pad$phi1)
tseries::jarque.bera.test(MC_pad$phi1)

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)
# M <- 200; n <- 2000 -----------------------------------------------------------------
M <- 200; n <- 2000 

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'phi0')
histo(MC_pad, 'phi1')
histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'phi0') 
shapiro.test(MC_pad$phi0)
tseries::jarque.bera.test(MC_pad$phi0)

QQplot(MC_pad, 'phi1')
shapiro.test(MC_pad$phi1)
tseries::jarque.bera.test(MC_pad$phi1)

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)
# M <- 500; n <- 1000 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1000

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'phi0')
histo(MC_pad, 'phi1')
histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'phi0') 
shapiro.test(MC_pad$phi0)
tseries::jarque.bera.test(MC_pad$phi0)

QQplot(MC_pad, 'phi1')
shapiro.test(MC_pad$phi1)
tseries::jarque.bera.test(MC_pad$phi1)

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)

# M <- 500; n <- 1500 -----------------------------------------------------------------
M <- 500; n <- 1500

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'phi0')
histo(MC_pad, 'phi1')
histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'phi0') 
shapiro.test(MC_pad$phi0)
tseries::jarque.bera.test(MC_pad$phi0)

QQplot(MC_pad, 'phi1')
shapiro.test(MC_pad$phi1)
tseries::jarque.bera.test(MC_pad$phi1)

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)
# M <- 500; n <- 2000 -----------------------------------------------------------------
M <- 500; n <- 2000

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'phi0')
histo(MC_pad, 'phi1')
histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'phi0') 
shapiro.test(MC_pad$phi0)
tseries::jarque.bera.test(MC_pad$phi0)

QQplot(MC_pad, 'phi1')
shapiro.test(MC_pad$phi1)
tseries::jarque.bera.test(MC_pad$phi1)

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)
