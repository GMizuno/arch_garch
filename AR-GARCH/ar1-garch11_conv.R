temp <- 'C:/Users/Gabriel/Desktop/arch_garch/AR-GARCH/Graficos/Convergencia'
source('./AR-GARCH/ar1-garch11_sim.R')
source('./AR-GARCH/ar1-garch11_est.R')
source('ggplot_graficos.R')
require(purrr)

inicio1 <- Sys.time()
# Monte Carlo -------------------------------------------------------------
gerando <- function(n, par, pars_init){
  # Simula os dados
  rt <- AR1_Garch11(n, pars)
  
  # Faz a otimizacao
  opt <- optim(par = pars_init, fn = llike_ar_garch_exp, method = "BFGS",
               control = list(fnscale=-1), rt = rt$rt, n = n)
  
  opt_par <- opt$par
  # Guarda em um data frame
  return(data.frame(phi0 = opt_par[1],phi1 = opt_par[2],
                    omega = exp(opt_par[3]), alpha = exp(opt_par[4]), 
                    beta = exp(opt_par[5]), ite = opt$counts[[2]]))
}

pad <- function(data){
  media <- apply(data, 2, mean)
  desv <- apply(data, 2, sd)
  data_pad <- apply(data, 1, function(x) (x-media)/desv) %>% t() %>% as.data.frame()
  return(data_pad)
}

# Config iniciais
pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(c(.324, .124, .777)))

# M <- 100; n <- 1000 -----------------------------------------------------------------
set.seed(1)
M <- 100; n <- 1000

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio
MC_pad <- pad(MC1)

apply(MC1, 2, mean)
pars
apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))
map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/ar_garch_MC_qq_', rep(1, 5),'_', names(MC_pad)[1:5],'.png')
nomehi <- paste0(temp, '/ar_garch_MC_hist_', rep(1, 5), '_', names(MC_pad)[1:5],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 100; n <- 1500 -----------------------------------------------------------------
set.seed(2)
M <- 100; n <- 1500 

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)
Sys.time() - inicio

apply(MC1, 2, mean)
pars
apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))
map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/ar_garch_MC_qq_', rep(2, 5),'_', names(MC_pad)[1:5],'.png')
nomehi <- paste0(temp, '/ar_garch_MC_hist_', rep(2, 5), '_', names(MC_pad)[1:5],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 100; n <- 2000 -----------------------------------------------------------------
set.seed(3)
M <- 100; n <- 2000 

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio
MC_pad <- pad(MC1)

apply(MC1, 2, mean)
pars
apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))
map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/ar_garch_MC_qq_', rep(3, 5),'_', names(MC_pad)[1:5],'.png')
nomehi <- paste0(temp, '/ar_garch_MC_hist_', rep(3, 5), '_', names(MC_pad)[1:5],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 200; n <- 1000 -----------------------------------------------------------------
set.seed(4)
M <- 200; n <- 1000

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio
MC_pad <- pad(MC1)

apply(MC1, 2, mean)
pars
apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))
map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/ar_garch_MC_qq_', rep(4, 5),'_', names(MC_pad)[1:5],'.png')
nomehi <- paste0(temp, '/ar_garch_MC_hist_', rep(4, 5), '_', names(MC_pad)[1:5],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 200; n <- 1500 -----------------------------------------------------------------
set.seed(5)
M <- 200; n <- 1500

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio
MC_pad <- pad(MC1)

apply(MC1, 2, mean)
apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))
map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/ar_garch_MC_qq_', rep(5, 5),'_', names(MC_pad)[1:5],'.png')
nomehi <- paste0(temp, '/ar_garch_MC_hist_', rep(5, 5), '_', names(MC_pad)[1:5],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 200; n <- 2000 -----------------------------------------------------------------
set.seed(6)
M <- 200; n <- 2000

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio
MC_pad <- pad(MC1)

apply(MC1, 2, mean)
apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))
map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/ar_garch_MC_qq_', rep(6, 5),'_', names(MC_pad)[1:5],'.png')
nomehi <- paste0(temp, '/ar_garch_MC_hist_', rep(6, 5), '_', names(MC_pad)[1:5],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

Sys.time() - inicio1
# M <- 500; n <- 1000 -----------------------------------------------------------------
set.seed(4)
M <- 500; n <- 1000

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio
MC_pad <- pad(MC1)

apply(MC1, 2, mean)
pars
apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))
map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/ar_garch_MC_qq_', rep(7, 5),'_', names(MC_pad)[1:5],'.png')
nomehi <- paste0(temp, '/ar_garch_MC_hist_', rep(7, 5), '_', names(MC_pad)[1:5],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 1500 -----------------------------------------------------------------
set.seed(5)
M <- 500; n <- 1500

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio
MC_pad <- pad(MC1)

apply(MC1, 2, mean)
apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))
map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/ar_garch_MC_qq_', rep(8, 5),'_', names(MC_pad)[1:5],'.png')
nomehi <- paste0(temp, '/ar_garch_MC_hist_', rep(8, 5), '_', names(MC_pad)[1:5],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 2000 -----------------------------------------------------------------
set.seed(6)
M <- 500; n <- 2000

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio
MC_pad <- pad(MC1)

apply(MC1, 2, mean)
apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))
map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/ar_garch_MC_qq_', rep(9, 5),'_', names(MC_pad)[1:5],'.png')
nomehi <- paste0(temp, '/ar_garch_MC_hist_', rep(9, 5), '_', names(MC_pad)[1:5],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))


Sys.time() - inicio1