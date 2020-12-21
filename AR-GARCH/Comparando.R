setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\AR-GARCH)")
temp <- 'C:/Users/Gabriel/Desktop/arch_garch/AR-GARCH/Graficos/Convergencia/Comparando'
source('ar1-garch11_sim.R')
source('ar1-garch11_est.R')
source('C:/Users/Gabriel/Desktop/arch_garch/ggplot_graficos.R')
require(purrr); require(rugarch); require(dplyr)

# Monte Carlo e funcoes auxiliares -------------------------------------------------------------
gerando <- function(n, par, pars_init){
  # Simula os dados
  rt <- AR1_Garch11(n, pars)
  
  # Faz a otimizacao
  opt <- optim(par = pars_init, fn = llike_ar_garch_exp, method = "BFGS",
               control = list(fnscale=-1), rt = rt$rt, n = n)
  
  spec <- ugarchspec(variance.model = list(model = 'sGARCH',
                                           garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(1,0)),
                     distribution.model = 'norm')
  
  opt_ugarch <- ugarchfit(spec = spec, data = rt$rt) %>% coef()
  
  opt_par <- opt$par
  # Guarda em um data frame
  return(data.frame(phi0 = opt_par[1],phi1 = opt_par[2],
                    omega = exp(opt_par[3]), alpha = exp(opt_par[4]), 
                    beta = exp(opt_par[5]), phi0_u = opt_ugarch[1],
                    phi1_u = opt_ugarch[2], omega1 = opt_ugarch[3],
                    alpha1 = opt_ugarch[2], beta1 = opt_ugarch[3]))
}

pad <- function(data){
  media <- apply(data, 2, mean)
  desv <- apply(data, 2, sd)
  data_pad <- apply(data, 1, function(x) (x-media)/desv) %>% t() %>% as.data.frame()
  return(data_pad)
}

erro <- function(data, pars)
{
  erro <- data %>% transmute(phi0_erro = abs(phi0 - pars[1]),
                     phi1_erro =  abs(phi1 - pars[2]),
                     omega_erro =  abs(omega - pars[3]),
                     alpha_erro =  abs(alpha - pars[4]),
                     beta_erro =  abs(beta - pars[5]))
  return(erro)
}

# M <- 100; n <- 1000 -----------------------------------------------------------------
set.seed(1)
M <- 100; n <- 1000

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)
erro(MC1)

q1 <- map(names(MC_pad)[1:10], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:10], ~histo(MC_pad, .x, M, n))
nomeqq <- paste0(temp, '/gach_MC_qq_', rep(1, 10),'_', names(MC_pad)[1:10],'.png')
nomehi <- paste0(temp, '/gach_MC_hist_', rep(1, 10), '_', names(MC_pad)[1:10],'.png')
walk2(nomehi, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 100; n <- 2000 -----------------------------------------------------------------
set.seed(1)
M <- 100; n <- 1000

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

q1 <- map(names(MC_pad)[1:10], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:10], ~histo(MC_pad, .x, M, n))
nomeqq <- paste0(temp, '/gach_MC_qq_', rep(2, 10),'_', names(MC_pad)[1:10],'.png')
nomehi <- paste0(temp, '/gach_MC_hist_', rep(2, 10), '_', names(MC_pad)[1:10],'.png')
walk2(nomehi, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 200; n <- 1000 -----------------------------------------------------------------
set.seed(1)
M <- 100; n <- 1000

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

q1 <- map(names(MC_pad)[1:10], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:10], ~histo(MC_pad, .x, M, n))
nomeqq <- paste0(temp, '/gach_MC_qq_', rep(3, 10),'_', names(MC_pad)[1:10],'.png')
nomehi <- paste0(temp, '/gach_MC_hist_', rep(3, 10), '_', names(MC_pad)[1:10],'.png')
walk2(nomehi, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 200; n <- 2000 -----------------------------------------------------------------
set.seed(1)
M <- 100; n <- 1000

pars <- c(-.05, 0.226, .324, .124, .777)
pars_init <- c(-.05, 0.23, log(rep(.1, 3)))

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

q1 <- map(names(MC_pad)[1:10], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:10], ~histo(MC_pad, .x, M, n))
nomeqq <- paste0(temp, '/gach_MC_qq_', rep(4, 10),'_', names(MC_pad)[1:10],'.png')
nomehi <- paste0(temp, '/gach_MC_hist_', rep(4, 10), '_', names(MC_pad)[1:10],'.png')
walk2(nomehi, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))