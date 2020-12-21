setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\ARCH_GARCH)")
temp <- 'C:/Users/Gabriel/Desktop/arch_garch/ARCH_GARCH/Graficos/Convergencia/Comparacao'
source("garch_est.R")
source("garch11.R")
source("ggplot_graficos.R")
require(purrr); require(rugarch); require(dplyr)

# Monte Carlo e funcoes auxiliares -------------------------------------------------------------
gerando <- function(n, par, pars_init){
  rt <- garch11(n, pars)
  
  opt <- optim(par = pars_init, fn = llike_garch_exp, method = "BFGS",
               control = list(fnscale=-1), rt = rt$rt, n = n)
  
  spec <- ugarchspec(variance.model = list(model = 'sGARCH',
                                          garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = rep(0,2), include.mean = FALSE),
                     distribution.model = 'norm')
  
  opt_ugarch <- ugarchfit(spec = spec, data = rt$rt) %>% coef()
  opt_par <- exp(opt$par)

  return(data.frame(omega = opt_par[1], alpha = opt_par[2], 
                    beta = opt_par[3], omega1 = opt_ugarch[1],
                    alpha1 = opt_ugarch[2], beta1 = opt_ugarch[3]))
}

pad <- function(data){
  media <- apply(data, 2, mean)
  desv <- apply(data, 2, sd)
  data_pad <- apply(data, 1, function(x) (x-media)/desv) %>% t() %>% as.data.frame()
  return(data_pad)
}

erro <- function(data)
{
  erro <- data %>% transmute(omega_erro =  abs(omega - omega1),
                             alpha_erro =  abs(alpha - alpha1),
                             beta_erro =  abs(beta - beta1))
  return(erro)
}


# M <- 100; n <- 1000 -----------------------------------------------------
pars <- c(.5, .13, .86); pars_init <- log(pars)
set.seed(1)
M <- 100; n <- 1000

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)
erro(MC1) %>% apply(2, mean)

q1 <- map(names(MC_pad)[1:6], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:6], ~histo(MC_pad, .x, M, n))
nomeqq <- paste0(temp, '/gach_MC_qq_', rep(1, 6),'_', names(MC_pad)[1:6],'.png')
nomehi <- paste0(temp, '/gach_MC_hist_', rep(1, 6), '_', names(MC_pad)[1:6],'.png')
walk2(nomehi, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 100; n <- 2000 -----------------------------------------------------
pars <- c(.5, .13, .86); pars_init <- log(pars)
set.seed(1)
M <- 100; n <- 1000

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

q1 <- map(names(MC_pad)[1:6], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:6], ~histo(MC_pad, .x, M, n))
nomeqq <- paste0(temp, '/gach_MC_qq_', rep(2, 6),'_', names(MC_pad)[1:6],'.png')
nomehi <- paste0(temp, '/gach_MC_hist_', rep(2, 6), '_', names(MC_pad)[1:6],'.png')
walk2(nomehi, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 200; n <- 1000 -----------------------------------------------------
pars <- c(.5, .13, .86); pars_init <- log(pars)
set.seed(1)
M <- 100; n <- 1000

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

q1 <- map(names(MC_pad)[1:6], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:6], ~histo(MC_pad, .x, M, n))
nomeqq <- paste0(temp, '/gach_MC_qq_', rep(2, 6),'_', names(MC_pad)[1:6],'.png')
nomehi <- paste0(temp, '/gach_MC_hist_', rep(2, 6), '_', names(MC_pad)[1:6],'.png')
walk2(nomehi, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 200; n <- 2000 -----------------------------------------------------
pars <- c(.5, .13, .86); pars_init <- log(pars)
set.seed(1)
M <- 100; n <- 1000

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

q1 <- map(names(MC_pad)[1:6], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:6], ~histo(MC_pad, .x, M, n))
nomeqq <- paste0(temp, '/gach_MC_qq_', rep(2, 6),'_', names(MC_pad)[1:6],'.png')
nomehi <- paste0(temp, '/gach_MC_hist_', rep(2, 6), '_', names(MC_pad)[1:6],'.png')
walk2(nomehi, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))