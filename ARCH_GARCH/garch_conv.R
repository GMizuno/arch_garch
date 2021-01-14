setwd('..')
temp <- './ARCH_GARCH/Graficos/Convergencia'
source("./ARCH_GARCH/garch_est.R")
source("./ARCH_GARCH/garch11.R")
source("ggplot_graficos.R")
require(purrr)

# Monte Carlo -------------------------------------------------------------
gerando <- function(n, pars, pars_init){
  # Simula os dados
  rt <- garch11(n, pars)
  
  # Faz a otimizacao
  opt <- optim(par = pars_init, fn = llike_garch_exp, method = "BFGS",
               control = list(fnscale=-1), rt = rt$rt, n = n)
  
  opt_par <- exp(opt$par)
  # Guarda em um data frame
  return(data.frame(omega = opt_par[1], alpha = opt_par[2], 
                    beta = opt_par[3], ite = opt$counts[[2]]))
}

pad <- function(data){
  media <- apply(data, 2, mean)
  desv <- apply(data, 2, sd)
  data_pad <- apply(data, 1, function(x) (x - media)/desv) %>% t() %>%
   as.data.frame()
  return(data_pad)
}

pars <- c(.05, .13, .86); pars_init <- log(pars)

# M <- 500; n <- 1500 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1000

pars <- c(.5, .13, .86); pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/garch_MC_qq_', rep(1, 3),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/garch_MC_hist_', rep(1, 3), '_', names(MC_pad)[1:3],'.png')

walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 1500 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1500

pars <- c(.5, .13, .86); pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/garch_MC_qq_', rep(2, 3),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/garch_MC_hist_', rep(2, 3), '_', names(MC_pad)[1:3],'.png')

walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
# M <- 500; n <- 2000 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 2000

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/garch_MC_qq_', rep(3, 3),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/garch_MC_hist_', rep(3, 3), '_', names(MC_pad)[1:3],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
# M <- 500; n <- 2500 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 2500

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/garch_MC_qq_', rep(4, 3),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/garch_MC_hist_', rep(4, 3), '_', names(MC_pad)[1:3],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 3000 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 3000

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/garch_MC_qq_', rep(5, 3),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/garch_MC_hist_', rep(5, 3), '_', names(MC_pad)[1:3],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
# M <- 500; n <- 3500 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 3500

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/garch_MC_qq_', rep(6, 3),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/garch_MC_hist_', rep(6, 3), '_', names(MC_pad)[1:3],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
# M <- 500; n <- 4000 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 4000

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/garch_MC_qq_', rep(7, 3),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/garch_MC_hist_', rep(7, 3), '_', names(MC_pad)[1:3],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
# M <- 500; n <- 4500 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 5000

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

# q1[[2]] + ylim(-4,4) + xlim(-4,4)
# MC1[order(MC1$ite), ]
# QQplot(pad(log(MC1)), 'beta', M, n)
q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/garch_MC_qq_', rep(8, 3),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/garch_MC_hist_', rep(8, 3), '_', names(MC_pad)[1:3],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 5000 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 5000

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

# q1[[2]] + ylim(-4,4) + xlim(-4,4)
# MC1[order(MC1$ite), ]
# QQplot(pad(log(MC1)), 'beta', M, n)
q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando 
nomeqq <- paste0(temp, '/garch_MC_qq_', rep(9, 3),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/garch_MC_hist_', rep(9, 3), '_', names(MC_pad)[1:3],'.png')
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))











