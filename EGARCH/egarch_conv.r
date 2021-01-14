source("./EGARCH/egarch_sim.r")
source("./EGARCH/egarch_est.r")
source("ggplot_graficos.R")
temp <- './EGARCH/Graficos/Convergencia'
require(purrr)

# Monte Carlo ---------------------------------------------------------------
gerando <- function(n, par, pars_init){
  # Simula os dados
  rt <- egarch_sim(n, pars)
  
  # Faz a otimizacao
  opt <- optim(par = pars_init, fn = llike_egarch, method = "BFGS", 
               control = list(fnscale=-1), rt = rt$rt, n = n)
  
  # Guarda em um data frame
  return(data.frame(omega = opt$par[1], alpha = opt$par[2], beta = opt$par[3],
                    gamma = opt$par[4]))
}

pad <- function(data){
  media <- apply(data, 2, mean)
  desv <- apply(data, 2, sd)
  data_pad <- apply(data, 1, function(x) (x-media)/desv) %>%
    t() %>% as.data.frame()
  return(data_pad)
}

pars <- c(-.057, .216, .951, -.151)
pars_init <- c(-.05, .2, .95, -.15)

# M = 100 e n = 1000 -----------------------------------------------------------------
M <- 100; n <- 1000

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio

apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:4], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:4], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:4], ~shapiro.test(.x))
map(MC_pad[,1:4], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/egarch_MC_qq_', rep(1, 4),'_', names(MC_pad)[1:4],'.png')
nomehi <- paste0(temp, '/egarch_MC_hist_', rep(1, 4), '_', names(MC_pad)[1:4],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M = 200 e n = 1000 -----------------------------------------------------------------
M <- 200; n <- 1000

set.seed(1)
inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio

apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:4], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:4], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:4], ~shapiro.test(.x))
map(MC_pad[,1:4], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/egarch_MC_qq_', rep(2, 4),'_', names(MC_pad)[1:4],'.png')
nomehi <- paste0(temp, '/egarch_MC_hist_', rep(2, 4), '_', names(MC_pad)[1:4],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
# M = 500 e n = 1000 -----------------------------------------------------------------
M <- 500; n <- 1000
set.seed(1)
inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicioMC1 <- map_df(1:M, ~gerando(n, pars, pars_init))

apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:4], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:4], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:4], ~shapiro.test(.x))
map(MC_pad[,1:4], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/egarch_MC_qq_', rep(3, 4),'_', names(MC_pad)[1:4],'.png')
nomehi <- paste0(temp, '/egarch_MC_hist_', rep(3, 4), '_', names(MC_pad)[1:4],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M = 100 e n = 2000 -----------------------------------------------------------------
M <- 100; n <- 2000
set.seed(1)
inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio

apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:4], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:4], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:4], ~shapiro.test(.x))
map(MC_pad[,1:4], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/egarch_MC_qq_', rep(3, 4),'_', names(MC_pad)[1:4],'.png')
nomehi <- paste0(temp, '/egarch_MC_hist_', rep(3, 4), '_', names(MC_pad)[1:4],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M = 200 e n = 2000 -----------------------------------------------------------------
M <- 200; n <- 2000
set.seed(1)

inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio

apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:4], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:4], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:4], ~shapiro.test(.x))
map(MC_pad[,1:4], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/egarch_MC_qq_', rep(4, 4),'_', names(MC_pad)[1:4],'.png')
nomehi <- paste0(temp, '/egarch_MC_hist_', rep(4, 4), '_', names(MC_pad)[1:4],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M = 500 e n = 2000 -----------------------------------------------------------------
M <- 500; n <- 2000
set.seed(1)
inicio <- Sys.time()
MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
Sys.time() - inicio

apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:4], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:4], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:4], ~shapiro.test(.x))
map(MC_pad[,1:4], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/egarch_MC_qq_', rep(5, 4),'_', names(MC_pad)[1:4],'.png')
nomehi <- paste0(temp, '/egarch_MC_hist_', rep(5, 4), '_', names(MC_pad)[1:4],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))