# setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\ARCH_GARCH)")
source("archp_est.R")
source("archp.R")
temp <- './Graficos/Convergencia'
source("../ggplot_graficos.R")
require(purrr)

# Monte Carlo e funcoes uteis -------------------------------------------------------------
gerando <- function(n, par, p, pars_init){
  # Simula os dados
  rt <- archp(n, pars)
  
  # Faz a otimizacao
  opt <- optim(par = pars_init, fn = llike_archp_exp, method = "BFGS",
               control = list(fnscale=-1), rt = rt$rt, n = n, p = p)
  
  opt_par <- exp(opt$par)
  
  # Guarda em um data frame
  ite = opt$counts[[2]]
  data <- as.data.frame(t(matrix(opt_par)))
  names(data) <- c('Omega', paste0("alpha", 1:(length(opt_par)-1)))
  data <- cbind(data, ite)
  return(data)
}

pad <- function(data){
  media <- apply(data, 2, mean)
  desv <- apply(data, 2, sd)
  data_pad <- apply(data, 1, function(x) (x-media)/desv) %>%
    t() %>% as.data.frame()
  return(data_pad)
}

# M <- 500; n <- 1000; p <- 2 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1000
pars <- c(.5, .13, .86); n <- 1000; p <- 2
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, p, pars_init))
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
nomeqq <- paste0(temp, '/arch', p, '_MC_qq_', rep(1, p+1),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/arch', p, '_MC_hist_', rep(1, p+1), '_', names(MC_pad)[1:3],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 1000; p <- 3 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1000
pars <- c(1.5, .25, .45, .1); n <- 1000; p <- 3
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, p, pars_init))
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
nomeqq <- paste0(temp, '/arch', p, '_MC_qq_', rep(2, p+1),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/arch', p, '_MC_hist_', rep(2, p+1), '_', names(MC_pad)[1:3],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 1000; p <- 4 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1000
pars <- c(1., .1, .15, .1, .35); n <- 1000; p <- 4
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, p, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/arch', p, '_MC_qq_', rep(3, p+1),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/arch', p, '_MC_hist_', rep(3, p+1), '_', names(MC_pad)[1:3],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 1500; p <- 2 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1500
pars <- c(.5, .13, .86); n <- 1000; p <- 2
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, p, pars_init))
apply(MC1, 2, mean)
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/arch', p, '_MC_qq_', rep(4, p+1),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/arch', p, '_MC_hist_', rep(4, p+1), '_', names(MC_pad)[1:3],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 1500; p <- 3 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1500
pars <- c(1.5, .25, .45, .1); n <- 1000; p <- 3
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, p, pars_init))
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
nomeqq <- paste0(temp, '/arch', p, '_MC_qq_', rep(5, p+1),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/arch', p, '_MC_hist_', rep(5, p+1), '_', names(MC_pad)[1:3],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 1500; p <- 4 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1500
pars <- c(1., .1, .15, .1, .35); n <- 1000; p <- 4
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, p, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/arch', p, '_MC_qq_', rep(6, p+1),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/arch', p, '_MC_hist_', rep(6, p+1), '_', names(MC_pad)[1:3],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 2000; p <- 2 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 2000
pars <- c(.5, .13, .86); n <- 1000; p <- 2
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, p, pars_init))
apply(MC1, 2, mean)
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/arch', p, '_MC_qq_', rep(7, p+1),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/arch', p, '_MC_hist_', rep(7, p+1), '_', names(MC_pad)[1:3],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

# M <- 500; n <- 2000; p <- 3 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 2000
pars <- c(1.5, .25, .45, .1); n <- 1000; p <- 3
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, p, pars_init))
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
nomeqq <- paste0(temp, '/arch', p, '_MC_qq_', rep(8, p+1),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/arch', p, '_MC_hist_', rep(8, p+1), '_', names(MC_pad)[1:3],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))


# M <- 500; n <- 2000; p <- 4 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 2000
pars <- c(1., .1, .15, .1, .35); n <- 1000; p <- 4
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, p, pars_init))
apply(MC1, 2, mean)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:5], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:5], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:5], ~shapiro.test(.x))
map(MC_pad[,1:5], ~tseries::jarque.bera.test(.x))

# Salvando
nomeqq <- paste0(temp, '/arch', p, '_MC_qq_', rep(9, p+1),'_', names(MC_pad)[1:3],'.png')
nomehi <- paste0(temp, '/arch', p, '_MC_hist_', rep(9, p+1), '_', names(MC_pad)[1:3],'.png')
walk2(nomeqq, q1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
walk2(nomehi, h1, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
