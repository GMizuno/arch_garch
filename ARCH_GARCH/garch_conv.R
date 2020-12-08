setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\ARCH_GARCH)")
source("garch_est.R")
source("garch11.R")
require(purrr)

# Monte Carlo -------------------------------------------------------------
gerando <- function(n, par, pars_init){
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

monte_carlo <- function(M, n, par, pars_init){
  
}

pad <- function(data){
  media <- apply(data, 2, mean)
  desv <- apply(data, 2, sd)
  data_pad <- apply(data, 1, function(x) (x-media)/desv) %>% t() %>% as.data.frame()
  return(data_pad)
}

# -----------------------------------------------------------------
M <- 500; n <- 2000; set.seed(1)
pars <- c(.5, .13, .86); n <- 1000
pars_init <- log(pars)

MC1 <- map_df(1:M, ~gerando(n, pars, pars_init))
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

hist(MC_pad$omega)
hist(MC_pad$alpha)
hist(MC_pad$beta)

qqnorm(MC_pad$omega)
qqline(MC_pad$omega)
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

qqnorm(MC_pad$alpha)
qqline(MC_pad$alpha)
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

qqnorm(MC_pad$beta)
qqline(MC_pad$beta)
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)

