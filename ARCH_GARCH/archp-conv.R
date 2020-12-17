setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\ARCH_GARCH)")
source("archp_est.R")
source("archp.R")
source("ggplot_graficos.R")
require(purrr)

# Monte Carlo -------------------------------------------------------------
gerando <- function(n, par, p, pars_init){
  # Simula os dados
  rt <- archp(n, pars)
  
  # Faz a otimizacao
  opt <- optim(par = pars_init, fn = llike_archp, method = "BFGS",
               control = list(fnscale=-1), rt = rt$rt, n = n, p = p)
  
  opt_par <- exp(opt$par)
  
  # Guarda em um data frame
  ite = opt$counts[[2]]
  data <- t(data.frame(opt_par))
  rownames(data) <- NULL
  colnames(data) <- c('Omega', paste0("alpha", 1:(length(opt_par)-1)))

  return(cbind(data, ite))
}


pad <- function(data){
  media <- apply(data, 2, mean)
  desv <- apply(data, 2, sd)
  data_pad <- apply(data, 1, function(x) (x-media)/desv) %>%
    t() %>% as.data.frame()
  return(data_pad)
}

# -----------------------------------------------------------------
M <- 5; n <- 2000; set.seed(1)
pars <- c(.5, .13, .86); n <- 1000; p <- 2
pars_init <- log(pars)

MC1 <- map(1:M, ~gerando(n, pars, p, pars_init))
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

histo(MC_pad, 'omega')
histo(MC_pad, 'alpha')
histo(MC_pad, 'beta')

QQplot(MC_pad, 'omega')
shapiro.test(MC_pad$omega)
tseries::jarque.bera.test(MC_pad$omega)

QQplot(MC_pad, 'alpha')
shapiro.test(MC_pad$alpha)
tseries::jarque.bera.test(MC_pad$alpha)

QQplot(MC_pad, 'beta')
shapiro.test(MC_pad$beta)
tseries::jarque.bera.test(MC_pad$beta)