setwd("C:/Users/Gabriel/Desktop/arch_garch/EGARCH")
source("egarch_sim.r")
source("egarch_est.r")
source("ggplot_graficos.R")
require(ggplot2); require(dplyr)

# Monte Carlo ---------------------------------------------------------------
gerando <- function(n, par, pars_init){
  # Simula os dados
  rt <- egarch_sim(n, pars)
  
  # Faz a otimizacao
  opt <- optim(par = pars_init, fn = llike_egarch, 
               method = "BFGS", control = list(fnscale=-1), rt = rt$rt, n = n)
  
  # Guarda em um data frame
  return(data.frame(omega = opt$par[1], alpha = opt$par[2], beta = opt$par[3],
                    gamma = opt$par[4]))
}

pars <- c(-.057, .216, .951, -.151)
pars_init <- c(-.05, .2, .95, -.15)

# M = 100 e n= 1000 -----------------------------------------------------------------
M <- 100; n <- 1000

inicio <- Sys.time()
MC1 <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
fim <- Sys.time()

fim - inicio
data <- tibble(omega = unlist(MC1[,1]), alpha = unlist(MC1[,2]),
               beta = unlist(MC1[,3]), gamma = unlist(MC1[,4]))

p1 <- line_gamma(data, pars); p1
p2 <- histo(data, pars); p2

gamma_pad <- (data$gamma-mean(data$gamma))/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)

# M = 200 e n= 1000 -----------------------------------------------------------------
M <- 200; n <- 1000
set.seed(1)

start_time <- Sys.time()
MC2 <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(MC2[,1]), alpha = unlist(MC2[,2]),
               beta = unlist(MC2[,3]), gamma = unlist(MC2[,4]))

p3 <- line_gamma(data, pars); p1
p4 <- histo(data, pars); p2

gamma_pad <- (data$gamma-pars[4])/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)

# M = 500 e n= 1000 -----------------------------------------------------------------
M <- 500; n <- 1000
set.seed(1)

MC3 <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(MC3[,1]), alpha = unlist(MC3[,2]),
               beta = unlist(MC3[,3]), gamma = unlist(MC3[,4]))

p5 <- line_gamma(data, pars); p5
p6 <- histo(data, pars); p6

gamma_pad <- (data$gamma-pars[4])/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)

# M = 100 e n= 2000 -----------------------------------------------------------------
M <- 100; n <- 2000
set.seed(1)

MC4 <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(MC4[,1]), alpha = unlist(MC4[,2]),
               beta = unlist(MC4[,3]), gamma = unlist(MC4[,4]))

p7 <- line_gamma(data, pars); p7
p8 <- histo(data, pars); p8

gamma_pad <- (data$gamma-pars[4])/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)

# M = 200 e n= 2000 -----------------------------------------------------------------
M <- 200; n <- 2000
set.seed(1)

MC5 <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(MC5[,1]), alpha = unlist(MC5[,2]),
               beta = unlist(MC5[,3]), gamma = unlist(MC5[,4]))
p9 <- line_gamma(data, pars); p9
p10 <- histo(data, pars); p10

gamma_pad <- (data$gamma-pars[4])/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)

# M = 500 e n= 2000 -----------------------------------------------------------------
M <- 500; n <- 2000
set.seed(1)

start_time <- Sys.time()
MC6 <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(MC6[,1]), alpha = unlist(MC6[,2]),
               beta = unlist(MC6[,3]), gamma = unlist(MC6[,4]))

p11 <- line_gamma(data, pars); p11
p12 <- histo(data, pars); p12

gamma_pad <- (data$gamma-pars[4])/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)


# Salvando ----------------------------------------------------------------

graficos <- list(p1,p2,p3, p4,p5,p6, 
                 p7,p8,p9, p10,p11,p12)
path <- "C:/Users/Gabriel/Desktop/arch_garch/EGARCH/Graficos"
nomes <- paste0('Grafico_convergencia', 1:12, '.png')
walk2(nomes, graficos, ~ggsave(filename = .x, plot = .y,
                               width = 9.7, height = 4, path = path))
