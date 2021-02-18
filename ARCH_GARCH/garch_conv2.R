# setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\ARCH_GARCH)")
temp <- './ARCH_GARCH/Graficos/Convergencia'
source("./ARCH_GARCH/garch_est.R")
source("./ARCH_GARCH/garch11.R")
source("ggplot_graficos.R")
require(purrr)
require(dplyr)

# Monte Carlo e funcoes -------------------------------------------------------------
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

# Tirando as estimativas ruins, sem while -------------------------------------------
tirando <- function(n, M, pars, pars_init){
  map_df(1:M, ~gerando(n, pars, pars_init)) %>% 
    filter(ite < 20, alpha + beta < 1, alpha < .5, beta > .5, omega < .2) %>% 
    return()
}

pars <- c(.05, .13, .86); pars_init <- log(pars)
T1 <- tirando(1000, 500, pars, pars_init); T1 %>% nrow()
T2 <- tirando(2000, 500, pars, pars_init); T2 %>% nrow() 
T3 <- tirando(3000, 500, pars, pars_init); T3 %>% nrow() 
T4 <- tirando(4000, 500, pars, pars_init); T4 %>% nrow() 
T5 <- tirando(5000, 500, pars, pars_init); T5 %>% nrow() 

# Tirando as estimativas ruins, com while -------------------------------------------
tirando2 <- function(M, n, pars, pars_init){
  data <- data.frame(omega = rep(NA, M), alpha = rep(NA, M), 
                    beta = rep(NA, M), ite = rep(NA, M))
  cont <- 0
  
  while (cont < M){
    
    est <- gerando(n, pars, pars_init)
    if (est$ite < 20 & est$alpha + est$beta < 1 &
     est$alpha < .5 & est$beta > .5 & est$omega > 10 & est$omega < 40){
       cont <- cont + 1 
       print(cont)
       data[cont,] <- est
    }
  }
  return(data)
}   

pars <- c(20, .13, .86); pars_init <- log(pars)
# M <- 500; n <- 1000 -----------------------------------------------------------------
set.seed(1)
M <- 500; n <- 1000
MC1 <- tirando2(M, n, pars, pars_init); 

apply(MC1, 2, mean)
apply(MC1, 2, sd)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# M <- 500; n <- 1500 -----------------------------------------------------------------
set.seed(2000)
M <- 500; n <- 1500
MC1 <- tirando2(M, n, pars, pars_init); 

apply(MC1, 2, mean)
apply(MC1, 2, sd)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# M <- 500; n <- 2000 -----------------------------------------------------------------
set.seed(3)
M <- 500; n <- 2000
MC1 <- tirando2(M, n, pars, pars_init); 

apply(MC1, 2, mean)
apply(MC1, 2, sd)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n)); q1
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n)); h1

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# M <- 500; n <- 2500 -----------------------------------------------------------------
set.seed(500)
M <- 500; n <- 2500
MC1 <- tirando2(M, n, pars, pars_init); 

apply(MC1, 2, mean)
apply(MC1, 2, sd)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# M <- 500; n <- 3000 -----------------------------------------------------------------
set.seed(666)
M <- 500; n <- 3000
MC1 <- tirando2(M, n, pars, pars_init); 

apply(MC1, 2, mean)
apply(MC1, 2, sd)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))

# M <- 500; n <- 3500 -----------------------------------------------------------------
set.seed(777)
M <- 500; n <- 3500
MC1 <- tirando2(M, n, pars, pars_init); 

apply(MC1, 2, mean)
apply(MC1, 2, sd)
pars
MC_pad <- pad(MC1)

apply(MC_pad, 2, mean)
apply(MC_pad, 2, sd)

q1 <- map(names(MC_pad)[1:3], ~QQplot(MC_pad, .x, M, n))
h1 <- map(names(MC_pad)[1:3], ~histo(MC_pad, .x, M, n))

map(MC_pad[,1:3], ~shapiro.test(.x))
map(MC_pad[,1:3], ~tseries::jarque.bera.test(.x))
