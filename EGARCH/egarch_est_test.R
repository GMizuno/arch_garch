source("./EGARCH/egarch_est.r")
source("./EGARCH/egarch_sim.r")
require(forecast); require(ggplot2); require(numDeriv)

# Controle do otimizador
ctrl <- list(fnscale = -1, REPORT = 1, trace = 1000, save.failures=TRUE)

# Otimizando 1 --------------------------------------------------------------
pars <- c(-.052, .076, .987, -.102)
set.seed(1)
r1 <- egarch_sim(2000, pars)
n <- length(r1$rt)

plot(r1$rt, type = 'l')
acf(r1$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r1$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
acf(r1$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r1$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))

# Chute proximo
opt1 <- optim(par = c(.05, .07, .9, -.1), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt1$par
opt1$count

# Chute com 2 parametros proximo
opt2 <- optim(par = c(.05, .07, .6, .01), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt2$par
opt2$count

# Chute com 1 parametros proximo
opt3 <- optim(par = c(.05, .99, .01, .9), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt3$par
opt3$count

# Chute com parametros longe 
opt4 <- optim(par = c(.9, .99, .1, 1), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt4$par
opt4$count

# Chute "padrao", retorna erro se eu passo um vetor de 0 como chute inicial
opt5 <- optim(par = rep(0, 4), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt5$par
opt5$count

# Chute "padrao", retorna erro se eu passo um vetor de 0 como chute inicial
opt6 <- optim(par = c(2, 2 , 2, 2, 2), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt6$par
opt6$count

# Chute "padrao", retorna erro se eu passo um vetor de 0.1 como chute inicial
opt6 <- optim(par = rep(.1, 4), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt6$par
opt6$count

# Chute 'exato'
opt7 <- optim(par = pars, llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt7$par
opt7$count

# Chute ruim
opt8 <- optim(par = c(1, 1, -.1, 1), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt8$par
opt8$count

# Estimativas 1 --------------------------------------------------------------
est1 <- rbind(pars, opt1$par, opt2$par, opt3$par, opt4$par, opt5$par, opt6$par,
             opt7$par)
row.names(est1) <- c('Verdadeiro', "opt1", "opt2", "opt3", "opt4", "opt5", 
                    "opt6", "opt7")
colnames(est1) <- c("omega", 'alpha', 'beta', 'gamma')
est1

it_f1 <- rbind(opt1$counts[1], opt2$counts[1], opt3$counts[1], opt4$counts[1], 
              opt5$counts[1], opt6$counts[1], opt7$counts[1])
it_g1 <- rbind(opt1$counts[2], opt2$counts[2], opt3$counts[2], opt4$counts[2], 
              opt5$counts[2], opt6$counts[2], opt7$counts[2])
it1 <- cbind(it_f1, it_g1)
row.names(it1) <- c("opt1", "opt2", "opt3", "opt4", "opt5",
                   "opt6", "opt7")
it1

# Otimizando 2 --------------------------------------------------------------
pars <- c(-.057, .216, .951, -.151)
set.seed(1)
r1 <- egarch_sim(2000, pars)
n <- length(r1$rt)

plot(r1$rt, type = 'l')
acf(r1$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r1$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
acf(r1$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r1$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))

# Chute proximo
opt1 <- optim(par = c(.05, .2, .9, -.1), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n) 
pars
opt1$par
opt1$count

# Chute com 2 parametros proximo
opt2 <- optim(par = c(.5, .2, .1, .1), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt2$par
opt2$count

# Chute com 1 parametros proximo
opt3 <- optim(par = c(.5, .8, .1, .1), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt3$par
opt3$count

# Chute com parametros longe 
opt4 <- optim(par = c(1, .9, 0.001, 1), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt4$par
opt4$count

# Chute "padrao", retorna erro se eu passo um vetor de 0.1 como chute inicial
opt5 <- optim(par = rep(.1, 4), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n) 
pars
opt5$par
opt5$count

# Chute "padrao", retorna erro se eu passo um vetor de 0 como chute inicial
opt6 <- optim(par = rep(0, 4), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt6$par
opt6$count

# Chute 'exato'
opt7 <- optim(par = pars, llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt7$par
opt7$count

# Estimativas 2 --------------------------------------------------------------
est2 <- rbind(pars, opt1$par, opt2$par, opt3$par, opt4$par, opt5$par, 
             opt6$par, opt7$par)
row.names(est2) <- c('Verdadeiro', "opt1", "opt2", "opt3", "opt4", "opt5",
                    "opt6", "opt7")
colnames(est2) <- c("omega", 'alpha', 'beta', 'gamma')
est2

it_f2 <- rbind(opt1$counts[1], opt2$counts[1], opt3$counts[1], opt4$counts[1], 
            opt5$counts[1], opt6$counts[1], opt7$counts[1])
it_g2 <- rbind(opt1$counts[2], opt2$counts[2], opt3$counts[2], opt4$counts[2], 
              opt5$counts[2], opt6$counts[2], opt7$counts[2])
it2 <- cbind(it_f2, it_g2)
row.names(it2) <- c("opt1", "opt2", "opt3", "opt4", "opt5",
                    "opt6", "opt7")
it2

# Otimizando 3 exemplo do Eviews --------------------------------------------------------------
pars <- c(-.129, .062, .991, -.025)
set.seed(4)
r1 <- egarch_sim(2000, pars)
n <- length(r1$rt)

plot(r1$rt, type = 'l')
acf(r1$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r1$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
acf(r1$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r1$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))

# Chute proximo
opt1 <- optim(par = c(.1, .05, .9, -.02), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt1$par
opt1$count

# Chute com 2 parametros proximo
opt2 <- optim(par = c(.1, .01, .01, .9), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt2$par
opt2$count

# Chute com 1 parametros proximo
opt3 <- optim(par = c(.01, .9, .01, .9), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt3$par
opt3$count

# Chute com parametros longe 
opt4 <- optim(par = c(.9, .9, .9, 1), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt4$par
opt4$count

# Chute "padrao", retorna erro se eu passo um vetor de 0 como chute inicial
opt5 <- optim(par = rep(.1, 4), llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt5$par
opt5$count

# Chute 'exato'
opt6 <- optim(par = pars, llike_egarch, 
              method = "BFGS", control = ctrl, rt = r1$rt, n = n)
pars
opt6$par
opt6$count

# Estimativas 3 --------------------------------------------------------------
est3 <- rbind(pars, opt1$par, opt2$par, opt3$par, opt4$par, opt5$par, opt6$par)
row.names(est3) <- c('Verdadeiro', "opt1", "opt2", "opt3", "opt4", "opt5", "opt6")
colnames(est3) <- c("omega", 'alpha', 'beta', 'gamma')
est3

it_f3 <- rbind(opt1$counts[1], opt2$counts[1], opt3$counts[1], opt4$counts[1], 
              opt5$counts[1], opt6$counts[1])
it_g3 <- rbind(opt1$counts[2], opt2$counts[2], opt3$counts[2], opt4$counts[2], 
              opt5$counts[2], opt6$counts[2])
it3 <- cbind(it_f3, it_g3)
row.names(it3) <- c("opt1", "opt2", "opt3", "opt4", "opt5",
                   "opt6", "opt7")
it3

# Limpando ----------------------------------------------------------------
rm(list = c(paste("opt",c(1:8),sep="")))
rm(list = c(paste("it_f",c(1:3),sep="")))
rm(list = c(paste("it_g",c(1:3),sep="")))
