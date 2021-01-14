source("./ARCH_GARCH/archp.R")
source("./ARCH_GARCH/archp_est.R")
set.seed(1); require(ggplot2)
ctrl <- list(fnscale = -1)

# Exemplo 1 ---------------------------------------------------------------
pars <- c(0.64214, 0.56935); n <- 1000; p <- 1

pars_init <- log(rep(.1, p+1))
r1 <- archp(n, pars)

plot(r1$rt, type = 'l')
acf(r1$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r1$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
acf(r1$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r1$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
llike_archp(r1$rt, pars, p, n)

opt1 <- optim(par = pars_init, llike_archp, method = "BFGS", 
              control = ctrl, rt = r1$rt, p = p, n = n)
exp(opt1$par)
pars
opt1$counts

# Exemplo 2 ---------------------------------------------------------------
pars <- c(1, .1, .25); n <- 1000; p <- 2

pars_init <- log(rep(.1, p+1))
r2 <- archp(n, pars)
llike_archp(r2$rt, pars_init, p, n)

plot(r2$rt, type = 'l')
acf(r2$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r2$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
acf(r2$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r2$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))

opt2 <- optim(par = pars_init, llike_archp, method = "BFGS", 
              control = ctrl, rt = r2$rt, p = p, n = n)
exp(opt2$par)
pars
opt2$count

# Exemplo 3 ---------------------------------------------------------------
pars <- c(1.5, .1, .15, .1); n <- 1000; p <- 3

pars_init <- log(rep(.1, p+1))
r3 <- archp(n, pars)

plot(r3$rt, type = 'l')
acf(r3$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r3$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
acf(r3$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r3$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))

opt3 <- optim(par = pars_init, llike_archp, method = "BFGS", 
              control = ctrl, rt = r3$rt, p = p, n = n)
exp(opt3$par)
pars
opt3$counts

# Exemplo 4 ---------------------------------------------------------------
pars <- c(1., .1, .15, .1, .35); n <- 1000; p <- 4

pars_init <- log(rep(.1, p+1))
r4 <- archp(n, pars)

plot(r4$rt, type = 'l')
acf(r4$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r4$rt, plot = F) %>% autoplot() + ylim(c(-1,1))
acf(r4$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(r4$rt^2, plot = F) %>% autoplot() + ylim(c(-1,1))

opt4 <- optim(par = pars_init, llike_archp, method = "BFGS", 
              control = ctrl, rt = r4$rt, p = p, n = n)
exp(opt4$par)
pars
opt4$counts

