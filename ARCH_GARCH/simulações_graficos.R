source("archp.R")
source("garch11.R")
source("ggplot_graficos.R")
require(purrr)
require(ggplot2); require(magrittr)
set.seed(1)
n <- 1000

#################### ARCH(1) ##################################### 

######## EXEMPLO 1 #################### 
pars <- c(.5, .1)
r1 <- archp(n, pars)
line(r1, "ARCH(1)")
acf_plot(r1$rt, "ARCH(1)")
pacf_plot(r1$rt, "ARCH(1)")
acf_plot(r1$rt2, "ARCH(1)")
pacf_plot(r1$rt2, "ARCH(1)")

######## EXEMPLO 2 #################### 
pars <- c(.5, .25)
r2 <- archp(n, pars)
line(r2, "ARCH(1)")
acf_plot(r2$rt, "ARCH(1)")
pacf_plot(r2$rt, "ARCH(1)")
acf_plot(r2$rt2, "ARCH(1)")
pacf_plot(r2$rt2, "ARCH(1)")

######## EXEMPLO 3 #################### 
pars <- c(.5, .8)
r3 <- archp(n, pars)
acf_plot(r3$rt, "ARCH(1)")
pacf_plot(r3$rt, "ARCH(1)")
acf_plot(r3$rt2, "ARCH(1)")
pacf_plot(r3$rt2, "ARCH(1)")

#################### ARCH(2) ##################################### 

######## EXEMPLO 4 #################### 
pars <- c(1, .1, .25)
r4 <- archp(n, pars)
acf_plot(r4$rt, "ARCH(2)")
pacf_plot(r4$rt, "ARCH(2)")
acf_plot(r4$rt2, "ARCH(2)")
pacf_plot(r4$rt2, "ARCH(2)")

######## EXEMPLO 5 #################### 
pars <- c(1, .2, .4)
r5 <- archp(n, pars)
acf_plot(r5$rt, "ARCH(2)")
pacf_plot(r5$rt, "ARCH(2)")
acf_plot(r5$rt2, "ARCH(2)")
pacf_plot(r5$rt2, "ARCH(2)")

######## EXEMPLO 6 #################### 
pars <- c(1, .05, .8)
r6 <- archp(n, pars)
acf_plot(r6$rt, "ARCH(2)")
pacf_plot(r6$rt, "ARCH(2)")
acf_plot(r6$rt2, "ARCH(2)")
pacf_plot(r6$rt2, "ARCH(2)")

#################### ARCH(3) ##################################### 

######## EXEMPLO 7 #################### 
pars <- c(1.5, .1, .15, .1)
r7 <- archp(n, pars)
acf_plot(r7$rt, "ARCH(3)")
pacf_plot(r7$rt, "ARCH(3)")
acf_plot(r7$rt2, "ARCH(3)")
pacf_plot(r7$rt2, "ARCH(3)")

######## EXEMPLO 8 #################### 
pars <- c(1.5, .25, .45, .1)
r8 <- archp(n, pars)
acf_plot(r8$rt, "ARCH(3)")
pacf_plot(r8$rt, "ARCH(3)")
acf_plot(r8$rt2, "ARCH(3)")
pacf_plot(r8$rt2, "ARCH(3)")

######## EXEMPLO 9 #################### 
pars <- c(1.5, .3, .5, .1)
r9 <- archp(n, pars)
acf_plot(r9$rt, "ARCH(3)")
pacf_plot(r9$rt, "ARCH(3)")
acf_plot(r9$rt2, "ARCH(3)")
pacf_plot(r9$rt2, "ARCH(3)")

#################### ARCH(4) ##################################### 

######## EXEMPLO 10 ####################  
pars <- c(1., .1, .15, .1, .35)
r10 <- archp(n, pars)
acf_plot(r10$rt, "ARCH(4)")
pacf_plot(r10$rt, "ARCH(4)")
acf_plot(r10$rt2, "ARCH(4)")
pacf_plot(r10$rt2, "ARCH(4)")

######## EXEMPLO 11 #################### 
pars <- c(1., .1, .2, .3, .3)
r11 <- archp(n, pars)
acf_plot(r11$rt, "ARCH(4)")
pacf_plot(r11$rt, "ARCH(4)")
acf_plot(r11$rt2, "ARCH(4)")
pacf_plot(r11$rt2, "ARCH(4)")

#################### GARCH(1,1) ##################################### 
pars <- list(p12 = c(.5, .1, .15), p13 = c(.5, .2, .1), p14 = c(.5, .5, .3), 
             p15 = c(.5, .15, .18), p16 = c(.5, .1, .85), 
             p17 = c(.5, .01, .9), p18 = c(.5, .16, .83),
             p19 = c(.5, .16, .83))
n <- 1000
temp <- 'C:/Users/Gabriel/Desktop/arch_garch/ARCH_GARCH/Graficos/Series e Quadrado da Serie'
nomes1 <- paste0(temp, '/garch_exemplo0', 1:length(pars), '.png')
nomes2 <- paste0(temp, '/garch_acf_exemplo0', 1:length(pars), '.png')
nomes3 <- paste0(temp, '/garch_pacf_exemplo0', 1:length(pars), '.png')
nomes4 <- paste0(temp, '/garch_acf2_exemplo0', 1:length(pars), '.png')
nomes5 <- paste0(temp, '/garch_pacf2_exemplo0', 1:length(pars), '.png')

series <- map(pars, ~garch11(n, .x))

line <- map(series, ~line(as.data.frame(.x), 'GARCH(1,1)')) %>% 
  walk2(nomes1, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

acf <- map(series, 'rt') %>% map(~acf_plot(.x, 'GARCH(1,1) - retorno')) %>% 
  walk2(nomes2, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

pacf <- map(series, 'rt') %>% map(~pacf_plot(.x, 'GARCH(1,1) - retorno')) %>% 
  walk2(nomes3, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

acf2 <-map(series, 'rt2') %>% map(~acf_plot(.x, 'GARCH(1,1) - retorno')) %>% 
  walk2(nomes4, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

pacf2 <-map(series, 'rt2') %>% map(~acf_plot(.x, 'GARCH(1,1) - retorno')) %>% 
  walk2(nomes5, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

