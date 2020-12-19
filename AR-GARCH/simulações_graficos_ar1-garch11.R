setwd(r'(C:\Users\Gabriel\Desktop\arch_garch\AR-GARCH)')
source('C:/Users/Gabriel/Desktop/arch_garch/AR-GARCH/ar1-garch11_sim.R')
source("C:/Users/Gabriel/Desktop/arch_garch/ggplot_graficos.R")
require(ggplot2); require(magrittr); require(purrr)
set.seed(1)

# NAO RODAR OS EXEMPLOS 
# Exemplo 1 (arquivo 4)---------------------------------------------------------------
pars <- c(0, -.053, .028, .06, .935)
rt <- AR1_Garch11(1000, pars)

line(rt,'AR1-GARCH11')
acf_plot(rt$rt,'AR1-GARCH11')
pacf_plot(rt$rt,'AR1-GARCH11')
acf_plot(rt$rt2,'AR1-GARCH11')
pacf_plot(rt$rt2,'AR1-GARCH11')

# Exemplo 2 (arquivo 1)---------------------------------------------------------------
pars <- c(.291, .109, .432, .158, .808)
rt <- AR1_Garch11(1000, pars)

line(rt,'AR1-GARCH11')
acf_plot(rt$rt,'AR1-GARCH11')
pacf_plot(rt$rt,'AR1-GARCH11')
acf_plot(rt$rt2,'AR1-GARCH11')
pacf_plot(rt$rt2,'AR1-GARCH11')

# Exemplo 3 (arquivo 3)---------------------------------------------------------------
pars <- c(-.051, .226, .324, .124, .777)
rt <- AR1_Garch11(1000, pars)

line(rt,'AR1-GARCH11')
acf_plot(rt$rt,'AR1-GARCH11')
pacf_plot(rt$rt,'AR1-GARCH11')
acf_plot(rt$rt2,'AR1-GARCH11')

# Salvando ----------------------------------------------------------------
set.seed(1)
pars <- list(p1 = c(0, -.053, .028, .06, .935),
             p2 = c(.291, .109, .432, .158, .808), 
             p3 = c(-.051, .226, .324, .124, .777))
n <- 1000
temp <- r'(C:\Users\Gabriel\Desktop\arch_garch\AR-GARCH\Graficos\Series e Quadrado da Serie)'
nomes1 <- paste0(temp, '/ar_garch_exemplo0', 1:length(pars), '.png')
nomes2 <- paste0(temp, '/ar_garch_acf_exemplo0', 1:length(pars), '.png')
nomes3 <- paste0(temp, '/ar_garch_pacf_exemplo0', 1:length(pars), '.png')
nomes4 <- paste0(temp, '/ar_garch_acf2_exemplo0', 1:length(pars), '.png')
nomes5 <- paste0(temp, '/ar_garch_pacf2_exemplo0', 1:length(pars), '.png')

series <- map(pars, ~AR1_Garch11(n, .x))

line <- map(series, ~line(as.data.frame(.x), 'AR1-GARCH11')) %>% 
  walk2(nomes1, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

acf <- map(series, 'rt') %>% map(~acf_plot(.x, 'FAC do retorno, AR1-GARCH11')) %>% 
  walk2(nomes2, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

pacf <- map(series, 'rt') %>% map(~pacf_plot(.x, 'FACp do retorno, AR1-GARCH11')) %>% 
  walk2(nomes3, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

acf2 <-map(series, 'rt2') %>% map(~acf_plot(.x, 'FAC do quadrado, AR1-GARCH11')) %>% 
  walk2(nomes4, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

pacf2 <-map(series, 'rt2') %>% map(~acf_plot(.x, 'FACp do quadrado, AR1-GARCH11')) %>% 
  walk2(nomes5, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
