setwd(r'(C:\Users\Gabriel\Desktop\arch_garch\AR-GARCH)')
source('ar1-garch11_sim.R')
source('ggplot_graficos.R')
require(ggplot2); require(magrittr); require(purrr)
set.seed(1)

# Exemplo 1 (arquivo 4)---------------------------------------------------------------
pars <- c(0, -.053, .028, .06, .935)
rt <- AR1_Garch11(1000, pars)

line(rt)
acf_plot(rt$rt)
pacf_plot(rt$rt)
acf_plot(rt$rt2)
pacf_plot(rt$rt2)

# Exemplo 2 (arquivo 1)---------------------------------------------------------------
pars <- c(.291, .109, .432, .158, .808)
rt <- AR1_Garch11(1000, pars)

line(rt)
acf_plot(rt$rt)
pacf_plot(rt$rt)
acf_plot(rt$rt2)
pacf_plot(rt$rt2)

# Exemplo 3 (arquivo 3)---------------------------------------------------------------
pars <- c(-.051, .226, .324, .124, .777)
rt <- AR1_Garch11(1000, pars)

line(rt)
acf_plot(rt$rt)
pacf_plot(rt$rt)
acf_plot(rt$rt2)
pacf_plot(rt$rt2)

# Salvando ----------------------------------------------------------------

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

line <- map(series, ~line(as.data.frame(.x))) %>% 
  walk2(nomes1, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

acf <- map(series, 'rt') %>% map(~acf_plot(.x)) %>% 
  walk2(nomes2, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

pacf <- map(series, 'rt') %>% map(~pacf_plot(.x)) %>% 
  walk2(nomes3, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

acf2 <-map(series, 'rt2') %>% map(~acf_plot(.x)) %>% 
  walk2(nomes4, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

pacf2 <-map(series, 'rt2') %>% map(~acf_plot(.x)) %>% 
  walk2(nomes5, ., ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
