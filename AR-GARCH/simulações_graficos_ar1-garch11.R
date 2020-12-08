source('ar1-garch11_sim.R')
source('ggplot_graficos.R')
require(ggplot2); require(magrittr); require(purrr)

dir <- r'(C:\Users\Gabriel\Desktop\arch_garch\AR-GARCH\Graficos)'

# Exemplo 1 (arquivo 4)---------------------------------------------------------------
pars <- c(-.003, -.053, .028, .06, .935)
rt <- AR1_Garch11(pars, 1000)

line(rt, pars)
acf_plot(rt$rt)
pacf_plot(rt$rt)
acf_plot(rt$rt2)
pacf_plot(rt$rt2)

# Exemplo 2 (arquivo )---------------------------------------------------------------
# Exemplo 3 (arquivo )---------------------------------------------------------------
# Exemplo 4 (arquivo )---------------------------------------------------------------
# Exemplo 5 (arquivo )---------------------------------------------------------------