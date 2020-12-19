source("C:/Users/Gabriel/Desktop/arch_garch/EGARCH/egarch_sim.R")
source("C:/Users/Gabriel/Desktop/arch_garch/ggplot_graficos.R")
require(ggplot2); require(magrittr); require(purrr)
set.seed(3)
temp <- 'C:/Users/Gabriel/Desktop/arch_garch/EGARCH/Graficos'

# Exemplo 1 (arquivo 1)---------------------------------------------------------------
pars <- c(-.077, .218, .957, -.144)
r1 <- egarch_sim(1000, pars) %>% as.data.frame()

# Exemplo 2 (arquivo 3)---------------------------------------------------------------
pars <- c(-.052, .076, .987, -.102)
r2 <- egarch_sim(1000, pars) %>% as.data.frame()

# Exemplo 3 (arquivo 4)---------------------------------------------------------------
pars <- c(-.043, .069, .992, -.066)
r3 <- egarch_sim(1000, pars) %>% as.data.frame()

# Exemplo 4 (arquivo 1 Adrian)---------------------------------------------------------------
pars <- c(-.057, .216, .951, -.151)
r4 <- egarch_sim(1000, pars) %>% as.data.frame()

# Salvando os graficos ---------------------------------------------------------------
set.seed(3)
nome1 <- paste0(temp, '/egarch_exemplo0', 1:4, '.png')
nome2 <- paste0(temp, '/acf_exemplo0', 1:4, '.png')
nome3 <- paste0(temp, '/pacf_exemplo0', 1:4, '.png')
nome4 <- paste0(temp, '/acf2_exemplo0', 1:4, '.png')
nome5 <- paste0(temp, '/pacf2_exemplo0', 1:4, '.png')

retornos <- list(
  r1 = r1, r2 = r2, r3 = r3, r4 = r4
)

pars <- list(
  pars1 = c(-.077, .218, .957, -.144),
  pars2 = c(-.052, .076, .987, -.102),
  pars3 = c(-.043, .069, .992, -.066),
  pars4 = c(-.057, .216, .951, -.151)
)

all_lines <- map(retornos, ~ line(.x,'EGARCH(1,1)')) 
walk2(nome1, all_lines, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

all_acf <- map(retornos, ~ acf_plot(.x$rt, 'EGARCH(1,1)')) 
walk2(nome2, all_acf,  ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

all_pacf <- map(retornos, ~ pacf_plot(.x$rt, 'EGARCH(1,1)')) 
walk2(nome3, all_pacf, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

all_acf2 <- map(retornos, ~ pacf_plot(.x$rt2, 'EGARCH(1,1)')) 
walk2(nome4, all_acf2, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))

all_pacf2 <- map(retornos, ~ acf_plot(.x$rt2, 'EGARCH(1,1)')) 
walk2(nome5, all_pacf2, ~ggsave(filename = .x, plot = .y, width = 9.7, height = 4))
