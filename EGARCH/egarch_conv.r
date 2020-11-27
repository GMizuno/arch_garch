setwd("C:/Users/Gabriel/OneDrive/TCC/Codigos/R/EGARCH")
source("egarch_sim.r")
source("egarch_est.r")
require(ggplot2); require(dplyr)
set.seed(1)

# Monte Carlo ---------------------------------------------------------------
set.seed(1)
gerando <- function(n, par, pars_init){
  rt <- egarch_sim(n, pars)
  opt <- optim(par = pars_init, fn = llike_egarch, 
               method = "BFGS", control = list(fnscale=-1), rt = rt$rt, n = n)
  return(data.frame(omega = opt$par[1], alpha = opt$par[2], beta = opt$par[3],
                    gamma = opt$par[4]))
}

pars <- c(-.057, .216, .951, -.151)
pars_init <- c(-.05, .2, .95, -.15)

# M = 100 e n= 1000 -----------------------------------------------------------------
M <- 100; n <- 1000
teste <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(teste[,1]),
               alpha = unlist(teste[,2]),
               beta = unlist(teste[,3]),
               gamma = unlist(teste[,4]))

p1 <- ggplot(data, aes(x = seq_along(gamma), y = gamma)) +
  geom_line(size = .8, colour = "#0c4c8a") +
  geom_hline(aes(yintercept = pars[4]), col = 'red', linetype = "dashed") + 
  theme_minimal() +  
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma), 
          subtitle = bquote("Verdadeiro parâmetro gama" ~ 
                              gamma == .(pars[4]))) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p1

p2 <- ggplot(data, aes(x = gamma)) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal() + 
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma)) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p2

gamma_pad <- (data$gamma-mean(data$gamma))/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)
gamma_pad <- (data$gamma-mean(data$gamma))/sd(data$gamma)
qqnorm(gamma_pad)

# M = 200 e n= 1000 -----------------------------------------------------------------
M <- 200; n <- 1000
teste <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(teste[,1]),
              alpha = unlist(teste[,2]),
              beta = unlist(teste[,3]),
              gamma = unlist(teste[,4]))

p1 <- ggplot(data, aes(x = seq_along(gamma), y = gamma)) +
  geom_line(size = .8, colour = "#0c4c8a") +
  geom_hline(aes(yintercept = pars[4]), col = 'red', linetype = "dashed") + 
  theme_minimal() +  
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma), 
          subtitle = bquote("Verdadeiro parâmetro gama" ~ 
                              gamma == .(pars[4]))) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p1

p2 <- ggplot(data, aes(x = gamma)) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal() + 
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma)) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p2

gamma_pad <- (data$gamma-mean(data$gamma))/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)

# M = 500 e n= 1000 -----------------------------------------------------------------
M <- 500; n <- 1000
teste <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(teste[,1]),
               alpha = unlist(teste[,2]),
               beta = unlist(teste[,3]),
               gamma = unlist(teste[,4]))
p1 <- ggplot(data, aes(x = seq_along(gamma), y = gamma)) +
  geom_line(size = .8, colour = "#0c4c8a") +
  geom_hline(aes(yintercept = pars[4]), col = 'red', linetype = "dashed") + 
  theme_minimal() +  
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma), 
          subtitle = bquote("Verdadeiro parâmetro gama" ~ 
                              gamma == .(pars[4]))) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p1

p2 <- ggplot(data, aes(x = gamma)) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal() + 
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma)) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p2

gamma_pad <- (data$gamma-mean(data$gamma))/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)

# M = 100 e n= 2000 -----------------------------------------------------------------
M <- 100; n <- 2000
teste <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(teste[,1]),
               alpha = unlist(teste[,2]),
               beta = unlist(teste[,3]),
               gamma = unlist(teste[,4]))

p1 <- ggplot(data, aes(x = seq_along(gamma), y = gamma)) +
  geom_line(size = .8, colour = "#0c4c8a") +
  geom_hline(aes(yintercept = pars[4]), col = 'red', linetype = "dashed") + 
  theme_minimal() +  
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma), 
          subtitle = bquote("Verdadeiro parâmetro gama" ~ 
                              gamma == .(pars[4]))) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p1

p2 <- ggplot(data, aes(x = gamma)) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal() + 
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma)) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p2

gamma_pad <- (data$gamma-mean(data$gamma))/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)

# M = 200 e n= 2000 -----------------------------------------------------------------
M <- 200; n <- 2000
teste <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(teste[,1]),
               alpha = unlist(teste[,2]),
               beta = unlist(teste[,3]),
               gamma = unlist(teste[,4]))

p1 <- ggplot(data, aes(x = seq_along(gamma), y = gamma)) +
  geom_line(size = .8, colour = "#0c4c8a") +
  geom_hline(aes(yintercept = pars[4]), col = 'red', linetype = "dashed") + 
  theme_minimal() +  
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma), 
          subtitle = bquote("Verdadeiro parâmetro gama" ~ 
                              gamma == .(pars[4]))) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p1

p2 <- ggplot(data, aes(x = gamma)) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal() + 
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma)) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p2

gamma_pad <- (data$gamma-mean(data$gamma))/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)

# M = 500 e n= 2000 -----------------------------------------------------------------
M <- 500; n <- 2000
teste <- replicate(M, gerando(n, pars, pars_init), simplify = TRUE) %>% t() 
data <- tibble(omega = unlist(teste[,1]),
               alpha = unlist(teste[,2]),
               beta = unlist(teste[,3]),
               gamma = unlist(teste[,4]))
p1 <- ggplot(data, aes(x = seq_along(gamma), y = gamma)) +
  geom_line(size = .8, colour = "#0c4c8a") +
  geom_hline(aes(yintercept = pars[4]), col = 'red', linetype = "dashed") + 
  theme_minimal() +  
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma), 
          subtitle = bquote("Verdadeiro parâmetro gama" ~ 
                              gamma == .(pars[4]))) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p1

p2 <- ggplot(data, aes(x = gamma)) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal() + 
  ggtitle(label = bquote('Convergência do estimador de' ~ gamma)) +
  ylab(bquote(hat(gamma))) + xlab("Amostra") + 
  theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
p2

gamma_pad <- (data$gamma-mean(data$gamma))/sd(data$gamma)
qqnorm(gamma_pad)
qqline(gamma_pad)
