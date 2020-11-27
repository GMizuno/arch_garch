source("egarch_sim.R")
require(ggplot2); require(magrittr); require(forecast); require(xlsx)
set.seed(3)

# Exemplo 1 (arquivo 1)---------------------------------------------------------------
pars <- c(-.077, .218, .957, -.144)
r1 <- egarch_sim(1000, pars) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt) ) + 
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "egarch(1,1)",
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~
                   ', ' ~ gamma == .(pars[4])
          )
  ) +
  ylab(bquote( ~ r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle = 0))

p1acf <- acf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))

p1acf2 <- acf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))
p1pacf2 <- pacf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))

ggsave("Graficos\\p1acf_exemplo01.png", plot =  p1acf, width = 9.7, height = 4)
ggsave("Graficos\\p1pacf_exemplo01.png", plot = p1pacf, width = 9.7, height = 4)
ggsave("Graficos\\p1acf2_exemplo01.png", plot =  p1acf2, width = 9.7, height = 4)
ggsave("Graficos\\p1pacf2_exemplo01.png", plot = p1pacf2, width = 9.7, height = 4)
ggsave("Graficos\\egarch_exemplo01.png", plot = p1, width = 9.7, height = 4)
rm(p1); rm(p1acf); rm(p1pacf); rm(p1acf2); rm(p1pacf2)

# Exemplo 2 (arquivo 3)---------------------------------------------------------------
pars <- c(-.052, .076, .987, -.102)
r2 <- egarch_sim(1000, pars) %>% as.data.frame()

p2 <- ggplot(r2, aes(x = time, y = rt) ) + 
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) +
  ylab(bquote(~r[t])) + xlab("Tempo") + 
  theme(axis.title.y = element_text(angle=0))

p1acf <- acf(r2$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r2$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))

p1acf2 <- acf(r2$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))
p1pacf2 <- pacf(r2$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))

ggsave("Graficos\\p1acf_exemplo02.png", plot =  p1acf, width = 9.7, height = 4)
ggsave("Graficos\\p1pacf_exemplo02.png", plot = p1pacf, width = 9.7, height = 4)
ggsave("Graficos\\p1acf2_exemplo02.png", plot =  p1acf2, width = 9.7, height = 4)
ggsave("Graficos\\p1pacf2_exemplo02.png", plot = p1pacf2, width = 9.7, height = 4)
ggsave("Graficos\\egarch_exemplo02.png", plot = p2, width = 9.7, height = 4)
rm(p2); rm(p1acf); rm(p1pacf); rm(p1acf2); rm(p1pacf2)

# Exemplo 3 (arquivo 4)---------------------------------------------------------------
pars <- c(-.043, .069, .992, -.066)
r3 <- egarch_sim(1000, pars) %>% as.data.frame()

p3 <- ggplot(r3, aes(x = time, y = rt) ) + 
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) +
  ylab(bquote(~r[t])) + xlab("Tempo") + 
  theme(axis.title.y = element_text(angle=0))

p1acf <- acf(r3$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r3$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))

p1acf2 <- acf(r3$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))
p1pacf2 <- pacf(r3$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))

ggsave("Graficos\\p1acf_exemplo03.png", plot =  p1acf, width = 9.7, height = 4)
ggsave("Graficos\\p1pacf_exemplo03.png", plot = p1pacf, width = 9.7, height = 4)
ggsave("Graficos\\p1acf2_exemplo03.png", plot =  p1acf2, width = 9.7, height = 4)
ggsave("Graficos\\p1pacf2_exemplo03.png", plot = p1pacf2, width = 9.7, height = 4)
ggsave("Graficos\\egarch_exemplo03.png", plot = p3, width = 9.7, height = 4)
rm(p3); rm(p1acf); rm(p1pacf); rm(p1acf2); rm(p1pacf2)


# Exemplo 4 (arquivo 1 Adrian)---------------------------------------------------------------
pars <- c(-.057, .216, .951, -.151)
r4 <- egarch_sim(1000, pars) %>% as.data.frame()

p4 <- ggplot(r4, aes(x = time, y = rt) ) + 
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) +
  ylab(bquote(~r[t])) + xlab("Tempo") + 
  theme(axis.title.y = element_text(angle=0))

p1acf <- acf(r4$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r4$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))

p1acf2 <- acf(r4$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))
p1pacf2 <- pacf(r4$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "egarch(1,1)", 
          bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                   ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                   ', ' ~ gamma == .(pars[4]))) + 
  ylim(c(-1,1))

ggsave("Graficos\\p1acf_exemplo04.png", plot =  p1acf, width = 9.7, height = 4)
ggsave("Graficos\\p1pacf_exemplo04.png", plot = p1pacf, width = 9.7, height = 4)
ggsave("Graficos\\p1acf2_exemplo04.png", plot =  p1acf2, width = 9.7, height = 4)
ggsave("Graficos\\p1pacf2_exemplo04.png", plot = p1pacf2, width = 9.7, height = 4)
ggsave("Graficos\\egarch_exemplo04.png", plot = p4, width = 9.7, height = 4)
rm(p4); rm(p1acf); rm(p1pacf); rm(p1acf2); rm(p1pacf2)
# Savando -----------------------------------------------------------------
data <- data.frame(r1 = r1$rt, r1quad =r1$rt2, 
                   r2 = r2$rt, r2quad = r2$rt2, 
                   r3 = r3$rt, r3quad = r3$rt2,
                   r4 = r4$rt, r4quad = r4$rt2)
write.xlsx(x = data, file = 'dados.xls')
write.csv(x = data, file = 'dados.csv')
