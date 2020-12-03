source("archp.R")
source("garch11.R")
source("ar1-garch11.R")
require(ggplot2); require(magrittr)
set.seed(1)

#################### ARCH(1) ##################################### 

######## EXEMPLO 1 #################### 
alpha <- c(.1); w <- .5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt) ) + 
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w))) +
  ylab(bquote(~r[t])) + xlab("Tempo") + theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) + 
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w)   )) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") + theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w)   )) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch1_exemplo1_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch1_exemplo1_2.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch1_exemplo1_3.png", plot = p3, width = 9.7, height = 4)

rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
######## EXEMPLO 2 #################### 

alpha <- c(.25); w <- .5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt) ) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w)   )) +
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w)   )) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w)   )) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch1_exemplo2_5.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch1_exemplo2_6.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch1_exemplo2_7.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
######## EXEMPLO 3 #################### 

alpha <- c(.8); w <- .5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt) ) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w)   )) +
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w)   )) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w)   )) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch1_exemplo3_9.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch1_exemplo3_10.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch1_exemplo3_11.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
#################### ARCH(2) ##################################### 

######## EXEMPLO 4 #################### 

alpha <- c(.1, .25); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) +
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch2_exemplo1_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch2_exemplo1_2.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch2_exemplo1_3.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
######## EXEMPLO 5 #################### 

alpha <- c(.2, .4); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) +
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch2_exemplo2_5.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch2_exemplo2_6.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch2_exemplo2_7.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
######## EXEMPLO 6 #################### 

alpha <- c(.85, .8); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) +
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch2_exemplo2_9.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch2_exemplo2_10.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch2_exemplo2_11.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
#################### ARCH(3) ##################################### 

######## EXEMPLO 7 #################### 

alpha <- c(.1, .15, .1); w <- 1.5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt) ) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch3_exemplo1_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch3_exemplo1_2.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch3_exemplo1_3.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
######## EXEMPLO 8 #################### 

alpha <- c(.25, .45, .1); w <- 1.5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch3_exemplo2_5.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch3_exemplo2_6.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch3_exemplo2_7.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
######## EXEMPLO 9 #################### 

alpha <- c(.3, .5, .1); w <- 1.5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt) ) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch3_exemplo3_9.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch3_exemplo3_10.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch3_exemplo3_11.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
#################### ARCH(4) ##################################### 

######## EXEMPLO 10 ####################  

alpha <- c(.1, .15, .1, .35); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch4_exemplo1_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch4_exemplo1_2.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch4_exemplo1_3.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
######## EXEMPLO 11 #################### 

alpha <- c(.1, .2, .3, .3); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1 <- ggplot(r1, aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p2 <- ggplot(r1, aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

p3 <- ggplot(r1, aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) +
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0))

ggsave("Graficos\\arch4_exemplo2_5.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\arch4_exemplo2_6.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\arch4_exemplo2_7.png", plot = p3, width = 9.7, height = 4)
rm(alpha); rm(w); rm(p1); rm(p2); rm(p3); rm(r1)
#################### GARCH(1,1) ##################################### 

######## EXEMPLO 12 ####
alpha1 <- .1; beta1 <- .15; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p2 <- ggplot(g1,  aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p3 <- ggplot(g1,  aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))
ggsave("Graficos\\garch11_exemplo1_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo1_2.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo1_3.png", plot = p3, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(p1); rm(p2); rm(p3); rm(g1)
######## EXEMPLO 13 ####

alpha1 <- .2; beta1 <- .1; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p2 <- ggplot(g1,  aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p3 <- ggplot(g1,  aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))
ggsave("Graficos\\garch11_exemplo2_4.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo2_5.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo2_6.png", plot = p3, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(p1); rm(p2); rm(p3); rm(g1)
######## EXEMPLO 14 ####

alpha1 <- .5; beta1 <- .3; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p2 <- ggplot(g1,  aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p3 <- ggplot(g1,  aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))
ggsave("Graficos\\garch11_exemplo3_7.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo3_8.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo3_9.png", plot = p3, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(p1); rm(p2); rm(p3); rm(g1)
######## EXEMPLO 15 ####

alpha1 <- .15; beta1 <- .8; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p2 <- ggplot(g1,  aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p3 <- ggplot(g1,  aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))
ggsave("Graficos\\garch11_exemplo4_10.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo4_11.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo4_12.png", plot = p3, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(p1); rm(p2); rm(p3); rm(g1)
######## EXEMPLO 16 ####

alpha1 <- .1; beta1 <- .85; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p2 <- ggplot(g1,  aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p3 <- ggplot(g1,  aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))
ggsave("Graficos\\garch11_exemplo5_13.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo5_14.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo5_15.png", plot = p3, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(p1); rm(p2); rm(p3); rm(g1)
######## EXEMPLO 17 ####

alpha1 <- .01; beta1 <- .9; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p2 <- ggplot(g1,  aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p3 <- ggplot(g1,  aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))
ggsave("Graficos\\garch11_exemplo6_16.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo6_17.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo6_18.png", plot = p3, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(p1); rm(p2); rm(p3); rm(g1)
#################### EXEMPLOS EXTRA #################### 

######## EXEMPLO 18 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p2 <- ggplot(g1,  aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p3 <- ggplot(g1,  aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

ggsave("Graficos\\garch11_exemplo7_19.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo7_20.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo7_21.png", plot = p3, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(p1); rm(p2); rm(p3); rm(g1)
######## EXEMPLO 19 ####################
alpha1 <- .16; beta1 <- .83; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p2 <- ggplot(g1,  aes(x = time, y = sqrt(sigma2))) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~sigma[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

p3 <- ggplot(g1,  aes(x = time, y = rt2)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~r[t]^2)) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

ggsave("Graficos\\garch11_exemplo8_22.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo8_23.png", plot = p2, width = 9.7, height = 4)
ggsave("Graficos\\garch11_exemplo8_24.png", plot = p3, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(p1); rm(p2); rm(p3); rm(g1)
#################### AR(1) + GARCH(1,1) ##################################### 
######## EXEMPLO 20 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .01
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") + theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

p2 <- ggplot(g1,  aes(x = time, y = epst)) +
  geom_line(size = 0.52, colour = "#000000") + theme_minimal() + 
  ylab(bquote(~epsilon[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

ggsave("Graficos\\ar_garch11_exemplo1_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\ar_garch11_exemplo1_2.png", plot = p2, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1); rm(p2); rm(g1)

######## EXEMPLO 21 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .1
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") + theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

p2 <- ggplot(g1,  aes(x = time, y = epst)) +
  geom_line(size = 0.52, colour = "#000000") + theme_minimal() + 
  ylab(bquote(~epsilon[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

ggsave("Graficos\\ar_garch11_exemplo2_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\ar_garch11_exemplo2_2.png", plot = p2, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1); rm(p2); rm(g1)
######## EXEMPLO 22 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .5
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") + theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

p2 <- ggplot(g1,  aes(x = time, y = epst)) +
  geom_line(size = 0.52, colour = "#000000") + theme_minimal() + 
  ylab(bquote(~epsilon[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

ggsave("Graficos\\ar_garch11_exemplo3_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\ar_garch11_exemplo3_2.png", plot = p2, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1); rm(p2); rm(g1)

######## EXEMPLO 23 ####################

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .7
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") + theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

p2 <- ggplot(g1,  aes(x = time, y = epst)) +
  geom_line(size = 0.52, colour = "#000000") +
  theme_minimal() + 
  ylab(bquote(~epsilon[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

ggsave("Graficos\\ar_garch11_exemplo4_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\ar_garch11_exemplo4_2.png", plot = p2, width = 9.7, height = 4)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1); rm(p2); rm(g1)

######## EXEMPLO 24 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- 1
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1 <- ggplot(g1,  aes(x = time, y = rt)) +
  geom_line(size = 0.52, colour = "#000000") + theme_minimal() + 
  ylab(bquote(~r[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

p2 <- ggplot(g1,  aes(x = time, y = epst)) +
  geom_line(size = 0.52, colour = "#000000") + theme_minimal() + 
  ylab(bquote(~epsilon[t])) + xlab("Tempo") +
  theme(axis.title.y = element_text(angle=0)) + 
  ggtitle(subtitle = "ar(1)-garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1)))

ggsave("Graficos\\ar_garch11_exemplo5_1.png", plot = p1, width = 9.7, height = 4)
ggsave("Graficos\\ar_garch11_exemplo5_2.png", plot = p2, width = 9.7, height = 4)

rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1); rm(p2); rm(g1)
