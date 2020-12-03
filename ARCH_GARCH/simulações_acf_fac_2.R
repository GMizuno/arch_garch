source("archp.R")
source("garch11.R")
source("ar1-garch11.R")
require(ggplot2); require(magrittr); require(forecast)
set.seed(1)

#################### ARCH 1 ####################

######## EXEMPLO 1 ####################
alpha <- c(.1); w <- .5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p1acf2.png", plot =  p1acf)
ggsave("fac2\\p1pacf2.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
######## EXEMPLO 2 ####################

alpha <- c(.25); w <- .5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ 
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ 
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p2acf2.png", plot =  p1acf)
ggsave("fac2\\p2pacf2.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

######## EXEMPLO 3  ####################

alpha <- c(.8); w <- .5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ 
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p3acf2.png", plot =  p1acf)
ggsave("fac2\\p3pacf2.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

#################### ARCH 2 ####################

######## EXEMPLO 4  ####################

alpha <- c(.1, .25); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(2)",bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                        ", " ~ alpha[2] == .(alpha[2]) ~
                                        ", " ~ omega == .(w))) + ylim(c(-1,1))
p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                   ", " ~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) + ylim(c(-1,1))

ggsave("fac2\\p4acf2.png", plot =  p1acf)
ggsave("fac2\\p4pacf2.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

######## EXEMPLO 5  ####################

alpha <- c(.2, .4); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt2, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                   ", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                   ", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p5acf2.png", plot =  p1acf)
ggsave("fac2\\p5pacf2.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

######## EXEMPLO 6 - NÃO RODAR ####################

alpha <- c(.85, .8); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()
rm(alpha); rm(w); rm(r1)

# p1acf <- acf(r1$rt2, plot = F) %>% autoplot() + 
#   ggtitle(subtitle = "arch(2)",
#           bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                    ", "~ alpha[2] == .(alpha[2]) ~
#                    ", " ~ omega == .(w))) + 
#   ylim(c(-1,1))
# 
# p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() + 
#   ggtitle(subtitle = "arch(2)",
#           bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                    ", "~ alpha[2] == .(alpha[2]) ~
#                    ", " ~ omega == .(w))) + 
#   ylim(c(-1,1))
# 
# ggsave("fac2\\p6acf2.png", plot =  p1acf)
# ggsave("fac2\\p6pacf2.png", plot = p1pacf)
# rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

#################### ARCH 3 ####################

######## EXEMPLO 7  ####################

alpha <- c(.1, .15, .1); w <- 1.5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt2, plot = F) %>% autoplot() +
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p7acf2.png", plot =  p1acf)
ggsave("fac2\\p7pacf2.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

######## EXEMPLO 8  ####################
alpha <- c(.25, .45, .1); w <- 1.5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p8acf2.png", plot =  p1acf)
ggsave("fac2\\p8pacf2.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

######## EXEMPLO 9 - NÃO RODAR ####################
alpha <- c(.95, .8, .9); w <- 1.5
r1 <- archp(1000, alpha, w) %>% as.data.frame()
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

# p1acf <- acf(r1$rt2, plot = F) %>% autoplot() + 
#   ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                                          ", "~ alpha[2] == .(alpha[2]) ~
#                                          ", "~ alpha[3] == .(alpha[3]) ~
#                                          ", " ~ omega == .(w))) + 
#   ylim(c(-1,1))
# 
# p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() + 
#   ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                                          ", "~ alpha[2] == .(alpha[2]) ~
#                                          ", "~ alpha[3] == .(alpha[3]) ~
#                                          ", " ~ omega == .(w))) + 
#   ylim(c(-1,1))
# 
# ggsave("fac2\\p9acf2.png", plot =  p1acf)
# ggsave("fac2\\p9pacf2.png", plot = p1pacf)
# rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

#################### ARCH 4 ####################

######## EXEMPLO 10  ####################

alpha <- c(.1, .15, .1, .35); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt2, plot = F) %>% autoplot() +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p10acf2.png", plot =  p1acf)
ggsave("fac2\\p10pacf2.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

######## EXEMPLO 11 - NÃO RODAR ####################

alpha <- c(.75, .80, .7, .7); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

# p1acf <- acf(r1$rt2, plot = F) %>% autoplot() +
#   theme_minimal() + 
#   ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                                          ", "~ alpha[2] == .(alpha[2]) ~
#                                          ", "~ alpha[3] == .(alpha[3]) ~
#                                          ", "~ alpha[4] == .(alpha[4]) ~
#                                          ", " ~ omega == .(w))) + 
#   ylim(c(-1,1))
# 
# p1pacf <- pacf(r1$rt2, plot = F) %>% autoplot() +
#   theme_minimal() + 
#   ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                                          ", "~ alpha[2] == .(alpha[2]) ~
#                                          ", "~ alpha[3] == .(alpha[3]) ~
#                                          ", "~ alpha[4] == .(alpha[4]) ~
#                                          ", " ~ omega == .(w))) + 
#   ylim(c(-1,1))
# 
# ggsave("fac2\\p11acf2.png", plot =  p1acf)
# ggsave("fac2\\p11pacf2.png", plot = p1pacf)
# rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)

#################### GARCH 11 ####################

######## EXEMPLO 12  ####################
alpha1 <- .1; beta1 <- .15; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt2, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

p1pacf <- pacf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p12acf2.png", plot =  p1acf)
ggsave("fac2\\p12pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)

######## EXEMPLO 13  ####################

alpha1 <- .2; beta1 <- .1; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

p1pacf <- pacf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p13acf2.png", plot =  p1acf)
ggsave("fac2\\p13pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)

######## EXEMPLO 14  ####################

alpha1 <- .5; beta1 <- .3; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

p1pacf <- pacf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

ggsave("fac2\\p14acf2.png", plot =  p1acf)
ggsave("fac2\\p14pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)

######## EXEMPLO 15  ####################

alpha1 <- .15; beta1 <- .8; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))
p1pacf <- pacf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

ggsave("fac2\\p15acf2.png", plot =  p1acf)
ggsave("fac2\\p15pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)

######## EXEMPLO 16  #################
alpha1 <- .1; beta1 <- .85; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

p1pacf <- pacf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

ggsave("fac2\\p16acf2.png", plot =  p1acf)
ggsave("fac2\\p16pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)
######## EXEMPLO 17  ####################

alpha1 <- .01; beta1 <- .9; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

p1pacf <- pacf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

ggsave("fac2\\p17acf2.png", plot =  p1acf)
ggsave("fac2\\p17pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)

#################### EXEMPLOS EXTRA #################### 

######## EXEMPLO 18 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

p1pacf <- autoplot(pacf(g1$rt^2, plot = F)) + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac2\\p18acf.png", plot =  p1acf)
ggsave("fac2\\p18pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)

######## EXEMPLO 19 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt2, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

p1pacf <- pacf(g1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
ggsave("fac2\\p19acf.png", plot =  p1acf)
ggsave("fac2\\p19pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)

#################### AR(1) + GARCH(1,1) ##################################### 

######## EXEMPLO 20 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .01
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf((g1$epst)^2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 

p1pacf <- pacf((g1$epst)^2, plot = F) %>%  autoplot() + 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 

ggsave("fac2\\p20acf2.png", plot =  p1acf)
ggsave("fac2\\p20pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1pacf); rm(p1acf); rm(g1)

######## EXEMPLO 21 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .1
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf((g1$epst)^2, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 

p1pacf <- pacf((g1$epst)^2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 
 
ggsave("fac2\\p21acf2.png", plot =  p1acf)
ggsave("fac2\\p21pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1pacf); rm(p1acf); rm(g1)

######## EXEMPLO 22 ################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .5
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf((g1$epst)^2, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 

p1pacf <- pacf((g1$epst)^2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 

ggsave("fac2\\p22acf2.png", plot =  p1acf)
ggsave("fac2\\p22pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1pacf); rm(p1acf); rm(g1)

######## EXEMPLO 23 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .7
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf((g1$epst)^2, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 

p1pacf <- pacf((g1$epst)^2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 

ggsave("fac2\\p23acf2.png", plot =  p1acf)
ggsave("fac2\\p23pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1pacf); rm(p1acf); rm(g1)

######## EXEMPLO 24 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- 1
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf((g1$epst)^2, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 

p1pacf <- pacf((g1$epst)^2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "Residuo ao quadrado", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) 

ggsave("fac2\\p24acf2.png", plot =  p1acf)
ggsave("fac2\\p24pacf2.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(p1pacf); rm(p1acf); rm(g1)