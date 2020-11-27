source("archp.R")
source("garch11.R")
source("ar1-garch11.R")
require(ggplot2); require(magrittr); require(forecast)
set.seed(1)

#################### ARCH1 ####################

######## EXEMPLO 1 ####################
alpha <- c(.1); w <- .5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

p1acf2 <- acf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf2 <- pacf(r1$rt2, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))


ggsave("fac\\p1acf.png", plot =  p1acf)
ggsave("fac\\p1pacf.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
######## EXEMPLO 2 ####################

alpha <- c(.25); w <- .5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", 
         bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p2acf.png", plot =  p1acf)
ggsave("fac\\p2pacf.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
######## EXEMPLO 3  ####################

alpha <- c(.8); w <- .5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~ ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p3acf.png", plot =  p1acf)
ggsave("fac\\p3pacf.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
#################### ARCH2 ####################
######## EXEMPLO 4  ####################

alpha <- c(.1, .25); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p4acf.png", plot =  p1acf)
ggsave("fac\\p4pacf.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
######## EXEMPLO 5  ####################

alpha <- c(.2, .4); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(2)",
          bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p5acf.png", plot =  p1acf)
ggsave("fac\\p5pacf.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
######## EXEMPLO 6 - NÃO RODAR ####################
alpha <- c(.85, .8); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()
rm(alpha); rm(w); rm(r1)

# p1acf <- acf(r1$rt, plot = F) %>% autoplot() +
#   ggtitle(subtitle = "arch(2)",
#           bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
#                    ", " ~ omega == .(w))) +
#   ylim(c(0,1))
# p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() +
#   ggtitle(subtitle = "arch(2)",
#           bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~", "~ alpha[2] == .(alpha[2]) ~
#                    ", " ~ omega == .(w))) +
#   ylim(c(0,1))
# 
# ggsave("fac\\p6acf.png", plot =  p1acf)
# ggsave("fac\\p6pacf.png", plot = p1pacf)
# rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
#################### ARCH3 ####################
######## EXEMPLO 7  ####################

alpha <- c(.1, .15, .1); w <- 1.5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p7acf.png", plot =  p1acf)
ggsave("fac\\p7pacf.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
######## EXEMPLO 8  ####################
alpha <- c(.25, .45, .1); w <- 1.5
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p8acf.png", plot =  p1acf)
ggsave("fac\\p8pacf.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
######## EXEMPLO 9 - NÃO RODAR ####################
alpha <- c(.95, .8, .9); w <- 1.5
r1 <- archp(1000, alpha, w) %>% as.data.frame()
rm(alpha); rm(w); rm(r1)
# p1acf <- acf(r1$rt, plot = F) %>% autoplot() +
#   ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                                          ", "~ alpha[2] == .(alpha[2]) ~
#                                          ", "~ alpha[3] == .(alpha[3]) ~
#                                          ", " ~ omega == .(w))) +
#   ylim(c(-1,1))
# p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() +
#   ggtitle(subtitle = "arch(3)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                                          ", "~ alpha[2] == .(alpha[2]) ~
#                                          ", "~ alpha[3] == .(alpha[3]) ~
#                                          ", " ~ omega == .(w))) +
#   ylim(c(-1,1))
# 
# ggsave("fac\\p9acf.png", plot =  p1acf)
# ggsave("fac\\p9pacf.png", plot = p1pacf)
# rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
#################### ARCH4 ####################
######## EXEMPLO 10 ####################

alpha <- c(.1, .15, .1, .35); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()

p1acf <- acf(r1$rt, plot = F) %>% autoplot() +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() +
  theme_minimal() + 
  ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
                                         ", "~ alpha[2] == .(alpha[2]) ~
                                         ", "~ alpha[3] == .(alpha[3]) ~
                                         ", "~ alpha[4] == .(alpha[4]) ~
                                         ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p10acf.png", plot =  p1acf)
ggsave("fac\\p10pacf.png", plot = p1pacf)
rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
######## EXEMPLO 11 - NÃO RODAR ####################
alpha <- c(.75, .80, .7, .7); w <- 1
r1 <- archp(1000, alpha, w) %>% as.data.frame()
rm(alpha); rm(w); rm(r1)
# p1acf <- acf(r1$rt, plot = F) %>% autoplot() +
#   theme_minimal() +
#   ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                                          ", "~ alpha[2] == .(alpha[2]) ~
#                                          ", "~ alpha[3] == .(alpha[3]) ~
#                                          ", "~ alpha[4] == .(alpha[4]) ~
#                                          ", " ~ omega == .(w))) +
#   ylim(c(-1,1))
# p1pacf <- pacf(r1$rt, plot = F) %>% autoplot() +
#   theme_minimal() +
#   ggtitle(subtitle = "arch(4)", bquote("Parametros:" ~ alpha[1] == .(alpha[1]) ~
#                                          ", "~ alpha[2] == .(alpha[2]) ~
#                                          ", "~ alpha[3] == .(alpha[3]) ~
#                                          ", "~ alpha[4] == .(alpha[4]) ~
#                                          ", " ~ omega == .(w))) +
#   ylim(c(-1,1))
# 
# ggsave("fac\\p11acf.png", plot =  p1acf)
# ggsave("fac\\p11pacf.png", plot = p1pacf)
# rm(alpha); rm(w); rm(p1pacf); rm(p1acf); rm(r1)
#################### GARCH11 ####################
######## EXEMPLO 12  ####################

alpha1 <- .1; beta1 <- .15; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
ggsave("fac\\p12acf.png", plot =  p1acf)
ggsave("fac\\p12pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)
######## EXEMPLO 13  ####################

alpha1 <- .2; beta1 <- .1; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w)))

ggsave("fac\\p13acf.png", plot =  p1acf)
ggsave("fac\\p13pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)
######## EXEMPLO 14  ####################

alpha1 <- .5; beta1 <- .3; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p14acf.png", plot =  p1acf)
ggsave("fac\\p14pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)
######## EXEMPLO 15  ####################

alpha1 <- .15; beta1 <- .8; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(g1$rt, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p15acf.png", plot =  p1acf)
ggsave("fac\\p15pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)
######## EXEMPLO 16  #################
alpha1 <- .1; beta1 <- .85; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p16acf.png", plot =  p1acf)
ggsave("fac\\p16pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)
######## EXEMPLO 17  ####################

alpha1 <- .01; beta1 <- .9; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p17acf.png", plot =  p1acf)
ggsave("fac\\p17pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)
#################### EXEMPLOS EXTRA #################### 
######## EXEMPLO 18 #################### 
alpha1 <- .16; beta1 <- .83; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p18acf.png", plot =  p1acf)
ggsave("fac\\p18pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)
######## EXEMPLO 19 #################### 
alpha1 <- .16; beta1 <- .83; w <- .5
g1 <- garch11(1000, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))
p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "garch(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w))) + 
  ylim(c(-1,1))

ggsave("fac\\p19acf.png", plot =  p1acf)
ggsave("fac\\p19pacf.png", plot = p1pacf)
rm(alpha1); rm(beta1); rm(w); rm(p1pacf); rm(p1acf); rm(g1)
#################### AR(1) + GARCH(1,1) ##################################### 
######## EXEMPLO 20 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .01
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))
p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1acf_eps <- acf(g1$epst, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = bquote(~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1pacf_eps <- pacf(g1$epst, plot = F) %>% autoplot() + 
  ggtitle(subtitle = bquote( ~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

ggsave("fac\\p20acf.png", plot =  p1acf)
ggsave("fac\\p20pacf.png", plot = p1pacf)
ggsave("fac\\p20acf_eps.png", plot = p1acf_eps)
ggsave("fac\\p20pacf_eps.png", plot = p1pacf_eps)
rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(g1)
rm(p1acf); rm(p1pacf); rm(p1acf_eps); rm(p1pacf_eps)

######## EXEMPLO 21 #################### 
alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .1
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1)) 

p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1acf_eps <- acf(g1$epst, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = bquote( ~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1pacf_eps <- pacf(g1$epst, plot = F) %>% autoplot() + 
  ggtitle(subtitle = bquote( ~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1)) 

ggsave("fac\\p21acf.png", plot =  p1acf)
ggsave("fac\\p21pacf.png", plot = p1pacf)
ggsave("fac\\p21acf_eps.png", plot = p1acf_eps)
ggsave("fac\\p21pacf_eps.png", plot = p1pacf_eps)

rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(g1)
rm(p1acf); rm(p1pacf); rm(p1acf_eps); rm(p1pacf_eps)

######## EXEMPLO 22 #################### 
alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .5
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1acf_eps <- acf(g1$epst, plot = F) %>% autoplot()+ 
  ggtitle(subtitle = bquote(~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1pacf_eps <- pacf(g1$epst, plot = F) %>% autoplot() + 
  ggtitle(subtitle = bquote( ~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

ggsave("fac\\p22acf.png", plot =  p1acf)
ggsave("fac\\p22pacf.png", plot = p1pacf)
ggsave("fac\\p22acf_eps.png", plot = p1acf_eps)
ggsave("fac\\p22pacf_eps.png", plot = p1pacf_eps)

rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(g1)
rm(p1acf); rm(p1pacf); rm(p1acf_eps); rm(p1pacf_eps)

######## EXEMPLO 23 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- .7
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1acf_eps <- acf(g1$epst, plot = F) %>% autoplot() +  
  ggtitle(subtitle = bquote( ~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1pacf_eps <- pacf(g1$epst, plot = F) %>% autoplot() + 
  ggtitle(subtitle = bquote( ~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

ggsave("fac\\p23acf.png", plot =  p1acf)
ggsave("fac\\p23pacf.png", plot = p1pacf)
ggsave("fac\\p23acf_eps.png", plot = p1acf_eps)
ggsave("fac\\p23pacf_eps.png", plot = p1pacf_eps)

rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(g1)
rm(p1acf); rm(p1pacf); rm(p1acf_eps); rm(p1pacf_eps)

######## EXEMPLO 24 #################### 

alpha1 <- .16; beta1 <- .83; w <- .5; phi1 <- 1
g1 <- AR1(1000, phi1, alpha1, beta1, w) %>% as.data.frame()

p1acf <- acf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1pacf <- pacf(g1$rt, plot = F) %>% autoplot() + 
  ggtitle(subtitle = "AR(1) + GARCH(1,1)", 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1acf_eps <- acf(g1$epst, plot = F) %>% autoplot() +  
  ggtitle(subtitle = bquote( ~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

p1pacf_eps <- pacf(g1$epst, plot = F) %>% autoplot() + 
  ggtitle(subtitle = bquote( ~ epsilon[t]), 
          bquote("Parametros:" ~ alpha[1] == .(alpha1) ~ 
                   ", " ~ beta[1] == .(beta1) ~
                   ", " ~ omega == .(w) ~
                   ", " ~ phi[1] == .(phi1))) + 
  ylim(c(-1,1))

ggsave("fac\\p24acf.png", plot =  p1acf)
ggsave("fac\\p24pacf.png", plot = p1pacf)
ggsave("fac\\p24acf_eps.png", plot = p1acf_eps)
ggsave("fac\\p24pacf_eps.png", plot = p1pacf_eps)

rm(alpha1); rm(beta1); rm(w); rm(phi1); rm(g1)
rm(p1acf); rm(p1pacf); rm(p1acf_eps); rm(p1pacf_eps)
