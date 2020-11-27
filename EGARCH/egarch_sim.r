# n.burn serve para excluir as primeiras simulações devido a inicialização 
# pars é um vetor de parametros, estou passando desse 
# jeito pois a função optim so aceita assim, ou seja, não poderia passar function(n, omega, alpha,....)
# omega intercepto de sigma_{t}^2 

########################## MODELO USADO ######################################

# rt = sigma_{t}*u_t
# log(sigma_{t}^2) = omega + beta*log(sigma_{t-1}^2) +
#                  + alpha*abs(r_{t-1}/sigma_{t-1} - \sqrt(\pi/2)) 
#                  + gamma*r_{t-1}/sigma_{t-1}
 
##############################################################################

# Simulando --------------------------------------------------------------
egarch_sim <- function(n, pars){
  w <- pars[1]
  alpha  <- pars[2]
  beta <- pars[3]
  gamma  <- pars[4]
  
  n.burn = 100
  # Inicizalizando
  N <- n+n.burn
  rt <- numeric(N); sigma2 <- numeric(N)
  
  # Armazenando o primeiro valor de sigma2 e rt.
  # Estou colocando log(sigma_{0}^2) = w/1-beta, pensei em colcoar sigma_{0}^2 = 1
  sigma2[1] <- exp(1) 
  rt[1] <- sqrt(sigma2[1])*rnorm(1)
  
  for (i in 2:N){
    zt <- rt[i-1]/sqrt(sigma2[i-1])
    sigma2[i] <- exp(w + beta*log(sigma2[i-1]) + 
                       alpha*abs(zt-sqrt(2/pi)) + 
                       gamma*zt)
    rt[i] <- sqrt(sigma2[i])*rnorm(1)
  }
  
  rt <- rt[(n.burn+1):N]
  sigma2 <- sigma2[(n.burn+1):N]
  
  # Guardando valores
  return(list(rt = rt, sigma2 = sigma2, rt2 = rt^2, time = 1:n))
}


