# n.burn serve para excluir as primeiras simulações devido a inicialização 

# alpha vetor com os paremetros 
# omega intercepto de sigma_{t}^2 

########################## MODELO USADO ######################################

# rt = sigma_{t}*z_t
# sigma_{t}^2 = omega + alpha_1 * r_{t-1}^2 + beta_1 * sigma_{t-1}^2

##############################################################################
# USAR set.seed(1)

garch11 <- function(n, pars, n.burn = 50){
  
  omega <- pars[1]
  alpha1 <- pars[2]
  beta1 <- pars[3]
  
  # Inicizalizando
  N <- n+n.burn
  rt <- numeric(N); sigma2 <- numeric(N)
  r20 <- omega/(1-alpha1-beta1) # Como r_{t}^2 segue em ARMA(1,1) estou colocando a média 
  
  # Armazenando o primeiro valor de sigma2 e rt
  sigma2[1] <- omega + alpha1*r20 # Estou colocando sigma_{0}^2 = 0 
  rt[1] <- sqrt(sigma2[1])*rnorm(1)
  
  # Iniciando a recursao
  for (i in 2:N){
    sigma2[i] <- omega + alpha1*(rt[i-1])^2 + beta1*sigma2[i-1]
    rt[i] <- sqrt(sigma2[i])*rnorm(1)
  }
  
  # Ignorando os valores iniciais passando pelo n.burn
  rt <- rt[(n.burn+1):N]
  sigma2 <- sigma2[(n.burn+1):N]
  
  # Guardando valores
  return(list(rt = rt, sigma2 = sigma2, rt2 = rt^2, time = 1:n))
}

