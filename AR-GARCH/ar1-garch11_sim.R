# n.burn serve para excluir as primeiras simulações devido a inicialização 

# alpha vetor com os paremetros 
# omega intercepto de sigma^2_{t}

########################## MODELO USADO ######################################

# y_{t} = phi0 + phi1*y_{t-1} + eps_{t}
# eps_{t} = sigma_{t}*z_t
# sigma_{t}^2 = omega + alpha_1 * eps_{t-1}^2 + beta_1 * sigma_{t-1}^2

##############################################################################

Garch11 <- function(pars, n){
  omega <- pars[1]
  alpha1 <- pars[2]
  beta1 <- pars[3]
  
  n.burn <- 50
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
  return(data.frame(rt = rt, sigma2 = sigma2, rt2 = rt^2))
}

AR1_Garch11 <- function(n, pars){
  # browser()
  
  # Parte AR
  phi0 <- pars[1]
  phi1 <- pars[2]
  
  # Parte GARCH
  omega <- pars[3]
  alpha1 <- pars[4]
  beta1 <- pars[5]
  
  n.burn <- 50
  N <- n + n.burn

  # Armazenando o primeiro valor de AR(1) com a média dele
  rt <- numeric(N); rt[1] <- phi0/(1-phi1)
  
  # Iniciando a recursao
  epst <- Garch11(pars[3:5], N)$rt
  
  for (i in 2:N){
    rt[i] <- phi0 + phi1*rt[i-1] + epst[i]
  }
  
  # Ignorando os valores iniciais passando pelo n.burn
  rt <- rt[(n.burn+1):N]
  epst <- epst[(n.burn+1):N]
  
  # Guardando valores
  return(data.frame(rt = rt, rt2 = rt^2, epst = epst, time = 1:n))
}



