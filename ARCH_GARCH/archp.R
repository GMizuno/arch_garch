# n.burn serve para excluir as primeiras simulações devido a inicialização 

# alpha vetor com os paremetros 
# omega intercepto de sigma^2_{t}

########################## MODELO USADO ######################################

# rt = sigma_{t}*z_t
# sigma_{t}^2 = omega + alpha_1 * r_{t-1}^2 + ... + alpha_p * r_{t-p}^2

##############################################################################

# USAR set.seed(1)

archp <- function(n, alpha, w, n.burn = 50){
  # Inicilizando
  N <- n+n.burn
  rt <- numeric(N); sigma2 <- numeric(N)
  p <- length(alpha)
  r20 <- w/(1-sum(alpha)) # Como r_{t}^2 segue em AR(p) estou colocando a média
  
  # Armazenando o primeiro valor de sigma2 e rt
  sigma2[1:p] <- w + alpha*r20
  rt[1:p] <- sqrt(sigma2[1:p])*rnorm(p) # Função rnorm gera um norma padrao
  
  # Iniciando a recursao
  for (i in (p+1):N){
    sigma2[i] <- w + sum(alpha*rt[i-(1:p)]^2) # parte i-(1:p) equivale a (i-1):(i-p)
    rt[i] <- sqrt(sigma2[i])*rnorm(1)
  }
  
  # Ignorando os valores iniciais passando pelo n.burn
  rt <- rt[(n.burn+1):N]
  sigma2 <- sigma2[(n.burn+1):N]
  
  # Guardando valores
  return(list(rt = rt, sigma2 = sigma2, rt2 = rt^2, time = 1:n))
}



