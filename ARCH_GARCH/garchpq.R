# n.burn serve para excluir as primeiras simulações devido a inicialização 

# alpha vetor com os paremetros 
# omega intercepto de sigma_{t}^2 

########################## MODELO USADO ######################################

# rt = sigma_{t}*z_t
# sigma_{t}^2 = omega + alpha_1 * r_{t-1}^2 + ... + alpha_p * r_{t-p}^2
#               beta_1 * sigma_{t-1}^2 + ... + beta_q * sigma_{t-q}^2

##############################################################################
# USAR set.seed(1)

garchpq <- function(n, alpha, beta, w){ 
  n.burn <- 50; N <- n+n.burn
  
  # Definindo variaveis
  rt <- numeric(N); sigma2 <- numeric(N)
  r20 <- w/(1-sum(alpha)-sum(beta))
  p <- length(alpha); q <- length(beta); m <- max(p, q)
  
  # Inicizalizando
  sigma2[1] <- w + sum(alpha*r20); sigma2
  rt[1] <- sqrt(sigma2[1])*rnorm(1); rt
  if (m>=2){
    for (i in 2:m){
      print('entrou aqui')
      p1 <- alpha*c(rt[1:(i-1)], rep(r20, I(p-i+1))) # primeira parcela
      p2 <- beta*c(sigma2[1:(i-1)], rep(0, I(q-i+1>0))) # segunda parcela
      sigma2[i] <- w + sum(p1) + sum(p2); sigma2
      rt[i] <- sqrt(sigma2[i])*rnorm(1); rt
    } 
  }

  # Iniciando a recursao
  for (i in (m+1):N){
    sigma2[i] <- w + sum(alpha*(rt[i-(1:p)])^2) + sum(beta*sigma2[i-(1:q)])
    rt[i] <- sqrt(sigma2[i])*rnorm(1)
  }
  
  # Ignorando os valores iniciais passando pelo n.burn
  rt <- rt[(n.burn+1):N]
  sigma2 <- sigma2[(n.burn+1):N]
  
  # Guardando valores
  return(list(rt = rt, sigma2 = sigma2, rt2 = rt^2, time = 1:n))
  
}

