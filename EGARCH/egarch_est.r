########################## MODELO USADO ######################################

# rt = sigma_{t}*u_t
# log(sigma_{t}^2) = omega + beta*log(sigma_{t-1}^2) +
#                  + alpha*abs(r_{t-1}/sigma_{t-1} - \sqrt(\pi/2)) 
#                  + gamma*r_{t-1}/sigma_{t-1}

########################## FUNCAO OBJETIVO ###################################

# log(L) = somatorio -log(sigma_{t}) + f(z_{t}/sigma_{t})
# Onde f eh a densidade de uma normal padrao, somatoria vai se 1 ate n

##############################################################################

# Likelihood --------------------------------------------------------------
llike_egarch <- function(pars, rt, n){
  w <- pars[1]
  alpha  <- pars[2]
  beta <- pars[3]
  gamma  <- pars[4]
  
  # Iniciando sigma{t} com 1.
  # Inicio o dizendo o tamanho do vetor para facilitar as contas para R.
  Sigma <- c(1, rep(NA, n-1)) 
  
  # Inicializando o somatorio da loglike
  
  for (i in 2:n){
    Sigma[i] <-  sqrt(exp(w + beta*log(Sigma[i-1]^2) + 
                            alpha*abs((rt[i-1]/Sigma[i-1]) - sqrt(2/pi)) + 
                            (gamma*rt[i-1]/Sigma[i-1])))
  }
  # dnorm eh uma funcao do R para avaliar a densidade da norma, se nao passar
  # os parametros por deafualt ele faz uma normal padrao
  # o parametro log = TRUE, aplica log na densidade
  s <- sum(-log(Sigma) + dnorm(rt/Sigma, log = TRUE))
  
  return(-s)
}

# Hessian -----------------------------------------------------------------



