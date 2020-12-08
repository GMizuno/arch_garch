require(magrittr); require(numDeriv); require(forecast)
setwd(r"(C:\Users\Gabriel\Desktop\arch_garch\ARCH_GARCH)")

########################## MODELO USADO ######################################

# rt = sigma_{t}*z_t
# sigma_{t}^2 = omega + alpha_1 * r_{t-1}^2 + ... + alpha_p * r_{t-p}^2

########################## FUNCAO OBJETIVO ###################################

# log(L) = somatorio log(f(rt))
# Onde f eh a densidade de uma normal com media 0 e 
# variancia omega + alpha1*r_{t-1}^2 + ... + alphap*r_{t-p}^2

##############################################################################

# Likelihood --------------------------------------------------------------
llike_archp <- function(rt, pars, p, n){
  omega <- exp(pars[1])
  alpha <- exp(pars[-1])
  
  # Inicio o dizendo o tamanho do vetor para facilitar as contas para R.
  sigma2 <- c(rep(1, p), rep(NA, n-p)) # Mudar inicializacao pois sabemos a var cond
  
  for (t in (p+1):n){
    sigma2[t] <- omega + sum(alpha*rt[t-(1:p)]^2)
  }
  
  s <- dnorm(rt, mean = 0, sd = sqrt(sigma2), log = TRUE)
  return(sum(s))
}


