require(magrittr); require(numDeriv); require(forecast)
setwd("C:/Users/Gabriel/OneDrive/TCC/Codigos/R/ARCH_GARCH")
source("garch11.R")

########################## MODELO USADO ######################################

# rt = sigma_{t}*z_t
# sigma_{t}^2 = omega + alpha_1 * r_{t-1}^2 + beta_1 * sigma_{t-1}^2

########################## FUNCAO OBJETIVO ###################################

# log(L) = somatorio - 0.5*(log(sigma_{t}^2) - r_{t}^2/sigma_{t}^2)
# Onde f eh a densidade de uma normal padrao, somatoria vai se 1 ate n

##############################################################################

# Likelihood --------------------------------------------------------------
llike_garch <- function(pars, rt){
  omega <- pars[1]
  alpha <- pars[2]
  beta <- pars[3]
  
  n <- length(rt)
  
  # Iniciando sigma{t} com omega(1-beta).
  # Inicio o dizendo o tamanho do vetor para facilitar as contas para R.
  sigma <- c(1, rep(NA, n-1)) 
  
  s <- -.5*(log(sigma[1]^2) + (rt[1]/sigma[1])^2)
  
  for (t in 2:n) {
    sigma[t] <- sqrt(omega + alpha*rt[t-1]^2 + beta*sigma[t-1]^2)
    
    s <- s - .5*(log(sigma[t]^2) + rt[t]^2/sigma[t]^2)
  }
  return(s)
}

######################## Outra forma ########################

# Likelihood exp --------------------------------------------------------------
llike_garch_exp <- function(pars, rt){
  omega <- exp(pars[1])
  alpha <- exp(pars[2])
  beta <- exp(pars[3])
  
  n <- length(rt)
  
  # Inicio o dizendo o tamanho do vetor para facilitar as contas para R.
  sigma <- c(1, rep(NA, n-1)) 
  
  s <- -.5*(log(sigma[1]^2) + (rt[1]/sigma[1])^2)
  
  for (t in 2:n) {
    sigma[t] <- sqrt(omega + alpha*rt[t-1]^2 + beta*sigma[t-1]^2)
    
    s <- s - .5*(log(sigma[t]^2) + rt[t]^2/sigma[t]^2)
  }
  return(s)
}
