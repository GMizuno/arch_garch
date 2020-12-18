########################## MODELO USADO ######################################
# rt = phi0 + phi1*r_{t-1} + eps_t
# eps_t = sigma_{t}*u_t
# sigma_{t}^2 = omega + alpha * eps_{t-1}^2 + beta * sigma_{t-1}^2

########################## FUNCAO OBJETIVO ###################################

# log(L) = somatorio de f(rt[t]), em que f representa a densidade de uma normal 
# com media phi0 + phi1*rt[t-1] e variancia sigma2[t] 

##############################################################################

llike_ar_garch <- function(pars, rt, n)
{
  # Parte AR
  phi0 <- pars[1]
  phi1 <- pars[2]
  
  # Parte GARCH
  omega <- exp(pars[3])
  alpha <- exp(pars[4])
  beta <- exp(pars[5])
  
  # Declarando tamanho do vetor
  epst <- rep(NA, n)
  sigma2t <- rep(NA, n)
  
  # Inicializando rt, epst e sigmat2, estou considerando r0, r-1, ... = 0
  epst[1] <- rt[1] - phi0
  sigma2t[1] <- omega/(1-beta-alpha)
  s <- 0
  
  for (t in 2:n)
  {
    sigma2t[t] <- omega + alpha*epst[t-1]^2 + beta*sigma2t[t-1]
    epst[t] <- rt[t] - phi0 - phi1*rt[t-1]
    s <- s + dnorm(rt[t], mean = phi0 + phi1*rt[t-1], 
                   sd = sqrt(sigma2t[t]), log = TRUE)
  }
  return(s)
}