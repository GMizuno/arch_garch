require(forecast)
line <- function(data, pars){
  rt <- data[['rt']]
  time <- data[['time']]
  
  ggplot(data, aes(x = time, y = rt) ) + 
    geom_line(size = 0.52, colour = "#000000") +
    theme_minimal() + 
    ggtitle(subtitle = "egarch(1,1)",
            bquote("Parâmetros:" ~ omega == .(pars[1]) ~
                     ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~
                     ', ' ~ gamma == .(pars[4])
            )
    ) +
    ylab(bquote( ~ r[t])) + xlab("Tempo") +
    theme(axis.title.y = element_text(angle = 0))
}

acf_plot <- function(data, pars, quad = FALSE){
  rt <- data[['rt']]
  rt2 <- data[['rt2']]
  
  if (quad == FALSE){
    acf(rt, plot = F) %>% autoplot() + 
      ggtitle(subtitle = "egarch(1,1)", 
              bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                       ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                       ', ' ~ gamma == .(pars[4]))) + 
      ylim(c(-1,1))
  } else {
    acf(rt2, plot = F) %>% autoplot() + 
      ggtitle(subtitle = "egarch(1,1)-quadrado do retrono", 
              bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                       ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                       ', ' ~ gamma == .(pars[4]))) + 
      ylim(c(-1,1))
  }
  
  
}

pacf_plot <- function(data, pars, quad = FALSE){
  rt <- data[['rt']]
  rt2 <- data[['rt2']]
  
  if (quad == FALSE){
    pacf(rt, plot = F) %>% autoplot() + 
      ggtitle(subtitle = "egarch(1,1)", 
              bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                       ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                       ', ' ~ gamma == .(pars[4]))) + 
      ylim(c(-1,1))
  } else {
    pacf(rt2, plot = F) %>% autoplot() + 
      ggtitle(subtitle = "egarch(1,1)-quadrado do retrono", 
              bquote("Parâmetros:" ~ omega == .(pars[1]) ~ 
                       ", " ~ alpha == .(pars[2]) ~ ', ' ~ beta == .(pars[3]) ~ 
                       ', ' ~ gamma == .(pars[4]))) + 
      ylim(c(-1,1))
  }
  

}

