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


histo <- function(data, pars){
  ggplot(data, aes(x = gamma)) + geom_histogram(bins = 30L, fill = "#0c4c8a") +
    theme_minimal() + 
    ggtitle(label = bquote('Convergência do estimador de' ~ gamma)) +
    ylab(bquote(hat(gamma))) + xlab("i-esima amostra") + 
    theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
}

line_gamma <- function(data, pars){
  ggplot(data, aes(x = seq_along(gamma), y = gamma)) +
    geom_line(size = .8, colour = "#0c4c8a") +
    geom_hline(aes(yintercept = pars[4]), col = 'red', linetype = "dashed") + 
    theme_minimal() +  
    ggtitle(label = bquote('Convergência do estimador de' ~ gamma), 
            subtitle = bquote("Verdadeiro parâmetro gama" ~ 
                                gamma == .(pars[4]))) +
    ylab(bquote(hat(gamma))) + xlab("i-esima amostra") + 
    theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
}