require(forecast); require(ggplot2); require(glue)

tema <- theme(
  panel.background = element_rect(fill = "white"), 
  panel.grid.major.y = element_line(linetype = "solid", colour = 'grey'),
  panel.grid.major.x = element_line(linetype = "solid", colour = 'grey'),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
)


line <- function(data)
{
  rt <- data[['rt']]
  time <- data[['time']]
  
  ggplot(data, aes(x = time, y = rt) ) + 
    geom_line(size = 0.52, colour = "#000000") +
    theme_minimal() + 
    ggtitle("AR(1)-GARCH(1,1)") +
    ylab(bquote( ~ r[t])) + xlab("Tempo") +
    theme(axis.title.y = element_text(angle = 0)) + tema
}

acf_plot <- function(data)
{
  acf(data, plot = F) %>% autoplot() + 
    ggtitle("AR(1)-GARCH(1,1)") + ylim(c(-1,1))
}

pacf_plot <- function(data)
{
  pacf(data, plot = F) %>% autoplot() + 
    ggtitle("AR(1)-GARCH(1,1)") + ylim(c(-1,1))
}

histo <- function(data, var, M, n)
{
  ggplot(data, aes_string(x = var)) + geom_histogram(fill = "#0c4c8a") +
    theme_minimal() + 
    labs(y = '', x = glue('Estimadores de {var}.'),
         title = glue("Histograma do {var}, com M = {M} e n = {n}")) + 
    theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
}

QQplot <- function(data, var, M, n)
{
  ggplot(data, aes_string(sample = var)) + 
    stat_qq() + 
    geom_abline(slope = 1, intercept = 0) + 
    tema +
    labs(x = 'Quantil Teorico', y = 'Quantil Amostral', 
         title = glue("QQplot do {var}, com M = {M} e n = {n}"))
}

