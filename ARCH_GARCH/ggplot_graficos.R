require(forecast)

line <- function(data, title)
{
  rt <- data[['rt']]
  time <- data[['time']]
  
  ggplot(data, aes(x = time, y = rt) ) + 
    geom_line(size = 0.52, colour = "#000000") +
    theme_minimal() +  ggtitle(title) +
    ylab(bquote( ~ r[t])) + xlab("Tempo") +
    theme(axis.title.y = element_text(angle = 0))
}

acf_plot <- function(data, title)
{
  acf(data, plot = F) %>% autoplot() + ggtitle(title) + ylim(c(-1,1))
}

pacf_plot <- function(data, title)
{
  pacf(data, plot = F) %>% autoplot() + ggtitle(title) + ylim(c(-1,1))
}

histo <- function(data, par)
{
  ggplot(data, aes(x = par)) + geom_histogram(bins = 30L, fill = "#0c4c8a") +
    theme_minimal() + 
    ggtitle(label = bquote('Convergência do estimador de' ~ par)) +
    theme(axis.title.y = element_text(angle=0, size = 15, vjust = .6))
}

