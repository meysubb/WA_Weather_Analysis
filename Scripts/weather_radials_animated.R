library(ggplot2)
library(viridis)
library(scales)
library(gganimate)
library(highcharter)

prepare_yearly_data <- function(data){
  year_dat <- data %>%
    group_by(year, month) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(newDay = seq(1, length(day))) %>%  # create matching x-axis as historical data
    ungroup() %>% 
    mutate(id = seq(nrow),
           CommonDate = as.Date(paste0("2000-",format(date, "%j")), "%Y-%j"))
  return(year_dat)
}

## Add Degree Symbol
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}
# y-axis
a <- dgr_fmt(seq(-10,115, by=10))

plot_weather_radial <- function(data,facet=FALSE,animate=FALSE){
  if(animate){
    p <- data %>% ggplot(aes(CommonDate,ymin = TMIN,ymax = TMAX,color = TAVG,frame = year)) + 
      geom_linerange(size = 1.3, alpha = 0.75) +
      scale_color_viridis(NULL, option = "A") +
      scale_x_date(labels = function(x) format(x, "%b")) + 
      coord_cartesian(ylim = c(-10,115)) +
      scale_y_continuous(breaks = seq(-10,115, by=10), labels = a) + 
      labs(title = "Temperature Range (F)",
           subtitle = "Tri-Cities,WA",
           caption = "@msubbaiah1",
           x = NULL, y = NULL) +
      theme_minimal()
    
    gganimate(p)
  }else if(facet){
    data %>% ggplot(aes(CommonDate,ymin = TMIN,ymax = TMAX,color = TAVG)) +
      facet_wrap(~year) + 
      geom_linerange(size = 1.3, alpha = 0.75) +
      scale_color_viridis(NULL, option = "A") +
      scale_x_date(labels = function(x) format(x, "%b")) + 
      coord_cartesian(ylim = c(-10,115)) +
      scale_y_continuous(breaks = seq(-10,115, by=10), labels = a) + 
      labs(title = "Temperature Range (F)",
           subtitle = "Tri-Cities,WA",
           caption = "@msubbaiah1",
           x = NULL, y = NULL) +
      theme_minimal()
  }else{
    data %>% ggplot(aes(CommonDate,ymin = TMIN,ymax = TMAX,color = TAVG)) +
      geom_linerange(size = 1.3, alpha = 0.75) +
      scale_color_viridis(NULL, option = "A") +
      scale_x_date(labels = function(x) format(x, "%b")) + 
      coord_cartesian(ylim = c(-10,115)) +
      scale_y_continuous(breaks = seq(-10,115, by=10), labels = a) + 
      labs(title = "Temperature Range (F)",
           subtitle = "Tri-Cities,WA",
           caption = "@msubbaiah1",
           x = NULL, y = NULL) +
      theme_minimal()
  }
}