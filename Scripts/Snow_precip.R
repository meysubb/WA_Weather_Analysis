library(lubridate)

prepare_monthly_data <- function(data){
  monthly_data <- data %>% mutate(
    date = as.Date(paste(year, month, day,sep="-"), "%Y-%m-%d")) %>% filter(!(is.na(date))) %>% 
    group_by(year,month) %>% 
    summarise(SNOW = sum(SNOW,na.rm=TRUE)) %>% 
    mutate(
      month_name = month(month,label=TRUE,abbr=FALSE)) 
  
  monthly_data$SNOW[monthly_data$SNOW == 0] <- NA
  monthly_data$year <- as.factor(monthly_data$year)
  return(monthly_data)
}

prepare_snow_heat_map <- function(plot_data){
  require(ggthemes)
  require(ggplot2)
  require(viridis)
  
  plot_data %>% ggplot(aes(year, month_name)) +
    geom_tile(aes(fill=SNOW), color="#b2b2b2", size=0.15) + 
    scale_x_discrete(breaks = levels(plot_data$year)[c(T, rep(F, 5))],expand=c(0,0.15), position="top") +
    scale_fill_viridis(option="B",name = "Total (in)", na.value="white") + 
    scale_y_discrete(name="", limits = rev(levels(plot_data$month_name))) + 
    guides(fill=guide_colourbar(label.position = "top", direction = "horizontal", title.vjust = 0)) +
    ## Add Themes
    theme_tufte() + 
    theme(legend.title = element_text(size=10)) +
    theme(legend.key.height = unit(0.5, "lines")) +
    theme(legend.position = 'bottom') + 
    ## Add Captions
    labs(x=NULL, y=NULL, title="Monthly Snowfall per Year",
         subtitle="Tri-Cities, WA",
         caption = "@msubbaiah1") + 
    ## Fixed Aspect Ratio 
    coord_fixed() +
    theme(axis.ticks = element_blank())
}  

