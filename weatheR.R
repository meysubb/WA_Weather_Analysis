### Note: This is not cleaned up
### Some of the scripts prep and plot the data
### The Tufte script only preps data, a function will be created to plot this. 
### Look for updates soon. 
### This code is just dumped from the RMD File that created the blog post at 
### http://meysubb.github.io/rstats/2017/04/23/weatheR.html

options(stringsAsFactors = FALSE)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
tri_cities_weather <- readRDS("Data/TC_WA_weather.RDS")


source("Scripts/weather_radials_animated.R")

### First Plot
data <- tri_cities_weather %>% filter(year >= 2013 & year < 2017)
df <- data %>% prepare_yearly_data()
plot_weather_radial(df,facet=TRUE)

### Delta T Plot
monthly_averages <- data %>%
  group_by(year,month) %>% 
  summarise(AVG_TMAX = mean(TMAX,na.rm = TRUE),
            AVG_TMIN = mean(TMIN,na.rm = TRUE),
            Temp_Diff = mean(TMAX-TMIN,na.rm=TRUE))

monthly_averages <- monthly_averages[with(monthly_averages, order(year, month)),]

monthly_averages %>% ggplot(aes(x=month,y=Temp_Diff,color=Temp_Diff)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("Jan", "Feb", "Mar", "Apr",
                                "May", "Jun", "Jul", "Aug", "Sept",
                                "Oct", "Nov", "Dec")) +  
  scale_color_viridis("Temp (F)", option = "A") +
  facet_wrap(~year,scales = "free_x") + 
  labs(title = expression(paste(Delta,"T"),sep=""),
       subtitle = "Tri-Cities,WA",
       caption = "@msubbaiah1",
       x = NULL, y = "Temp Diff") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Tufte Themed Plot
#Script Prepares Data, legend, and annotations
#Script can be adapted so it can be used for different year's
source("Scripts/Tufte_plots.R")
ggplot(Past, aes(newDay, avg)) +
  geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1) + 
  geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4",alpha=.4) + 
  geom_linerange(Present,mapping=aes(x=newDay,ymin=TMIN,ymax=TMAX,group=1),colour = "red4") + 
  geom_vline(data = month_lines,aes(xintercept = time),colour = "wheat4",size=1,alpha=0.2) +
  coord_cartesian(ylim = c(-20,115)) +
  scale_y_continuous(breaks = seq(-20,115, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December")) + 
  ### Add Titles
  labs(title = "Tri Cities (WA) Weather in 2016",
       x="",
       y="") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 25, y = 110, label = "Temperature (F)", size=4, fontface="bold") + 
  annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "wheat2", size=3) +
  annotate("segment", x = 181, xend = 181, y = 12, yend = 18, colour = "wheat4", size=3) +
  ### Add Data Source
  annotate("text", x = 40, y = 106, 
           label = "Data accessible back to January 1, 1998.", size=3, colour="gray30") +
  annotate("text", x = 40, y = 102, 
           label = "Data Source: NOAA and CDIAC (ORNL).", size=3, colour="gray30") +
  ### Add Legend
  geom_line(data=legend_data, aes(x=x,y=y),colour="red4") +
  annotate("segment", x = 183, xend = 185, y = 17.7, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, colour = "wheat4", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 12.2, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("text", x = 196, y = 14.75, label = "NORMAL RANGE", size=2, colour="gray30") +
  annotate("text", x = 162, y = 14.75, label = "2016 TEMPERATURE", size=2, colour="gray30") +
  annotate("text", x = 193, y = 25, label = "RECORD HIGH", size=2, colour="gray30") +
  annotate("text", x = 193, y = 5, label = "RECORD LOW", size=2, colour="gray30") + 
  ### Add Text Annotations
  geom_point(data=PresentLows, aes(x=newDay, y=TMIN), colour="blue3") +
  geom_point(data=PresentHighs, aes(x=newDay, y=TMAX), colour="firebrick3") + 
  annotate("segment", x = 88, xend = 95, y = 28, yend = 21, colour = "blue3") +
  annotate("text", x = 105, y = 18, label = coldest, size=3, colour="blue3") +
  annotate("text", x = 100, y = 14, label = "below the previous record (up to 1998)", size=3, colour="blue3") +
  annotate("segment", x = 234, xend = 240, y = 100, yend = 105, colour = "firebrick3") +
  annotate("text", x = 260, y = 112, label = warm, size=3, colour="firebrick3") +
  annotate("text", x = 255, y = 108, label = "above the previous record (up to 1998)", size=3, colour="firebrick3") + 
  theme_tufte()


### Long Snow Plot
source("Scripts/Snow_precip.R")
monthly_data <- tri_cities_weather %>% filter(year > 1975) %>% prepare_monthly_data()
prepare_snow_heat_map(monthly_data) 

## Short Snow Plot
monthly_data <- tri_cities_weather %>% filter(year > 2005) %>% prepare_monthly_data()
prepare_snow_heat_map(monthly_data) 

## Seasonal Snowfall 
monthly_data <- tri_cities_weather %>% filter(year > 1975) %>% filter(!(year > 1984 & year < 2004)) %>% prepare_monthly_data() 
monthly_data$year <- as.numeric(levels(monthly_data$year))[monthly_data$year] 

seasonal_data <-  monthly_data %>% ungroup() %>% group_by(
  season = match(month_name, month.name) %% 12 %/% 3,
  year = ifelse(month_name == "December", year + 1, year)
) %>%
  ungroup() %>%
  # summer, autumn, winter and spring
  mutate(season = factor(
    season,
    levels = c(0, 1, 2, 3),
    labels = c("WIN", "SPR", "SUM", "FAL")
  )) %>%
  filter(season %in% c("FAL", "WIN")) %>% group_by(year, season) %>%
  summarise(value = sum(SNOW, na.rm = TRUE))

plot_dat <- seasonal_data %>%
  group_by(year) %>%
  filter(sum(value) > 0) 

sum <- seasonal_data %>% group_by(year) %>% summarise(total = sum(value,na.rm=TRUE))

plot_dat %>% 
  ggplot(aes(season,value)) + 
  geom_bar(stat = "identity",fill = "#3399FF") + 
  facet_wrap(~year) + 
  labs(x=NULL, y="Total Inches", title="Seasonal Snowfall per Year",
       subtitle="Tri-Cities, WA",
       caption = "@msubbaiah1") + 
  theme_minimal() + 
  geom_text(data=sum,aes(x=1.5,y=20,label=paste0("Total (in): ", total))) 


### Precipitation Plot 
## Prepare Data
dat <- tri_cities_weather %>% filter(year > 1975) %>% 
  group_by(year) %>%
  mutate(newDay = seq(1, length(day))) %>% ungroup()

yearly_data <- dat %>% 
  group_by(year) %>% 
  summarise(PRP = sum(PRCP,na.rm=TRUE))
## Plot
yearly_data %>% ggplot(aes(x=year,y=PRP)) + 
  geom_bar(stat='identity',fill = "#3399FF",alpha=0.5) + 
  scale_y_reverse() + 
  theme_minimal() + 
  labs(title = "Total Rainfall per Year",
       x="",
       y="(in)",
       subtitle = "Tri Cities (WA)",
       caption = "@msubbaiah1")

## Cumulative Sum Rainfall Plot
past_data <- dat %>% filter(year>1998 & year < 2016) %>% group_by(newDay) %>% 
  summarise(avg_rainfall = mean(PRCP,na.rm=TRUE))

past_data <- past_data %>% 
  mutate(cum_rainfall = cumsum(avg_rainfall))

current_data <- dat %>% filter(year == 2016) %>% select(newDay,PRCP) %>% 
  mutate(cum_rainfall = cumsum(PRCP))

month_lines <- data_frame(time = c(0,31,59,90,120,151,181,212,243,273,304,334,365))

ggplot() + 
  geom_step(data = past_data,aes(x=newDay,y=cum_rainfall),color="black") + 
  geom_step(data = current_data,aes(x=newDay,y=cum_rainfall),color="blue") + 
  geom_text(aes(x = 200, y = 2, label = "Average Precipitation (1998-2016)"), color = "black") + 
  geom_text(aes(x = 245, y = 6, label = "2016 Precipitation"), color = "blue") + 
  geom_text(aes(x=100,y=10,label="2016 rainfall ranks 4 out of 19"),color="blue") +
  geom_vline(data = month_lines,aes(xintercept = time),colour = "wheat4",size=1,alpha=0.2) +
  theme_tufte() + 
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(15, 45, 75, 105, 135, 165, 195, 228, 258, 288, 320, 350),
    labels = c("January","February","March","April","May","June",
               "July","August","September","October","November","December")) + 
  labs(title = "Cumulative Precipitation",
       x="",
       y="(in)",
       caption = "@msubbaiah1") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) 