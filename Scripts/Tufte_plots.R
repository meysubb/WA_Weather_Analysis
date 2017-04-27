library(tidyverse)
library(ggplot2)
library(ggthemes)

prepare_past_data <- function(data){
  ret_dat <- data %>%
    group_by(year) %>%
    mutate(newDay = seq(1, length(day))) %>%   # label days as 1:365 (will represent x-axis)         
    ungroup() %>%
    group_by(newDay) %>%
    mutate(upper = max(TMAX,na.rm=TRUE), # identify max value for each day
           lower = min(TMIN,na.rm=TRUE), # identify min value for each day
           avg = mean(TAVG,na.rm=TRUE),  # calculate mean value for each day
           se_max = sd(TMAX,na.rm=TRUE)/sqrt(length(TMAX)),
           se_low = sd(TMIN,na.rm=TRUE)/sqrt(length(TMIN))) %>%  # calculate standard error of mean
    mutate(avg_upper = avg+(2.101*se_max),  # calculate 95% CI for mean
           avg_lower = avg-(2.101*se_low)) %>%  # calculate 95% CI for mean
    ungroup()
  
  return(ret_dat)
}

prepare_current_dat <- function(data,yr){
  ret_data <- tri_cities_weather %>% filter(year == yr) %>%
    group_by(year, month) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(newDay = seq(1, length(day)),
           avg = TAVG) %>%  # create matching x-axis as historical data
    ungroup()
}

find_hilo_dat <- function(dat,opt="lows"){
  if(opt == "lows"){
    ret_dat <- dat %>%
      group_by(newDay) %>%
      summarise(Pastlow = min(TMIN,na.rm=TRUE)) 
  }else{
    ret_dat <- dat %>%
      group_by(newDay) %>%
      summarise(Pasthigh = max(TMAX,na.rm=TRUE)) 
  }
}


### Prepare Data
historical_data <- tri_cities_weather %>% filter(year >= 1998)
Past <- prepare_past_data(historical_data)
Present <- prepare_current_dat(tri_cities_weather,2016)
PastLows <- find_hilo_dat(Past)
PastHighs <- find_hilo_dat(Past,"hi")


# Lower than Historical Data
PresentLows <- Present %>%
  left_join(PastLows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(TMIN<=Pastlow, "Y", "N")) %>% 
  filter(record == "Y")  

# Higher than Historical Data
PresentHighs <- Present %>%
  left_join(PastHighs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(TMAX>=Pasthigh, "Y", "N")) %>% 
  filter(record == "Y")  

## Add Degree Symbol
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}
# y-axis
a <- dgr_fmt(seq(-20,115, by=10))

# Legend Symbol. 
legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))
# Month Line
month_lines <- data_frame(time = c(0,31,59,90,120,151,181,212,243,273,304,334,365))
# Annotations
coldest <- paste0("There were ", nrow(PresentLows) ," days where the temperature was at or")
warm <- paste0("There were ", nrow(PresentHighs), " days where the temperature was at or")