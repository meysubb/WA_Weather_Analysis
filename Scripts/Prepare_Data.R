library(lubridate)
library(dplyr)
library(tidyr)
library(stringi)

options(stringsAsFactors = FALSE)
### Load Pasco Data 
### Choppy Snow Data
pasco_weather <- read.csv("Data/893151.csv")
pasco_weather[pasco_weather==-9999]<-NA
pasco_weather[,c(4:5,7:12)] <- sapply(pasco_weather[,c(4:5,7:12)],as.numeric)

pasco_weather <- pasco_weather %>% mutate(year = year(ymd(DATE)),
                                    month = month(ymd(DATE)),
                                    day = day(ymd(DATE))) %>% select(STATION_NAME,year,month,day,PRCP,
                                                                     SNOW,TAVG,TMAX,TMIN,LATITUDE,LONGITUDE) %>%
  group_by(year, month, day) %>%
  mutate(TAVG = mean(c(TMAX, TMIN), na.rm = TRUE))

pasco_weather_to_merge <- pasco_weather %>% filter(year>=2014) %>% mutate(
  date = as.Date(paste(year, month, day,sep="-"), "%Y-%m-%d")
) %>% select(-c(LATITUDE,LONGITUDE)) %>% ungroup()


### Station 
stations <-  read_fwf(file = "Data/stations.txt",
                      col_positions = fwf_widths(c(6, 9, 10, 7, 3, 31, 7, 7, 7, 3),
                                                 col_names = c("coop_id", "latitude", "longitude",
                                                               "elevation", "state", "name",
                                                               "component_1", "component_2",
                                                               "component_3", "utc_offset")),
                      col_types = "cdddcccccc") 

closestStation <- function(stations, lat, lon, restrict_to = NULL) {
  if (!is.null(restrict_to)) stations <- filter(stations, state == restrict_to)
  index <- which.min(sqrt((stations$latitude-lat)^2 +
                            (stations$longitude-lon)^2))
  stations[index,]
}


tri_cities_id <- closestStation(stations, 46.2396, -119.1006, restrict_to="WA")

### Load Kennewick Data
daily_wx <- read_fwf(file = "Data/state45_WA.txt",
                     col_positions = fwf_widths(c(6, 4, 2, 4, rep(c(5, 1, 1, 1), 31)),
                                                col_names = c("coop_id", "year", "month", "element",
                                                              flatten_chr(map(1:31, ~paste("r_", c("v", "fm", "fq", "fs"),
                                                                                           .x, sep=""))))),
                     col_types = paste0("ciic", paste0(rep("iccc", 31), collapse=""), collapse=""),
                     na = c("", "NA", "-", "-9999")) %>%
  gather(day, value, starts_with("r_v")) %>%
  select(-starts_with("r_")) %>%
  mutate(day = as.numeric(stri_replace_first_fixed(day, "r_v", ""))) %>%
  mutate(date = sprintf("%s-%02d-%02d", year, month, day)) 

## Coop_id for Tri-Cities: 454154
kennewick_data <- daily_wx %>% filter(coop_id == tri_cities_id$coop_id) %>% spread(element,value) %>% 
  mutate(STATION_NAME = tri_cities_id$name,
         SNOW = SNOW / 10,
         PRCP = PRCP / 100,
         date = as.Date(paste(year, month, day,sep="-"), "%Y-%m-%d")) %>% filter(year>=1905 & year < 2014) %>%  group_by(year, month, day) %>%
  mutate(TAVG = mean(c(TMAX, TMIN), na.rm = TRUE)) %>% select(-c(SNWD,coop_id)) %>% ungroup()

kennewick_data_to_merge <- kennewick_data %>% select(STATION_NAME,year,month,day,PRCP,SNOW,TAVG,TMAX,TMIN,date)


### Merged Data 
tri_cities_weather <- rbind(kennewick_data_to_merge,pasco_weather_to_merge) %>% 
  filter(!is.na(date))

saveRDS(tri_cities_weather,"Data/TC_WA_weather.RDS")




