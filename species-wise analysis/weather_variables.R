library(tidyverse)
library(suncalc)
library(lubridate)


wdat <- read.csv('weather_man_aut_day_compiled.csv',stringsAsFactors = F)
wdat <- wdat %>% filter(Date > '2007-01-01')
wdat$Date <- as.Date(wdat$Date)
wdat <- wdat %>% arrange(Date) %>% filter(Max.temp < 50)
dates_df <- data.frame(Date = seq.Date(as.Date('2007-01-02'),as.Date('2017-08-01'), by = 1))
wdat <- left_join(dates_df,wdat)
wdat$Rainfall.mm <- as.numeric(wdat$Rainfall.mm)
wdat$Min.temp <- as.numeric(wdat$Min.temp)
wdat$Max.temp <- as.numeric(wdat$Max.temp)

rain <- wdat$Rainfall.mm
temp <- (wdat$Min.temp + wdat$Max.temp)/2
mintemp <- wdat$Min.temp
maxtemp <- wdat$Max.temp

plot(rain~temp)
plot(temp~mintemp)
plot(temp~maxtemp)
plot(mintemp~maxtemp)


sundat <- getSunlightTimes(date = wdat$Date, lat = 13.64, lon = 78.45, tz = 'Asia/Calcutta')

wdat$sunrise <- hour(sundat$sunrise)*60 + minute(sundat$sunrise) + second(sundat$sunrise)/60

wdat$sunset <- hour(sundat$sunset)*60 + minute(sundat$sunset)+ second(sundat$sunset)/60


wdat$photoperiod <- wdat$sunset - wdat$sunrise

plot(wdat$sunrise/60~wdat$Date, typ = 'l', ylim = c(0,20), col = 'blue')
lines(wdat$sunset/60~wdat$Date, typ = 'l', col = 'red')
lines((wdat$sunset-wdat$sunrise)/60~wdat$Date, col = 'black')  

with(wdat,plot(sunset~mintemp))
  
write.csv(wdat,'weather_photoperiod.csv',row.names = F)
