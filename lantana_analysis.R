rm(list =ls())
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)
library(forecast)



phen <- read.csv('collated1.csv',stringsAsFactors = F)
phendat <- gather(phen[,-(2:5)], key = 'phenophase',value = 'value', 3:8)
phendat$value <- factor(phendat$value)
library(plyr)
phendat$value <- mapvalues(phendat$value,from = c('Y','N',0,1,2), to = c(1,0,0,1,1))
detach("package:plyr")
phendat$datep <- as.Date(phendat$datep)
phendat$value <- as.numeric(as.character(phendat$value))

datesdat <- expand.grid(year = 2007:2016,month = 1:12, day = c(1,15), species_id = 1:18, phenophase = unique(phendat$phenophase))
datesdat$datep <- as.Date(paste(datesdat$year,datesdat$month,datesdat$day, sep = '-'))
datesdat <- datesdat %>% filter(datep > '2007-11-30') %>% dplyr::select(datep,species_id,phenophase) 
phendat1 <- phendat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 
phendat1 <- left_join(datesdat,phendat1)
phendat1 <- phendat1 %>% arrange(species_id,phenophase,datep)


wdat <- read.csv('weather_man_aut_day_compiled.csv',stringsAsFactors = F)
wdat$datep <- as.Date(wdat$datep)
wdat <- wdat %>% filter(datep > '2007-01-01')%>% filter(Max.temp<50) %>% group_by(datep) %>% summarise(Max.temp = max(Max.temp,na.rm = T), Min.temp = min(Min.temp,na.rm = T), Rainfall.mm = sum(Rainfall.mm, na.rm = T))



lant <- phendat1 %>% filter(species_id == 12)

lantleaf <- lant %>% filter(phenophase == 'flower_bud')

#write.csv(lant, 'lantana_phenology.csv',row.names = F)

plot(lant$prop[lant$phenophase == 'leaves_new'],typ = 'l')

get_soilmoisture <- function(rain,k,soil_upper_sat = Inf, soil_lower_sat = -Inf){
  sm <- numeric(length = length(rain))
  for(i in 1:(length(rain)-1)){
    smn  <- round((sm[i] + rain[i+1])*exp(-k),2)
    sm[i+1] <- ifelse(smn > soil_upper_sat, soil_upper_sat,smn)
  }
  return(sm)
}

k <- .1
upper_bound <- 75

mod1 <- function(k,upper_bound){
  dat1 <- wdat %>% arrange(as.Date(datep)) %>% filter(datep > '2006-12-31', datep < '2012-04-01')
  dat1$soilmoist <- NA
  dat1$soilmoist <- get_soilmoisture(dat1$Rainfall.mm,k)
  dat1$soilmoist <- ifelse(dat1$soilmoist > upper_bound, upper_bound,dat1$soilmoist)
  dat1$soilmoist <- dat1$soilmoist/upper_bound
  dat2 <- left_join(lantleaf,dat1, by = c("datep"="datep")) %>% mutate(sqerr = (prop-soilmoist)^2) %>% filter(!is.na(soilmoist))
  return(as.numeric(sqrt(sum(dat2$sqerr,na.rm = T)/nrow(dat2))))
}

k1 <- seq(0.2,0.6,length.out = 20)
ub1 <- seq(150,300, length.out = 21)
ssemat <- matrix(NA, nrow = length(k1), ncol = length(ub1))

for(i in 1:length(k1)){
  for(j in 1:length(ub1)){
    print(i)
    ssemat[i,j] <- mod1(k1[i],ub1[j])
  }
}

contour(k1,ub1,ssemat,nlevels = 45)

t <- which(ssemat == min(ssemat),arr.ind = T)
k <- k1[t[1]]
upper_bound = ub1[t[2]]

dat1 <- wdat %>% arrange(as.Date(datep)) %>% filter(datep < '2011-01-01',datep> '2007-01-01') 
dat1$soilmoist <- NA
dat1$soilmoist <- get_soilmoisture(dat1$Rainfall.mm,k,upper_bound)
dat1$soilmoist[dat1$soilmoist > upper_bound] <- upper_bound
dat1$soilmoist <- dat1$soilmoist/upper_bound
dat2 <- left_join(lantleaf,dat1) 

plot(dat2$prop~dat2$datep, typ = 'l')
lines(dat2$soilmoist~dat2$datep, col = 'red')


dat1 <- wdat %>% arrange(as.Date(datep)) %>% filter(datep> '2007-01-01') 
dat1$soilmoist <- NA
dat1$soilmoist <- get_soilmoisture(dat1$Rainfall.mm,k,upper_bound)
dat1$soilmoist[dat1$soilmoist > upper_bound] <- upper_bound
dat1$soilmoist <- dat1$soilmoist/upper_bound
dat2 <- left_join(lantleaf,dat1) 

plot(dat2$prop~dat2$datep, typ = 'l')

lines(dat2$soilmoist~dat2$datep, col = 'red')

plot(dat2$prop~dat2$soilmoist)

t <- lm(dat2$soilmoist~dat2$prop)

summary(t)

########## differentiate between soil moisture capacity and upper cut off for flushing (2 upper bounds) also incorporate lower bound for phenophase trigger

leafts <- ts(lant$prop[lant$phenophase == 'leaves_old'], start = c(2007,23), frequency = 24)

leafts <- na.interp(leafts)

decleafts <- decompose(leafts)

leafns <- decleafts$trend + decleafts$random

plot(leafns)


leaf <- lant %>% filter(phenophase == 'flower_bud') %>% mutate(fort = paste(month(datep),day(datep), sep = '-'))

leafave <- leaf %>% group_by(fort) %>% summarise(aveprop = mean(prop,na.rm = T))

leaf <- left_join(leaf,leafave)

leaf <- leaf %>% mutate(dev = prop-aveprop)

plot(leaf$dev~leaf$datep, typ = 'l')
par(new = T)
plot(leafns, col = 'red')



wdat1 <- wdat %>% mutate(fort = paste(month(datep),day(datep), sep = '-')) 

wdat1$Max.temp <- as.numeric(wdat1$Max.temp)
wdat1$Min.temp <- as.numeric(wdat1$Min.temp)

wdatseas <- wdat1 %>% group_by(fort) %>% summarise(seasmaxt = mean(Max.temp,na.rm = T), seasmint = mean(Min.temp,na.rm = T), seasrain = mean(Rainfall.mm,na.rm = T))

wdat1 <- left_join(wdat1,wdatseas)

wdat1 <- wdat1 %>% mutate(maxtns = Max.temp - seasmaxt, mintns = Min.temp - seasmint, rainns = Rainfall.mm - seasrain)

wdat1 <- wdat1 %>% filter(datep > '2007-11-25')

plot(wdat1$mintns~wdat1$datep, typ = 'l')

leaf <- left_join(leaf,wdat1)



leaf$year <- year(leaf$datep)

t <- leaf$datep[3:26]
fort <- paste(month(t),day(t),sep = '-')
fort <- factor(fort, levels = fort)

leaf$fort <- factor(leaf$fort, levels = fort)

ggplot() + geom_point(data = leaf, aes(x = fort,y = dev,col = factor(year)))

leaf1 <- leaf %>% filter(month(datep) < 7)

ggplot() + geom_point(data = leaf1, aes(x = fort,y = dev,col = factor(year)))

plot(leaf$prop~leaf$datep, typ = 'l')
plot(leaf$dev~leaf$Min.temp)

summary(lm(leaf1$dev~leaf1$mintns*leaf1$fort))


plot(leaf$prop~leaf$datep, typ = 'l')

