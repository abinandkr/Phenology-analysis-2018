library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

lantana <- read.csv('C:/Users/Abinand Reddy/Desktop/RV Phenology/Analysis/collated1.csv', stringsAsFactors =  F)
lantana <- lantana %>% filter(species_id == 12) 
phendat <- gather(lantana[,-(2:5)], key = 'phenophase',value = 'value', 3:8)
phendat$value <- factor(phendat$value)
library(plyr)
phendat$value <- mapvalues(phendat$value,from = c('Y','N',0,1,2), to = c(1,0,0,1,1))
detach("package:plyr")
phendat$value <- as.numeric(as.character(phendat$value))
phendat1 <- phendat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n*100) 
lantleaf <- phendat1 %>% filter(phenophase == 'leaves_new', datep > '2007-12-31', datep < '2017-01-01') 
lantleaf$datep <- as.Date(lantleaf$datep)
plot(lantleaf$prop~lantleaf$datep, typ = 'l')


dat <- read.csv('C:/Users/Abinand Reddy/Desktop/RV Phenology/Analysis/rvs weather 2014.csv', stringsAsFactors = F)
dat$Rainfall.mm <- as.numeric(dat$Rainfall.mm)
dat[dat == ''] <- NA
dat$Rainfall.mm[is.na(dat$Rainfall.mm)] <- 0
dat$Date <- as.Date(dat$Date,format = '%d.%m.%Y')
dat$datep <- ifelse(day(dat$Date) < 15, paste(year(dat$Date),month(dat$Date),'01',sep = '-'),paste(year(dat$Date),month(dat$Date),'15',sep = '-'))
dat$datep <- as.Date(dat$datep)
dat <- dat %>% filter(Date > as.Date('2007-01-01')) %>% select(Date,Rainfall.mm,datep)



wdat <- read.csv('C:/Users/Abinand Reddy/Desktop/RV Phenology/Analysis/August 2017 compiled.csv',stringsAsFactors = F)
wdat$Date <- as.Date(wdat$Date)
wdat <- wdat %>% filter(Date > max(dat$Date)) %>% group_by(Date) %>% summarise(Rainfall.mm = sum(Rain.mm,na.rm = T))
wdat$datep <- ifelse(day(wdat$Date) < 15, paste(year(wdat$Date),month(wdat$Date),'01',sep = '-'),paste(year(wdat$Date),month(wdat$Date),'15',sep = '-'))
dat <- rbind(dat,wdat)


get_soilmoisture <- function(rain,k,soilsat = Inf){
  sm <- numeric(length = length(rain))
  for(i in 1:(length(rain)-1)){
  smn  <- round((sm[i] + rain[i+1])*exp(-k),2)
  sm[i+1] <- ifelse(smn > soilsat, soilsat,smn)
  }
  return(sm)
}

k <- .1
upper_bound <- 75

mod1 <- function(k,upper_bound){
  dat1 <- dat %>% arrange(as.Date(datep)) %>% filter(datep > '2006-12-31', datep < '2012-04-01')
  dat1$soilmoist <- NA
  dat1$soilmoist <- get_soilmoisture(dat1$Rainfall.mm,k)
  dat1$soilmoist[dat1$soilmoist > upper_bound] <- upper_bound
  dat1$soilmoist <- dat1$soilmoist*100/upper_bound
  dat2 <- left_join(lantleaf,dat1, by = c("datep"="datep")) %>% mutate(sqerr = (prop-soilmoist)^2) %>% filter(!is.na(soilmoist))
  return(as.numeric(sqrt(sum(dat2$sqerr)/nrow(dat2))))
}

mod2 <- function(k,upper_bound, soil_saturation){
  dat1 <- dat %>% arrange(as.Date(datep)) %>% filter(datep > '2008-12-31', datep < '2012-04-01') %>% mutate(soilmoist = NA)
  dat1$soilmoist <- get_soilmoisture(dat1$Rainfall.mm,k,soil_saturation)
  dat1$soilmoist[dat1$soilmoist > upper_bound] <- upper_bound
  dat1$soilmoist <- dat1$soilmoist*100/upper_bound
  dat1 <- dat1 %>% group_by(datep) %>% summarise(soilm = mean(soilmoist,na.rm = T)) %>% arrange(datep)
  dat2 <- left_join(lantleaf,dat1, by = c("datep"="datep")) %>% mutate(sqerr = (prop-soilm)^2) %>% filter(!is.na(soilm))
  return(as.numeric(sqrt(sum(dat2$sqerr)/nrow(dat2))))
}


k1 <- seq(0.025,.035,length.out = 20)
ub1 <- seq(60,70, length.out = 21)
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

dat1 <- dat %>% arrange(as.Date(datep)) %>% filter(datep < '2011-01-01',datep> '2007-01-01') 
dat1$soilmoist <- NA
dat1$soilmoist <- get_soilmoisture(dat1$Rainfall.mm,k1[t[1]])
dat1$soilmoist[dat1$soilmoist > upper_bound] <- upper_bound
dat1$soilmoist <- dat1$soilmoist*100/upper_bound
dat2 <- left_join(lantleaf,dat1) 

plot(dat2$prop~dat2$datep, typ = 'l')
lines(dat1$soilm~dat1$Date, col = 'red')

lantleaf <- phendat1 %>% filter(phenophase == 'leaves_new')
lantflow <- phendat1 %>% filter(phenophase == 'flower_bud')


lantleaf$datep <- as.Date(lantleaf$datep)
dat1 <- dat %>% arrange(as.Date(datep))
dat1$soilmoist <- NA
dat1$soilmoist <- get_soilmoisture(dat1$Rainfall.mm,k1[t[1]])
dat1$soilmoist[dat1$soilmoist > upper_bound] <- upper_bound
dat1$soilmoist <- dat1$soilmoist*100/upper_bound
dat2 <- left_join(lantleaf,dat1) 

years = as.Date(paste0(c(2008:2017),'-01-01'))
plot(dat2$prop~dat2$datep, typ = 'l', lwd = 2)
abline(v = years, col = 'purple')
lines(dat2$soilmoist~dat2$Date, col = 'red', lwd = 2)

upper_bound/exp(-k)

decompose()

plot(dat2$prop~dat2$soilm)

lm(dat2$prop~dat2$soilm)
