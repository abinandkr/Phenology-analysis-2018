rm(list = ls())
library(tidyverse)   


lant <- read.csv('lantana_phenology.csv', stringsAsFactors = F)


plot(lant$prop[lant$phenophase == 'leaves_new'], typ = 'l')
lines(lant$prop[lant$phenophase == 'flower_bud'], typ = 'l',col = 'red')


wdat <- read.csv('weather_man_aut_day_compiled.csv', stringsAsFactors = F)

wdat1 <- wdat %>% group_by(datep) %>% summarise(Rainfall.mm = sum(Rainfall.mm))

wdat1$raindays <- 0

wdat1$lastrain <- wdat1$Rainfall.mm

for(i in 2:nrow(wdat1)){
  if(wdat1$Rainfall.mm[i]<20 & wdat1$Rainfall.mm[i-1]>20) wdat1$raindays[i] <- -1
  if(wdat1$Rainfall.mm[i]>20 & wdat1$Rainfall.mm[i-1]>20) wdat1$raindays[i] <- wdat1$raindays[i-1]+1
  if(wdat1$Rainfall.mm[i]<20 & wdat1$Rainfall.mm[i-1]<20) wdat1$raindays[i] <- wdat1$raindays[i-1]-1
  if(wdat1$lastrain[i] < 20) wdat1$lastrain[i] <- wdat1$lastrain[i-1]
}


hist(wdat1$raindays)
lant_leaf <- lant %>% filter(phenophase == 'leaves_new')
lant_leaf <- left_join(lant_leaf,wdat1)
head(lant_leaf)
plot(lant_leaf$prop~lant_leaf$raindays)
mod1 <- with(lant_leaf, glm(prop~raindays,family = 'binomial'))
summary(mod1)
x <- seq(-11,10,by = .1)


preddat <- predict(mod1,newdata = data.frame(raindays = x),se.fit = TRUE)
with(lant_leaf, plot(raindays, prop))
with(preddat, lines(x, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))


plot(lant_leaf$prop[!is.na(lant_leaf$prop)], typ = 'l')
preddat1 <- predict(mod1,se.fit = TRUE)
with(preddat1,lines(exp(fit)/(1+exp(fit)), col="blue"))
with(preddat1, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat1, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))

mod2 <- with(lant_leaf, glm(prop~raindays+lastrain, family = 'binomial'))
summary(mod2)
plot(lant_leaf$prop[!is.na(lant_leaf$prop)], typ = 'l')
preddat2 <- predict(mod2,se.fit = TRUE)
with(preddat2,lines(exp(fit)/(1+exp(fit)), col="blue"))
with(preddat2, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat2, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))


phendat <- read.csv('collated1.csv', stringsAsFactors =  F)
phendat <- gather(phendat[,-(2:5)], key = 'phenophase',value = 'value', 3:8)
phendat$value <- factor(phendat$value)
library(plyr)
phendat$value <- mapvalues(phendat$value,from = c('Y','N',0,1,2), to = c(1,0,0,1,1))
detach("package:plyr")
phendat$value <- as.numeric(as.character(phendat$value))
phendat <- phendat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 
phendat <- phendat %>% filter(datep > '2007-12-31', datep < '2017-01-01') 

{
spcdat <- phendat %>% filter(species_id == 15, phenophase == 'leaves_new')

#wdat$Rainfall.mm <- ifelse(wdat$Rainfall.mm >10, wdat$Rainfall.mm,0)  
    
wdat1 <- wdat %>% group_by(datep) %>% summarise(Rainfall.mm1 = sum(Rainfall.mm), maxrain = max(Rainfall.mm), mintemp = min(Min.temp,na.rm = T),maxtemp = max(Max.temp,na.rm = T), meantemp = sum(mean(Max.temp,na.rm = T),mean(Min.temp, na.rm = T))/2)
  
wdat1$raindays <- 0
wdat1$lastrain <- wdat1$Rainfall.mm1
raincut <- 10

for(i in 2:nrow(wdat1)){
  if(wdat1$Rainfall.mm1[i]<raincut & wdat1$Rainfall.mm1[i-1]>raincut) wdat1$raindays[i] <- -1
  if(wdat1$Rainfall.mm1[i]>raincut & wdat1$Rainfall.mm1[i-1]>raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]+1
  if(wdat1$Rainfall.mm1[i]<raincut & wdat1$Rainfall.mm1[i-1]<raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]-1
  if(wdat1$lastrain[i] < raincut) wdat1$lastrain[i] <- wdat1$lastrain[i-1]
}

spcdat <- left_join(spcdat,wdat1)
plot(spcdat$prop~spcdat$raindays)

}

plot(spcdat$prop, typ = 'l')

plot(lant_leaf$prop, typ = 'l')
lines(spcdat$prop, col = 'red')


mod3 <- with(spcdat, glm(prop~raindays, family = 'binomial'))
summary(mod3)
plot(spcdat$prop[!is.na(spcdat$prop)], typ = 'l')
preddat3 <- predict(mod3,se.fit = TRUE)
with(preddat3,lines(exp(fit)/(1+exp(fit)), col="blue"))
with(preddat3, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat3, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))

