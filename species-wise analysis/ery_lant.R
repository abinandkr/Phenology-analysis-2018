rm(list = ls())
library(tidyverse)


dat <- read.csv('collated_long.csv', stringsAsFactors = F)
dat$value[dat$value == 2] <- 1
dat <- dat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 
dat <- dat %>% filter(datep > '2007-12-31', datep < '2017-01-01') 
dat$datep <- as.Date(dat$datep)
dat_test <- dat %>% filter(datep > '2008-12-31',datep<'2015-01-01')


wdat <- read.csv('weather_photoperiod.csv',stringsAsFactors = F)
wdat <- wdat %>% filter(Date > '2007-01-01')
wdat$Date <- as.Date(wdat$Date)
wdat <- wdat %>% arrange(Date) %>% filter(Max.temp < 50)
wdat$Max.temp <- na.spline(wdat$Max.temp)
wdat$Min.temp <- na.spline(wdat$Min.temp)
wdat$Rainfall.mm <- as.numeric(wdat$Rainfall.mm)

wdat1 <- wdat %>% group_by(datep) %>% summarise(Rainfall.mm = sum(Rainfall.mm,na.rm = T))


ery_leaf <- dat_test %>% filter(species_id == 10, phenophase == 'leaves_new')
lant_leaf <- dat_test %>% filter(species_id == 12, phenophase == 'leaves_new')
ery_bud <- dat_test %>% filter(species_id == 10, phenophase == 'flower_bud')
lant_bud <- dat_test %>% filter(species_id == 12, phenophase == 'flower_bud')

ery_leaf_all <- dat %>% filter(species_id == 10, phenophase == 'leaves_new')
lant_leaf_all <- dat %>% filter(species_id == 12, phenophase == 'leaves_new')
ery_bud_all <- dat %>% filter(species_id == 10, phenophase == 'flower_bud')
lant_bud_all <- dat %>% filter(species_id == 12, phenophase == 'flower_bud')


{raincut <- 15
wdat1$raindays <- 0
  
wdat1$datep <- as.Date(wdat1$datep)
  
for(i in 2:nrow(wdat1)){
  if(wdat1$Rainfall.mm[i]<raincut & wdat1$Rainfall.mm[i-1]>raincut) wdat1$raindays[i] <- -1
  if(wdat1$Rainfall.mm[i]>raincut & wdat1$Rainfall.mm[i-1]>raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]+1
  if(wdat1$Rainfall.mm[i]<raincut & wdat1$Rainfall.mm[i-1]<raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]-1
  #  if(wdat1$lastrain[i] < 10) wdat1$lastrain[i] <- wdat1$lastrain[i-1]
}

  
ery_leaf1 <- left_join(ery_leaf,wdat1)
lant_leaf1 <- left_join(lant_leaf,wdat1)

mod_ery_leaf <- with(ery_leaf1, glm(prop~raindays, family = 'binomial'))
mod_lant_leaf <- with(lant_leaf1, glm(prop~raindays, family = 'binomial'))

erymod<- summary(mod_ery_leaf)
lantmod <- summary(mod_lant_leaf)

t <- data.frame(AIC = c(erymod$aic,lantmod$aic), rdev = c(erymod$deviance,lantmod$deviance), row.names =  c('Ery','Lan'))

x <- seq(-11,11,length.out = 100)

plot(ery_leaf1$prop~ery_leaf1$raindays, col = 'darkred',pch = 16, cex = .5)
points(lant_leaf1$prop~lant_leaf1$raindays, col = 'darkblue',pch = 16, cex = .5)

pred_ery_leaf <- predict(mod_ery_leaf,newdata = data.frame(raindays = x),se.fit = TRUE)
with(pred_ery_leaf, lines(x, exp(fit)/(1+exp(fit)), col="darkred"))
with(pred_ery_leaf, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2, col = 'darkred'))
with(pred_ery_leaf, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2, col = 'darkred'))

pred_lant_leaf <- predict(mod_lant_leaf,newdata = data.frame(raindays = x),se.fit = TRUE)
with(pred_lant_leaf, lines(x, exp(fit)/(1+exp(fit)), col="darkblue"))
with(pred_lant_leaf, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2, col = 'darkblue'))
with(pred_lant_leaf, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2, col = 'darkblue'))

print(t)
}

ery_leaf_all <- left_join(ery_leaf_all,wdat1)

plot(ery_leaf_all$prop, typ = 'l', lwd = 2)
preddat1 <- predict(mod_ery_leaf,se.fit = TRUE,newdata = data.frame(raindays = lag(ery_leaf_all$raindays)))
with(preddat1,lines(exp(fit)/(1+exp(fit)), col="blue"))
with(preddat1, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), col = 'grey'))
with(preddat1, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), col = 'grey'))

lant_leaf_all <- left_join(lant_leaf_all,wdat1)

plot(lant_leaf_all$prop, typ = 'l', lwd = 2)
preddat1 <- predict(mod_lant_leaf,se.fit = TRUE,newdata = data.frame(raindays = lag(ery_leaf_all$raindays)))
with(preddat1,lines(exp(fit)/(1+exp(fit)), col="darkblue", lwd = 2))
with(preddat1, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), col = 'blue', lty = 3))
with(preddat1, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), col = 'blue', lty = 3))





{raincut <- 10
  wdat1$raindays <- 0
  
  wdat1$datep <- as.Date(wdat1$datep)
  
  for(i in 2:nrow(wdat1)){
    if(wdat1$Rainfall.mm[i]<raincut & wdat1$Rainfall.mm[i-1]>=raincut) wdat1$raindays[i] <- -1
    if(wdat1$Rainfall.mm[i]>=raincut & wdat1$Rainfall.mm[i-1]>=raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]+1
    if(wdat1$Rainfall.mm[i]<raincut & wdat1$Rainfall.mm[i-1]<raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]-1
    #  if(wdat1$lastrain[i] < 10) wdat1$lastrain[i] <- wdat1$lastrain[i-1]
  }
  
  
  ery_bud1 <- left_join(ery_bud,wdat1)
  
  mod_ery_bud <- with(ery_bud1, glm(prop~raindays, family = 'binomial'))
  
  #summary(mod_ery_bud)
  #summary(mod_lant_bud)
  
  x <- seq(-15,11,length.out = 100)
  
  plot(ery_bud1$prop~ery_bud1$raindays, col = 'darkred',pch = 16, cex = .5)
  
  pred_ery_bud <- predict(mod_ery_bud,newdata = data.frame(raindays = x),se.fit = TRUE)
  with(pred_ery_bud, lines(x, exp(fit)/(1+exp(fit)), col="darkred"))
  with(pred_ery_bud, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2, col = 'darkred'))
  with(pred_ery_bud, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2, col = 'darkred'))
  
}


plot(ery_bud_all$prop, typ = 'l', lwd = 2)
preddat1 <- predict(mod_ery_bud,se.fit = TRUE,newdata = data.frame(raindays = lag(ery_leaf_all$raindays)))
with(preddat1,lines(exp(fit)/(1+exp(fit)), col="darkblue", lwd = 2))
with(preddat1, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), col = 'blue', lty = 3))
with(preddat1, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), col = 'blue', lty = 3))


{raincut <- 35
  wdat1$raindays <- 0
  
  wdat1$datep <- as.Date(wdat1$datep)
  
  for(i in 2:nrow(wdat1)){
    if(wdat1$Rainfall.mm[i]<raincut & wdat1$Rainfall.mm[i-1]>=raincut) wdat1$raindays[i] <- -1
    if(wdat1$Rainfall.mm[i]>=raincut & wdat1$Rainfall.mm[i-1]>=raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]+1
    if(wdat1$Rainfall.mm[i]<raincut & wdat1$Rainfall.mm[i-1]<raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]-1
    #  if(wdat1$lastrain[i] < 10) wdat1$lastrain[i] <- wdat1$lastrain[i-1]
  }
  
  
lant_bud1 <- left_join(lant_bud,wdat1)

mod_lant_bud <- with(lant_bud1, glm(prop~raindays, family = 'binomial'))

plot(lant_bud1$prop~lant_bud1$raindays, col = 'darkblue',pch = 16, cex = .5)


pred_lant_bud <- predict(mod_lant_bud,newdata = data.frame(raindays = x),se.fit = TRUE)
with(pred_lant_bud, lines(x, exp(fit)/(1+exp(fit)), col="darkblue"))
with(pred_lant_bud, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2, col = 'darkblue'))
with(pred_lant_bud, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2, col = 'darkblue'))
}

summary(mod_lant_bud)
plot(lant_bud_all$prop, typ = 'l', lwd = 2)
preddat1 <- predict(mod_lant_bud,se.fit = TRUE,newdata = data.frame(raindays = lag(ery_leaf_all$raindays)))
with(preddat1,lines(exp(fit)/(1+exp(fit)), col="darkblue", lwd = 2))
with(preddat1, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), col = 'blue', lty = 3))
with(preddat1, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), col = 'blue', lty = 3))
