#################### Randia ###################

rm(list = ls())
library(climwin)
library(tidyverse)
library(zoo)
library(evd)


dat <- read.csv('collated_long.csv', stringsAsFactors = F)
dat$value[dat$value == 2] <- 1
dat <- dat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 
dat <- dat %>% filter(datep > '2007-12-31', datep < '2017-01-01') 
dat$datep <- as.Date(dat$datep)
dat_spc <- dat %>% filter(species_id == 15)
rm(dat)
wdat <- read.csv('weather_photoperiod.csv',stringsAsFactors = F)
wdat <- wdat %>% filter(Date > '2007-01-01')
wdat$Date <- as.Date(wdat$Date)
wdat <- wdat %>% arrange(Date) %>% filter(Max.temp < 50)
wdat$Max.temp <- na.spline(wdat$Max.temp)
wdat$Min.temp <- na.spline(wdat$Min.temp)
wdat$Rainfall.mm <- as.numeric(wdat$Rainfall.mm)
#wdat <- wdat %>% filter(!is.na(Rainfall.mm))
wdat$Date <- format(as.Date(wdat$Date), '%d/%m/%Y')

rain <- wdat$Rainfall.mm
wdat$temp <- (wdat$Min.temp = wdat$Max.temp)/2
temp <- wdat$temp
mintemp <- wdat$Min.temp
maxtemp <- wdat$Max.temp
sunrise <- wdat$sunrise
sunrise <- wdat$sunset
photop <- wdat$photoperiod


####### Leaf flush #########
range = c(30,0)

dat_phen <- dat_spc %>% filter(phenophase == 'leaves_new')

plot(dat_phen$prop, typ = 'l')

dat_phen$datep <- format(as.Date(dat_phen$datep), '%d/%m/%Y')


phenWin <- slidingwin(xvar = list(rain = rain),
                      cdate = wdat$Date,
                      bdate = dat_phen$datep,
                      baseline = glm(prop ~ 1, data = dat_phen, family = 'binomial'),
                      cinterval = 'day',
                      range = c(45,0),
                      type = 'relative',
                      stat = 'sum',
                      func = 'lin',
                      binary = T,
                      upper = c(7:10),
                      cmissing = 'method2')



phenWin$combos

summary(phenWin[[1]]$BestModel)

moddat <- phenWin[[1]]$BestModelData

phenOutput <- phenWin[[1]]$Dataset

plot(moddat$yvar~moddat$climate)

plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)


#########################
dat <- read.csv('collated_long.csv', stringsAsFactors = F)
dat$value[dat$value == 2] <- 1
dat_spc <- dat %>% filter(species_id == 15)
dat_spc$datep <- as.Date(dat_spc$datep)
dat_spc1 <- dat_spc %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 
dat_spc1 <- dat_spc1 %>% filter(datep > '2007-12-31', datep < '2017-01-01') 


dat_phen <- dat_spc1 %>% filter(phenophase == 'leaves_new')

dat_phen$datep <- as.Date(dat_phen$datep)

cutoff <- 8

wdat1 <- wdat 

wdat1$Rainfall.mm[wdat1$Rainfall.mm < cutoff] <- 0 

wdat1 <- wdat1 %>% group_by(datep) %>% summarise(Rainfall.mm = sum(Rainfall.mm, na.rm =  T))

wdat1$raindays <- 0

weekcut <- 20


#for(i in 3:nrow(wdat1)){
#  if(wdat1$Rainfall.mm[i]<weekcut & wdat1$Rainfall.mm[i-1]<weekcut) wdat1$raindays[i] <- -1
#  if((wdat1$Rainfall.mm[i] + wdat1$Rainfall.mm[i-1]) > 2*weekcut) wdat1$raindays[i] <- wdat1$raindays[i-1]+1
#  if(wdat1$Rainfall.mm[i]<weekcut & wdat1$Rainfall.mm[i-1]<weekcut & wdat1$Rainfall.mm[i-2]<weekcut) wdat1$raindays[i] <- wdat1$raindays[i-1]-1
#  if(wdat1$Rainfall.mm[i]>weekcut & wdat1$Rainfall.mm[i-1]<weekcut & wdat1$Rainfall.mm[i-2]<weekcut) wdat1$raindays[i] <- 0
#  if(wdat1$raindays[i] > 5) 
#}

for(i in 2:nrow(wdat1)){
  if(wdat1$Rainfall.mm[i]<10 & wdat1$Rainfall.mm[i-1]>10) wdat1$raindays[i] <- -1
  if(wdat1$Rainfall.mm[i]>10 & wdat1$Rainfall.mm[i-1]>10) wdat1$raindays[i] <- wdat1$raindays[i-1]+1
  if(wdat1$Rainfall.mm[i]<10 & wdat1$Rainfall.mm[i-1]<10) wdat1$raindays[i] <- wdat1$raindays[i-1]-1
  #  if(wdat1$lastrain[i] < 10) wdat1$lastrain[i] <- wdat1$lastrain[i-1]
}


hist(wdat1$raindays)
wdat1$datep <- as.Date(wdat1$datep)

dat1 <- left_join(dat_phen,wdat1)

plot(dat1$prop~dat1$raindays)

mod1 <- glm(prop~raindays, data = dat1, family = 'binomial')
summary(mod1)

x <- seq(-15,15,length.out = 100)

preddat <- predict(mod1,newdata = data.frame(raindays = x),se.fit = TRUE)
with(dat1, plot(raindays, prop))
with(preddat, lines(x, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))



plot(dat1$prop[!is.na(dat1$prop)], typ = 'l', lwd = 2)
preddat1 <- predict(mod1,se.fit = TRUE)
with(preddat1,lines(exp(fit)/(1+exp(fit)), col="blue"))
with(preddat1, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), col = 'grey'))
with(preddat1, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), col = 'grey'))



dat_spc_phen <- dat_spc %>% filter(phenophase =='leaves_new')

dat2 <- left_join(dat_spc_phen,wdat1)

plot(jitter(dat2$value)~dat2$raindays)

mod2 <- glmer(value~raindays+(1|ind_id), data = dat2, family = 'binomial')

summary(mod2)
lattice::dotplot(ranef(mod2, which = "ind_id", condVar = TRUE))

################ flowering ###############

dat_phen <- dat_spc %>% filter(phenophase == 'flower_bud')

plot(dat_phen$prop, typ = 'l')

dat_phen$datep <- format(as.Date(dat_phen$datep), '%d/%m/%Y')


phenWin <- slidingwin(xvar = list(rain = temp),
                      cdate = wdat$Date,
                      bdate = dat_phen$datep,
                      baseline = glm(prop ~ 1, data = dat_phen, family = 'binomial'),
                      cinterval = 'day',
                      range = c(75,0),
                      type = 'relative',
                      stat = 'sum',
                      func = 'lin',
                      binary = T,
                      upper = 21,
                      cmissing = 'method2')


Y
t <- phenWin$combos

summary(phenWin[[1]]$BestModel)

moddat <- phenWin[[1]]$BestModelData

phenOutput <- phenWin[[1]]$Dataset

plot(moddat$yvar~moddat$climate)

plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)



