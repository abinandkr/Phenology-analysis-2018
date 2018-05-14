rm(list = ls())
library(tidyverse)   


lant <- read.csv('lantana_phenology.csv', stringsAsFactors = F)


plot(lant$prop[lant$phenophase == 'leaves_new'], typ = 'l')
lines(lant$prop[lant$phenophase == 'flower_bud'], typ = 'l',col = 'red')


wdat <- read.csv('weather_man_aut_day_compiled.csv', stringsAsFactors = F)

wdat1 <- wdat %>% group_by(datep) %>% summarise(Rainfall.mm = sum(Rainfall.mm))

wdat1$raindays <- 0

wdat1$lastrain <- wdat1$Rainfall.mm

phendat <- read.csv('collated1.csv', stringsAsFactors =  F)
phendat1 <- phendat %>% filter(species_id == 12, !notes %in% c('','-', '_'))


unique(phendat1$notes)

phendat1 <- phendat1[grep('dead', phendat1$notes, ignore.case = T),]

phendat2 <- phendat1 %>% group_by(datep) %>% summarise(ndead = n_distinct(ind_id))



for(i in 2:nrow(wdat1)){
  if(wdat1$Rainfall.mm[i]<15 & wdat1$Rainfall.mm[i-1]>15) wdat1$raindays[i] <- -1
  if(wdat1$Rainfall.mm[i]>15 & wdat1$Rainfall.mm[i-1]>15) wdat1$raindays[i] <- wdat1$raindays[i-1]+1
  if(wdat1$Rainfall.mm[i]<15 & wdat1$Rainfall.mm[i-1]<15) wdat1$raindays[i] <- wdat1$raindays[i-1]-1
  if(wdat1$lastrain[i] < 15) wdat1$lastrain[i] <- wdat1$lastrain[i-1]
}


hist(wdat1$raindays)
lant_leaf <- lant %>% filter(phenophase == 'leaves_new')
lant_leaf <- left_join(lant_leaf,wdat1)
head(lant_leaf)
plot(lant_leaf$prop~lant_leaf$raindays)


#phendat2 <- left_join(wdat1,phendat2)

#plot(phendat2$ndead~phendat2$raindays)


mod1 <- with(lant_leaf, glm(prop~raindays,family = 'binomial'))
summary(mod1)
x <- seq(-11,10,by = .1)


preddat <- predict(mod1,newdata = data.frame(raindays = x),se.fit = TRUE)
with(lant_leaf, plot(raindays, prop))
with(preddat, lines(x, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))





plot(lant_leaf$prop[!is.na(lant_leaf$prop)], typ = 'l', lwd = 2)
preddat1 <- predict(mod1,se.fit = TRUE)
with(preddat1,lines(exp(fit)/(1+exp(fit)), col="blue"))
with(preddat1, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), col = 'grey'))
with(preddat1, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), col = 'grey'))

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

<<<<<<< HEAD




dat <- read.csv('collated_long.csv', stringsAsFactors = F)

landat <- dat %>% filter(species_id == 12, phenophase == 'leaves_new')

landat$value[landat$value == 2] <- 1

landat1 <- left_join(landat,wdat1)

plot(jitter(landat1$value)~landat1$raindays)

landat1$ind_id <- factor(landat1$ind_id)

library(lme4)

mod3 <- glmer(value~raindays+(1|ind_id), family = 'binomial',data = landat1)

summary(mod3)

lattice::dotplot(ranef(mod3, which = "ind_id", condVar = TRUE))

cm <- coef(mod3)

x <- seq(-11,11,length.out = 100)

imax <- max(cm$ind_id$`(Intercept)`)
imin <- min(cm$ind_id$`(Intercept)`)

mumax <- imax + .4285*x
mumin <- imin + .4285*x

ymax <- exp(mumax)/(1+exp(mumax))
ymin <- exp(mumin)/(1+exp(mumin))

plot(ymax~x, typ = 'l')
lines(ymin~x)
abline(v = 0)
abline(h = .5)
abline(h = .75)


testdat <- landat %>% filter(ind_id %in% c(5,9))
=======
>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
