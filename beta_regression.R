####### Beta regression phenology weather

rm(list =ls())
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)


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
plot(phendat1$prop[phendat1$species_id == 1 & phendat1$phenophase == 'flower_bud'], typ = 'l')


dat <- read.csv('rvs weather 2014.csv', stringsAsFactors = F)
dat$Rainfall.mm <- as.numeric(dat$Rainfall.mm)
dat[dat == ''] <- NA
dat$Rainfall.mm[is.na(dat$Rainfall.mm)] <- 0
dat$Date <- as.Date(dat$Date,format = '%d.%m.%Y')
dat$datep <- ifelse(day(dat$Date) < 15, paste(year(dat$Date),month(dat$Date),'01',sep = '-'),paste(year(dat$Date),month(dat$Date),'15',sep = '-'))
dat$datep <- as.Date(dat$datep)
wdat <- read.csv('August 2017 compiled.csv',stringsAsFactors = F)
wdat$Date <- as.Date(wdat$Date)
wdat <- wdat %>%  group_by(Date) %>% summarise(Rainfall.mm = sum(Rain.mm,na.rm = T), mintemp = min(Temp, na.rm = T),maxtemp = max(Temp,na.rm = T), parsum = sum(PAR,na.rm = T), pardur = sum(PAR>1.2)*15)
wdat$datep <- ifelse(day(wdat$Date) < 15, paste(year(wdat$Date),month(wdat$Date),'01',sep = '-'),paste(year(wdat$Date),month(wdat$Date),'15',sep = '-'))
dat <- dat %>% select(Date,Rainfall.mm,Min.temp,Max.temp,datep)
dat <- dat[!is.na(dat$Date),]
dat$parsum <- NA
dat$pardur <- NA
dat <- dat[,c(1,2,3,4,6,7,5)]
colnames(wdat) <- colnames(dat)
wdat <- wdat %>% filter(Date>max(dat$Date))
dat <- rbind(dat,wdat)
dat$Min.temp <- as.numeric(dat$Min.temp)
dat$Max.temp <- as.numeric(dat$Max.temp)
dat$Rainfall.mm <- as.numeric(dat$Rainfall.mm)

dat1 <- dat %>% group_by(datep) %>% summarise(rain = sum(Rainfall.mm, na.rm = T), tempmax = max(Max.temp, na.rm = T), tempmin = min(Min.temp, na.rm = T), pars = mean(log(parsum), na.rm = T), pard = mean(pardur, na.rm = T))
dat1 <- dat1 %>% filter(datep > '2007-11-21', datep<'2017-01-01')
warnings()

phendat2 <- left_join(phendat1,dat1)
phendat2$year <- year(phendat2$datep)
phendat2$month <- month(phendat2$datep)

phendat2 <- phendat2 %>% filter(!is.na(prop),!is.infinite(tempmax))
phendat2$prop[phendat2$prop == 0 ] <- .001
phendat2$prop[phendat2$prop == 1 ] <- .999


library(betareg)

par(mfrow = c(2,2), mar = c(2,2,2,2))


phen = 'flower_bud'
weat = 'tempmax'
spec = 12
#testphen <- phendat2 %>% filter(species_id == spec, phenophase == phen)
#plot(testphen$tempmin~testphen$pars)

{
testphen <- phendat2 %>% filter(species_id == spec, phenophase == phen)

plot(testphen$prop, typ = 'l')
testphen <- testphen %>% select_('prop',weat)
colnames(testphen) <- c('prop','weath')
mod1 <- betareg(prop~weath, data = testphen)

print(summary(mod1))
se <- coef(summary(mod1))$mean[,2]

b0 = coef(mod1)[1]
b1 = coef(mod1)[2]
phi = coef(mod1)[3]

b0.2se = b0 + se[1] 
b1.2se = b1 + se[2]

b0.neg2se = b0 - se[1] 
b1.neg2se = b1 - se[2]


print(phi)
temp = seq(min(testphen$weath, na.rm = T),max(testphen$weath,na.rm = T), length.out = 100)

mu = exp(b0+(b1*temp))/(1+exp(b0+(b1*temp)))
mu.2se =  exp(b0.2se+(b1.2se*temp))/(1+exp(b0.2se+(b1.2se*temp)))
mu.neg2se = exp(b0.neg2se+(b1.neg2se*temp))/(1+exp(b0.neg2se+(b1.neg2se*temp)))
p <- mu*phi
q <- (1-mu)*phi

x <-  seq(0,1,length.out = 100)
mutemp = exp(b0+(b1*temp))/(1+exp(b0+(b1*temp)))

plot(testphen$prop~testphen$weath)
lines(mu~temp, col = 'red')
lines(mu.2se~temp, col = 'red', lty = 2)
lines(mu.neg2se~temp, col = 'red', lty = 2)


plot(dbeta(x,p[1],q[1])~x, typ = 'l', col = 'blue', ylim = c(0,1))
lines(dbeta(x,p[100],q[100])~x, typ = 'l', col = 'red')

plot(p~temp, col = 'blue', ylim = c(0,phi), typ = 'l')
lines(q~temp, col = 'red')
lines((p/(p+q))~temp,col = 'purple',lty = 2 )
}

 
lantphen <- phendat2 %>% filter(species_id == 12)

lantleaf <- lantphen %>% filter(phenophase == 'leaves_new')
lantbud <- lantphen %>% filter(phenophase == 'flower_bud')


par(mfrow = c(1,1))
plot(lantleaf$prop, typ = 'l')
lines(lantbud$prop, typ = 'l', col = 'red')

plot(jitter(lantbud$prop)~jitter(lantleaf$prop))
abline(0,1)


#plot(mod1)

x <- runif(20)

b0 = -3
b1 = 3
phi = 20


mu = exp(b0+(b1*x))/(1+exp(b0+(b1*x)))

testmat <- data.frame(x = x, mu = mu, p = mu*phi,q = (1-mu)*phi)

testmat <- testmat %>% rowwise() %>% mutate(y = rbeta(1,p,q))

testmat <- testmat[order(testmat$mu),]

plot(testmat$y~testmat$x)
lines(testmat$mu~testmat$x, col = 'red')
testmat$y[testmat$y == 0] <- .0000001
testmat$y[testmat$y == 1] <- .9999999

t1 <- betareg(y~x, data = testmat)
summary(t1)


par(mfrow = c(1,1))
plot(wdat$PAR[1:4000], typ = 'l')
abline(h = 2)

dayl <- wdat %>% select(Date,PAR)

dayl <- dayl %>% group_by(Date)%>% summarise(mins = sum(PAR > 1.2)*15) 

plot(dayl$mins~dayl$Date, typ = 'l')

dayl$year <- year(dayl$Date)
dayl$month <- month(dayl$Date)
dayl$day <- day(dayl$Date)
dayl$day <- ifelse(dayl$day < 15,1,15)
dayl$datep <- as.Date(paste(dayl$year,dayl$month,dayl$day, sep = '-'))

dayl1 <- dayl %>% group_by(datep) %>% summarise(avemins = mean(mins))

plot(log(dayl1$avemins), typ = 'l')
