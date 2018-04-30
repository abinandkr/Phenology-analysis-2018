rm(list = ls())
library(climwin)
library(tidyverse)
library(zoo)
library(evd)
library(lme4)
plot_model <- function(prop,model, se.fit = FALSE){
  preddat <- predict(model,se.fit = TRUE)
  plot(prop, typ = 'l')
  with(preddat, lines(exp(fit)/(1+exp(fit)), col="blue"))
  if(se.fit == TRUE){
    with(preddat, lines(exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
    with(preddat, lines(exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))
  }
}


spcs <- 8

dat <- read.csv('collated_long.csv', stringsAsFactors = F)
dat$value[dat$value == 2] <- 1
dat <- dat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 
dat$datep <- as.Date(dat$datep)
dat_spc <- dat %>% filter(species_id == spcs)
rm(dat)
wdat <- read.csv('weather_photoperiod.csv',stringsAsFactors = F)
wdat$Date <- as.Date(wdat$Date)
wdat$Max.temp <- na.spline(wdat$Max.temp)
wdat$Min.temp <- na.spline(wdat$Min.temp)
wdat$Rainfall.mm <- as.numeric(wdat$Rainfall.mm)
wdat$temp <- (wdat$Min.temp + wdat$Max.temp)/2
wdat$diff <- c(NA,diff(wdat$photoperiod))
wdat$diff2 <- abs(c(NA,diff(wdat$photoperiod)))

rain <- wdat$Rainfall.mm
temp <- wdat$temp
photop <- wdat$photoperiod

####### Leaf flush #########

dat_phenl <- dat_spc %>% filter(phenophase == 'leaves_new')
dat_phenl$datep <- as.Date(dat_phenl$datep)
wdat$datep <- as.Date(wdat$datep)

plot(dat_phenl$prop, typ = 'l')
dat_phenl <- left_join(dat_phenl,wdat, by = c("datep" = "Date"))


leaf_mod1 <- glm(prop~temp,data = dat_phenl,family = 'binomial')
summary(leaf_mod1)
plot_model(dat_phenl$prop,leaf_mod1)

leaf_mod2 <- glm(prop~photoperiod,data = dat_phenl,family = 'binomial')
summary(leaf_mod2)
plot_model(dat_phenl$prop,leaf_mod2)

dat_phenl$temp_photop_int <- dat_phenl$photoperiod*dat_phenl$temp/60

leaf_mod3 <- glm(prop~temp+temp_photop_int,data = dat_phenl,family = 'binomial')
summary(leaf_mod3)
plot_model(dat_phenl$prop,leaf_mod3)


dat_phenl$datep1 <- format(as.Date(dat_phenl$datep), '%d/%m/%Y')
wdat$Date1 <- format(as.Date(wdat$Date), '%d/%m/%Y')
phenWin1 <- slidingwin(xvar = list(temp = temp, rain = rain),
                       cdate = wdat$Date1,
                       bdate = dat_phenl$datep1,
                       baseline = glm(prop ~ 1, data = dat_phenl, family = 'binomial'),
                       cinterval = 'week',
                       range = c(20,0),
                       type = 'relative',
                       stat = 'mean',
                       func = 'lin',
                       cmissing = 'method2')

phenWin1$combos
leaf_climwin_mod1 <- phenWin1[[1]]$BestModel
plot_model(dat_phenl$prop,leaf_climwin_mod1)
summary(phenWin1[[1]]$BestModel)
moddat <- phenWin1[[1]]$BestModelData
phenOutput <- phenWin1[[1]]$Dataset
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)


phenWin2 <- slidingwin(xvar = list(temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenl$datep1,
                       baseline = glm(prop ~ 1, data = dat_phenl, family = 'binomial'),
                       cinterval = 'day',
                       range = c(100,0),
                       type = 'relative',
                       stat = 'mean',
                       func = 'lin',
                       cmissing = 'method2')

phenWin2$combos
leaf_climwin_mod2 <- phenWin2[[1]]$BestModel
summary(leaf_climwin_mod2)
plot_model(dat_phenl$prop,leaf_climwin_mod2)
summary(phenWin2[[1]]$BestModel)
moddat <- phenWin2[[1]]$BestModelData
phenOutput <- phenWin2[[1]]$Dataset
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)


phenWin3 <- slidingwin(xvar = list(temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenl$datep1,
                       baseline = glm(prop ~ 1 + photoperiod, data = dat_phenl, family = 'binomial'),
                       cinterval = 'day',
                       range = c(50,0),
                       type = 'relative',
                       stat = 'mean',
                       func = 'lin',
                       cmissing = 'method2')

phenWin3$combos
leaf_climwin_mod3 <- phenWin3[[1]]$BestModel
summary(leaf_climwin_mod3)
plot_model(dat_phenl$prop,leaf_climwin_mod3)
summary(phenWin3[[1]]$BestModel)
moddat <- phenWin3[[1]]$BestModelData
phenOutput <- phenWin3[[1]]$Dataset
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)



phenWin4 <- slidingwin(xvar = list(temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenl$datep1,
                       baseline = glm(prop ~ 1, data = dat_phenl, family = 'binomial'),
                       cinterval = 'week',
                       range = c(20,0),
                       type = 'relative',
                       stat = 'mean',
                       func = 'lin',
                       binary = T,
                       upper = c(25),
                       cmissing = 'method2')



Y
phenWin4$combos
leaf_climwin_mod4 <- phenWin4[[1]]$BestModel
summary(leaf_climwin_mod4)
plot_model(dat_phenl$prop,leaf_climwin_mod4)
summary(phenWin4[[1]]$BestModel)
moddat <- phenWin4[[1]]$BestModelData
phenOutput <- phenWin4[[1]]$Dataset
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)


phenWin5 <- slidingwin(xvar = list(temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenl$datep1,
                       baseline = glm(prop ~ 1, data = dat_phenl, family = 'binomial'),
                       cinterval = 'day',
                       range = c(100,0),
                       type = 'relative',
                       stat = 'mean',
                       func = 'lin',
                       binary = T,
                       upper = c(25),
                       cmissing = 'method2')

phenWin5$combos
leaf_climwin_mod5 <- phenWin5[[1]]$BestModel
summary(leaf_climwin_mod5)
plot_model(dat_phenl$prop,leaf_climwin_mod5)
summary(phenWin5[[1]]$BestModel)
moddat <- phenWin5[[1]]$BestModelData
phenOutput <- phenWin5[[1]]$Dataset
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)



phenWin6 <- slidingwin(xvar = list(temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenl$datep1,
                       baseline = glm(prop ~ 1 + photoperiod, data = dat_phenl, family = 'binomial'),
                       cinterval = 'day',
                       range = c(100,0),
                       type = 'relative',
                       stat = 'mean',
                       func = 'lin',
                       binary = T,
                       upper = c(25),
                       cmissing = 'method2')



Y
phenWin6$combos
leaf_climwin_mod6 <- phenWin6[[1]]$BestModel
summary(leaf_climwin_mod6)
plot_model(dat_phenl$prop,leaf_climwin_mod6)
summary(phenWin6[[1]]$BestModel)
moddat <- phenWin6[[1]]$BestModelData
phenOutput <- phenWin6[[1]]$Dataset
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)



######################### flowerbud###############

dat_phenf <- dat_spc %>% filter(phenophase == 'flower_bud')
plot(dat_phenf$prop, typ = 'l')
dat_phenf$datep <- as.Date(dat_phenf$datep)
wdat$datep <- as.Date(wdat$datep)
dat_phenf <- left_join(dat_phenf,wdat, by = c("datep" = "Date"))

dat_phenf$diff2 <- abs(dat_phenf$diff)

fl_mod1 <- glm(prop~photoperiod,data = dat_phenf,family = 'binomial')
summary(fl_mod1)
plot_model(dat_phenf$prop,fl_mod1)

fl_mod2 <- glm(prop~temp,data = dat_phenf,family = 'binomial')
summary(fl_mod2)
plot_model(dat_phenf$prop,fl_mod2)


dat_phenf$datep1 <- format(as.Date(dat_phenf$datep), '%d/%m/%Y')
wdat$Date1 <- format(as.Date(wdat$Date), '%d/%m/%Y')

phenWin1 <- slidingwin(xvar = list(rain = rain, temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenf$datep1,
                       baseline = glm(prop ~ 1, data = dat_phenf, family = 'binomial'),
                       cinterval = 'week',
                       range = c(47,0),
                       type = 'relative',
                       stat = c('mean'),
                       func = 'lin',
                       cmissing = 'method2')



phenWin1$combos

fl_climwin_mod1 <- phenWin1[[2]]$BestModel
summary(fl_climwin_mod1)
moddat <- phenWin1[[2]]$BestModelData
phenOutput <- phenWin1[[2]]$Dataset
plot(moddat$yvar~moddat$climate)
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)
plot_model(dat_phenf$prop,fl_climwin_mod1)


phenWin2 <- slidingwin(xvar = list(rain = rain, temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenf$datep1,
                       baseline = glm(prop ~ 1 + photoperiod, data = dat_phenf, family = 'binomial'),
                       cinterval = 'week',
                       range = c(47,0),
                       type = 'relative',
                       stat = c('mean'),
                       func = 'lin',
                       cmissing = 'method2')



phenWin2$combos

fl_climwin_mod2 <- phenWin2[[2]]$BestModel
summary(fl_climwin_mod2)
moddat <- phenWin2[[2]]$BestModelData
phenOutput <- phenWin2[[2]]$Dataset
plot(moddat$yvar~moddat$climate)
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)
plot_model(dat_phenf$prop,fl_climwin_mod2)


phenWin3 <- slidingwin(xvar = list(temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenf$datep1,
                       baseline = glm(prop ~ 1 , data = dat_phenf, family = 'binomial'),
                       cinterval = 'day',
                       range = c(200,0),
                       type = 'relative',
                       stat = c('mean'),
                       func = 'lin',
                       cmissing = 'method2')


y
phenWin3$combos

fl_climwin_mod3 <- phenWin3[[1]]$BestModel
summary(fl_climwin_mod3)
moddat <- phenWin3[[1]]$BestModelData
phenOutput <- phenWin3[[1]]$Dataset
plot(moddat$yvar~moddat$climate)
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)
plot_model(dat_phenf$prop,fl_climwin_mod3)

phenWin4 <- slidingwin(xvar = list(temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenf$datep1,
                       baseline = glm(prop ~ 1 + photoperiod, data = dat_phenf, family = 'binomial'),
                       cinterval = 'day',
                       range = c(200,0),
                       type = 'relative',
                       stat = c('mean'),
                       func = 'lin',
                       cmissing = 'method2')



phenWin4$combos

fl_climwin_mod4 <- phenWin4[[1]]$BestModel
summary(fl_climwin_mod4)
moddat <- phenWin4[[1]]$BestModelData
phenOutput <- phenWin4[[1]]$Dataset
plot(moddat$yvar~moddat$climate)
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)
plot_model(dat_phenf$prop,fl_climwin_mod4)

phenWin5 <- slidingwin(xvar = list(temp = temp),
                       cdate = wdat$Date1,
                       bdate = dat_phenf$datep1,
                       baseline = glm(prop ~ 1 + photoperiod, data = dat_phenf, family = 'binomial'),
                       cinterval = 'day',
                       range = c(150,0),
                       type = 'relative',
                       stat = c('mean'),
                       func = 'lin',
                       binary = T,
                       upper= c(25),
                       cmissing = 'method2')



phenWin5$combos

fl_climwin_mod5 <- phenWin5[[1]]$BestModel
summary(fl_climwin_mod5)
moddat <- phenWin5[[1]]$BestModelData
phenOutput <- phenWin5[[1]]$Dataset
plot(moddat$yvar~moddat$climate)
plotdelta(phenOutput)
plotweights(phenOutput)
plotwin(phenOutput)
plot_model(dat_phenf$prop,fl_climwin_mod5)
