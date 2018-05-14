rm(list = ls())
library(climwin)
library(tidyverse)
library(zoo)
<<<<<<< HEAD
library(evd)
=======
>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186

dat <- read.csv('collated1.csv', stringsAsFactors = F)
dat <- gather(dat[,-(2:5)], key = 'phenophase',value = 'value', 3:8)
dat$value <- factor(dat$value)
library(plyr)
dat$value <- mapvalues(dat$value,from = c('Y','N',0,1,2), to = c(1,0,0,1,1))
detach("package:plyr")
dat$value <- as.numeric(as.character(dat$value))
<<<<<<< HEAD

=======
>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
dat <- dat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 
dat <- dat %>% filter(datep > '2007-12-31', datep < '2017-01-01') 
dat$datep <- as.Date(dat$datep)

<<<<<<< HEAD

dat_phen <- dat %>% filter(species_id == 12, phenophase == 'leaves_new', !is.na(prop))
=======
dat_phen <- dat %>% filter(species_id == 12, phenophase == 'flower_bud', !is.na(prop))
>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
dat_phen$datep <- format(as.Date(dat_phen$datep), '%d/%m/%Y')
dat_phen$prop <- as.numeric(dat_phen$prop)

wdat <- read.csv('weather_man_aut_day_compiled.csv',stringsAsFactors = F)
wdat <- wdat %>% filter(Date > '2007-01-01')
wdat$Date <- as.Date(wdat$Date)
<<<<<<< HEAD
wdat <- wdat %>% arrange(Date) %>% filter(Max.temp < 50)
dates_df <- data.frame(Date = seq.Date(as.Date('2007-01-02'),as.Date('2017-08-01'), by = 1))
wdat <- left_join(dates_df,wdat)
wdat$Max.temp <- na.spline(wdat$Max.temp)
wdat$Min.temp <- na.spline(wdat$Min.temp)
=======
dates_df <- data.frame(Date = seq.Date(as.Date('2007-01-02'),as.Date('2017-08-01'), by = 1))
wdat <- left_join(dates_df,wdat)
wdat <- wdat %>% arrange(Date)
wdat$Max.temp <- na.spline(wdat$Max.temp)
wdat$Min.temp <- na.spline(wdat$Min.temp)



>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
wdat$Rainfall.mm <- as.numeric(wdat$Rainfall.mm)
wdat <- wdat %>% filter(!is.na(Rainfall.mm))
wdat$Date <- format(as.Date(wdat$Date), '%d/%m/%Y')



<<<<<<< HEAD
#wdat$Rainfall.mm[wdat$Rainfall.mm == 0] <- .001

range = c(30,0)



phenWin <- slidingwin(xvar = list(rain = wdat$Rainfall.mm),
=======
wdat$Rainfall.mm[wdat$Rainfall.mm == 0] <- .001

range = c(40,0)


phenWin <- slidingwin(xvar = list(rain = wdat$Rainfall.mm, mintemp = wdat$Min.temp,maxtemp = wdat$Max.temp),
>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
                      cdate = wdat$Date,
                      bdate = dat_phen$datep,
                      baseline = glm(prop ~ 1, data = dat_phen, family = 'binomial'),
                      cinterval = 'week',
                      range = range,
                      type = 'relative',
<<<<<<< HEAD
                      stat = 'sum',
                      func = 'lin',
                      binary = T,
                      upper = c(0:15),
                      cmissing = 'method2')


y

=======
                      stat = 'mean',
                      func = 'lin',
                      cmissing = 'method2')


Y
#phenWin$combos
>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186

phenWin$combos


<<<<<<< HEAD
#phenweightwin <- weightwin(n =5,weightfunc = 'G',
#                           xvar = list(rain = wdat$Rainfall.mm),
#                           cdate = wdat$Date,
#                           bdate = dat_phen$datep,
 #                          baseline = glm(prop ~ 1, data = dat_phen, family = 'binomial'),
  #                         cinterval = 'week',
   #                        range = range,
    #                       type = 'relative',
     #                      func = 'lin',
      #                     cmissing = 'method2',
       #                    par = c(-2,8,-4) )


summary(phenWin[[5]]$BestModel)
 
plot(phenWin[[5]]$BestModelData$yvar~phenWin[[5]]$BestModelData$climate)




x <- c(-10:10)

y <- rgev(1000,loc = -8.33,scale = 1,shape = -4)

hist(y)

plot(jitter(phenweightwi[[1]]$BestModelData$prop)~phenweightwin[[1]]$BestModelData$climate)
mod1 <- phenWin[[5]]$BestModel
x <- seq(0,15,length.out = 100)
preddat <- predict(mod1,newdata = data.frame(climate = x),se.fit = TRUE)

with(preddat, lines(x, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))


phenOutput <- phenWin[[5]]$Dataset
=======
summary(phenWin[[1]]$BestModel)
summary(phenWin[[2]]$BestModel)
summary(phenWin[[3]]$BestModel)


summary(phenWin[[20]]$BestModel)




phenOutput <- phenWin[[1]]$Dataset

phenWin[[20]]$BestModelData

>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
plotdelta(phenOutput)
plotweights(phenOutput)
plotbetas(phenOutput)
plotwin(phenOutput)

<<<<<<< HEAD
randWin_phen <- randwin(repeats = 10, 
                        xvar = list(temp = temp),
=======
randWin_phen <- randwin(repeats = 5, 
                        xvar = list(mintemp = wdat$Min.temp),
>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
                        cdate = wdat$Date,
                        bdate = dat_phen$datep,
                        baseline = glm(prop ~ 1, data = dat_phen, family = 'binomial'),
                        cinterval = 'week',
                        range = range,
                        type = 'relative',
<<<<<<< HEAD
                        stat = 'sum',
                        func = 'lin',
                        binary = T,
                        lower = c(22),
                        cmissing = 'method2')



Y

#randWin_phen[[1]]

phenRand <- randWin_phen[[1]]

=======
                        stat = 'mean',
                        func = 'inv',
                        cmissing = 'method2')


Y

randWin_phen[[1]]

phenRand <- randWin_phen[[1]]

head(phenWin[[1]]$Dataset)



>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
plotdelta(phenOutput)
plotweights(phenOutput)
plotbetas(phenOutput)
plotwin(phenOutput)

<<<<<<< HEAD
phenSingle <- singlewin(xvar = list(temp = temp),
=======
phenSingle <- singlewin(xvar = list(mintemp = wdat$Min.temp),
>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
                        cdate = wdat$Date,
                        bdate = dat_phen$datep,
                        baseline = glm(prop ~ 1, data = dat_phen, family = 'binomial'),
                        cinterval = 'week',
<<<<<<< HEAD
                        range = c(17,0),
                        type = 'relative',
                        stat = 'sum',
                        func = 'lin',
                        binary = T,
                        lower = 22,
                        cmissing = 'method2')


Y
=======
                        range = c(21,2),
                        type = 'relative',
                        stat = 'mean',
                        func = 'inv', 
                        cmissing = 'method2')



>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
plotall(dataset = phenOutput,
        datasetrand = phenRand,
        bestmodel = phenSingle$BestModel, 
        bestmodeldata = phenSingle$BestModelData)


<<<<<<< HEAD
 plotbest(dataset = phenOutput,
=======
plotbest(dataset = phenOutput,
>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
         bestmodel = phenSingle$BestModel, 
         bestmodeldata = phenSingle$BestModelData)


plot(phenSingle$BestModelData$yvar~phenSingle$BestModelData$`I(climate^-1)`)

summary(phenSingle$BestModel)

<<<<<<< HEAD
################### All non binary model generator ######### 
=======

>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186

dat_sp <- dat %>% filter(phenophase %in% c('flower_bud','leaves_new','fruit_green'), !species_id %in% c(6,7,18))


slidingwinphen <- function(prop,date){
  phenmodel <- slidingwin(xvar = list(rain = wdat$Rainfall.mm, mintemp = wdat$Min.temp,maxtemp = wdat$Max.temp),
                          cdate = wdat$Date,
                          bdate = date,
                          baseline = glm(prop ~ 1, family = 'binomial'),
                          cinterval = 'week',
                          range = range,
                          type = 'relative',
                          stat = c('mean','min','max'),
                          func = c('lin','inv'))
  return(phenmodel)
}


<<<<<<< HEAD
#dat_sp_clim <- dat_sp %>% group_by(species_id, phenophase) %>% do(mods = slidingwinphen(.$prop,.$datep))

dat_sp_clim <- readRDS('phenology_slidingwindow_models_list.rds')

dat_sp_clim$combos <- map(dat_sp_clim$mods,'combos')

t <- unlist(dat_sp_clim$mods, recursive = F)

t <- t[-(c(1:45)*19)]

dat_sp_clim <- dat_sp_clim %>% unnest(.$combos)

dat_sp_clim$BestModel <- map(t,'BestModel')

dat_sp_clim$Dataset <- map(t,'Dataset')
dat_sp_clim$BestModelData <- map(t,'BestModelData')


combos <- dat_sp_clim %>% group_by(species_id,phenophase) %>% arrange(DeltaAICc) %>% slice(1:3) 

combos <- combos[,1:12]


dat_sp_clim$BestModel[[2]]

plotdelta(dat_sp_clim$Dataset[[1]])

plotweights(dat_sp_clim$Dataset[[1]])

#saveRDS(dat_sp_clim,'phenology_slidingwindow_models_list.rds')

######Cumulative onset -Tamarind ######

range = c(25,0)

cumdat <- read.csv('cumulative_onset.csv')

cumdatsp <- cumdat %>% filter(species_id == 1, phenophase == 'flower_bud')
cumdatsp$datep <- format(as.Date(cumdatsp$datep), '%d/%m/%Y')
cumdatsp$scalcumons <- as.numeric(cumdatsp$scalcumons)

cumdatsp <- cumdatsp %>% filter(ons >0)

temp <- (wdat$Min.temp+wdat$Max.temp)/2
phenWin <- slidingwin(xvar = list(temp = temp),
                      cdate = wdat$Date,
                      bdate = cumdatsp$datep,
                      baseline = glm(scalcumons/100 ~ 1, data = cumdatsp,family = 'binomial'),
                      cinterval = 'week',
                      range = range,
                      type = 'relative',
                      stat = 'sum',
                      func = 'lin',
                      binary = T,
                      upper = c(25:30),
                      cmissing = 'method2')



Y

phenWin$combos

plot(phenWin[[5]]$BestModelData$yvar~phenWin[[5]]$BestModelData$climate)

summary(phenWin[[5]]$BestModel)
phenOutput <- phenWin[[5]]$Dataset
plotdelta(phenOutput)
plotweights(phenOutput)
plotbetas(phenOutput)
plotwin(phenOutput)

randWin_phen <- randwin(repeats = 10, 
                        xvar = list(temp =temp),
                        cdate = wdat$Date,
                        bdate = cumdatsp$datep,
                        baseline = glm(scalcumons/100 ~ 1, data = cumdatsp,family = 'binomial'),
                        cinterval = 'week',
                        range = range,
                        type = 'relative',
                        stat = c('sum'),
                        func = 'lin',
                        binary = T,
                        upper = 29,
                        cmissing = 'method2')

y

phenRand <- randWin_phen[[1]]

phenSingle <- singlewin(xvar = list(temp = ((wdat$Min.temp+wdat$Max.temp)/2)),
                        cdate = wdat$Date,
                        bdate = cumdatsp$datep,
                        baseline = glm(scalcumons/100 ~ 1, data = cumdatsp,family = 'binomial'),
                        cinterval = 'week',
                        range = c(25,1),
                        type = 'relative',
                        stat = c('sum'),
                        func = 'lin',
                        binary = T,
                        upper = 29,
                        cmissing = 'method2')


Y
plotall(dataset = phenOutput,
        datasetrand = phenRand,
        bestmodel = phenSingle$BestModel, 
        bestmodeldata = phenSingle$BestModelData)


#########

cumdatsp <- cumdat %>% filter(species_id == 5, phenophase == 'flower_bud')
cumdatsp$datep <- format(as.Date(cumdatsp$datep), '%d/%m/%Y')
cumdatsp$scalcumons <- as.numeric(cumdatsp$scalcumons)

#cumdatsp <- cumdatsp %>% filter(ons > 0)

range(30,0)

phenWin <- slidingwin(xvar = list(temp = temp),
                      cdate = wdat$Date,
                      bdate = cumdatsp$datep,
                      baseline = glm(scalcumons/100 ~ 1, data = cumdatsp,family = 'binomial'),
                      cinterval = 'week',
                      range = range,
                      type = 'relative',
                      stat = c('sum'),
                      func = 'lin',
                      binary = T,
                      upper = c(20:30),
                      cmissing = 'method2')


y
phenWin$combos

plot(jitter(phenWin[[8]]$BestModelData$yvar)~phenWin[[8]]$BestModelData$climate)
mod1 <- phenWin[[8]]$BestModel
x <- seq(0,17,length.out = 100)
preddat <- predict(mod1,newdata = data.frame(climate = x),se.fit = TRUE)
with(preddat, lines(x, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))


summary(phenWin[[8]]$BestModel)
phenOutput <- phenWin[[8]]$Dataset
plotdelta(phenOutput)
plotweights(phenOutput)
plotbetas(phenOutput)
plotwin(phenOutput)


ranged = c(90,0)

phenWin <- slidingwin(xvar = list(temp = temp),
                      cdate = wdat$Date,
                      bdate = cumdatsp$datep,
                      baseline = glm(scalcumons/100 ~ 1, data = cumdatsp,family = 'binomial'),
                      cinterval = 'day',
                      range = ranged,
                      type = 'relative',
                      #                      stat = c('sum'),
                      func = 'lin',
                      binary = T,
                      upper = c(26:30),
                      cmissing = 'method2')



phenWin$combos

plot(jitter(phenWin[[1]]$BestModelData$yvar)~phenWin[[1]]$BestModelData$climate)
mod1 <- phenWin[[1]]$BestModel
x <- seq(0,20,length.out = 100)
preddat <- predict(mod1,newdata = data.frame(climate = x),se.fit = TRUE)
with(preddat, lines(x, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(x, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(x, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))


summary(phenWin[[1]]$BestModel)
phenOutput <- phenWin[[1]]$Dataset
plotdelta(phenOutput)
plotweights(phenOutput)
plotbetas(phenOutput)
plotwin(phenOutput)


randWin_phen <- randwin(repeats = 10, 
                        xvar = list(temp = temp),
                        cdate = wdat$Date,
                        bdate = cumdatsp$datep,
                        baseline = glm(scalcumons/100 ~ 1, data = cumdatsp,family = 'binomial'),
                        cinterval = 'week',
                        range = range,
                        type = 'relative',
                        stat = c('sum'),
                        func = 'lin',
                        binary = T,
                        upper = 21,
                        cmissing = 'method2')

y

phenRand <- randWin_phen[[1]]

phenSingle <- singlewin(xvar = list(temp = temp),
                        cdate = wdat$Date,
                        bdate = cumdatsp$datep,
                        baseline = glm(scalcumons/100 ~ 1, data = cumdatsp,family = 'binomial'),
                        cinterval = 'week',
                        range = c(7,1),
                        type = 'relative',
                        stat = c('sum'),
                        func = 'lin',
                        binary = T,
                        upper = 21,
                        cmissing = 'method2')


Y
plotall(dataset = phenOutput,
        datasetrand = phenRand,
        bestmodel = phenSingle$BestModel, 
        bestmodeldata = phenSingle$BestModelData)
 
=======
dat_sp_clim <- dat_sp %>% group_by(species_id, phenophase) %>% do(mods = slidingwinphen(.$prop,.$datep))

dat_sp_clim <- readRDS('phenology_slidingwindow_models_list.rds')


dat_sp_clim1$combos <- map(dat_sp_clim$mods,'combos')

map(dat_sp_clim$mods[[1]],'Dataset')

dat_sp_clim$bestmodel <- map(dat_sp_clim$mods[[1]],'BestModel')[1:18]

dat_sp_clim1 <- dat_sp_clim %>% unnest(.$combos, .preserve = 4)

saveRDS(dat_sp_clim,'phenology_slidingwindow_models_list.rds')








>>>>>>> 4c764ac054a21988cb7d3d298861439d7ea08186
