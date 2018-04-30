rm(list = ls())
library(climwin)
library(tidyverse)
library(zoo)

dat <- read.csv('collated1.csv', stringsAsFactors = F)
dat <- gather(dat[,-(2:5)], key = 'phenophase',value = 'value', 3:8)
dat$value <- factor(dat$value)
library(plyr)
dat$value <- mapvalues(dat$value,from = c('Y','N',0,1,2), to = c(1,0,0,1,1))
detach("package:plyr")
dat$value <- as.numeric(as.character(dat$value))
dat <- dat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 
dat <- dat %>% filter(datep > '2007-12-31', datep < '2017-01-01') 
dat$datep <- as.Date(dat$datep)

dat_phen <- dat %>% filter(species_id == 12, phenophase == 'flower_bud', !is.na(prop))
dat_phen$datep <- format(as.Date(dat_phen$datep), '%d/%m/%Y')
dat_phen$prop <- as.numeric(dat_phen$prop)

wdat <- read.csv('weather_man_aut_day_compiled.csv',stringsAsFactors = F)
wdat <- wdat %>% filter(Date > '2007-01-01')
wdat$Date <- as.Date(wdat$Date)
dates_df <- data.frame(Date = seq.Date(as.Date('2007-01-02'),as.Date('2017-08-01'), by = 1))
wdat <- left_join(dates_df,wdat)
wdat <- wdat %>% arrange(Date)
wdat$Max.temp <- na.spline(wdat$Max.temp)
wdat$Min.temp <- na.spline(wdat$Min.temp)



wdat$Rainfall.mm <- as.numeric(wdat$Rainfall.mm)
wdat <- wdat %>% filter(!is.na(Rainfall.mm))
wdat$Date <- format(as.Date(wdat$Date), '%d/%m/%Y')



wdat$Rainfall.mm[wdat$Rainfall.mm == 0] <- .001

range = c(40,0)


phenWin <- slidingwin(xvar = list(rain = wdat$Rainfall.mm, mintemp = wdat$Min.temp,maxtemp = wdat$Max.temp),
                      cdate = wdat$Date,
                      bdate = dat_phen$datep,
                      baseline = glm(prop ~ 1, data = dat_phen, family = 'binomial'),
                      cinterval = 'week',
                      range = range,
                      type = 'relative',
                      stat = 'mean',
                      func = 'lin',
                      cmissing = 'method2')


Y
#phenWin$combos

phenWin$combos


summary(phenWin[[1]]$BestModel)
summary(phenWin[[2]]$BestModel)
summary(phenWin[[3]]$BestModel)


summary(phenWin[[20]]$BestModel)




phenOutput <- phenWin[[1]]$Dataset

phenWin[[20]]$BestModelData

plotdelta(phenOutput)
plotweights(phenOutput)
plotbetas(phenOutput)
plotwin(phenOutput)

randWin_phen <- randwin(repeats = 5, 
                        xvar = list(mintemp = wdat$Min.temp),
                        cdate = wdat$Date,
                        bdate = dat_phen$datep,
                        baseline = glm(prop ~ 1, data = dat_phen, family = 'binomial'),
                        cinterval = 'week',
                        range = range,
                        type = 'relative',
                        stat = 'mean',
                        func = 'inv',
                        cmissing = 'method2')


Y

randWin_phen[[1]]

phenRand <- randWin_phen[[1]]

head(phenWin[[1]]$Dataset)



plotdelta(phenOutput)
plotweights(phenOutput)
plotbetas(phenOutput)
plotwin(phenOutput)

phenSingle <- singlewin(xvar = list(mintemp = wdat$Min.temp),
                        cdate = wdat$Date,
                        bdate = dat_phen$datep,
                        baseline = glm(prop ~ 1, data = dat_phen, family = 'binomial'),
                        cinterval = 'week',
                        range = c(21,2),
                        type = 'relative',
                        stat = 'mean',
                        func = 'inv', 
                        cmissing = 'method2')



plotall(dataset = phenOutput,
        datasetrand = phenRand,
        bestmodel = phenSingle$BestModel, 
        bestmodeldata = phenSingle$BestModelData)


plotbest(dataset = phenOutput,
         bestmodel = phenSingle$BestModel, 
         bestmodeldata = phenSingle$BestModelData)


plot(phenSingle$BestModelData$yvar~phenSingle$BestModelData$`I(climate^-1)`)

summary(phenSingle$BestModel)



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


dat_sp_clim <- dat_sp %>% group_by(species_id, phenophase) %>% do(mods = slidingwinphen(.$prop,.$datep))

dat_sp_clim <- readRDS('phenology_slidingwindow_models_list.rds')


dat_sp_clim1$combos <- map(dat_sp_clim$mods,'combos')

map(dat_sp_clim$mods[[1]],'Dataset')

dat_sp_clim$bestmodel <- map(dat_sp_clim$mods[[1]],'BestModel')[1:18]

dat_sp_clim1 <- dat_sp_clim %>% unnest(.$combos, .preserve = 4)

saveRDS(dat_sp_clim,'phenology_slidingwindow_models_list.rds')








