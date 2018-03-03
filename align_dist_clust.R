######### phenology sequence alignment
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

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
phendat1 <- phendat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n*100) 

phendat <- left_join(datesdat,phendat) %>% arrange(species_id,ind_id,phenophase,datep)
phendat1 <- left_join(datesdat,phendat1) %>% arrange(species_id,phenophase,datep)
