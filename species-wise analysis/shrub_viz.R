rm(list = ls())
library(tidyverse)
library(lubridate)


dat <- read.csv('collated_long.csv', stringsAsFactors = F)

dat <- dat %>% filter(species_id %in% c(9,10,12,15,16,17)) %>% filter(datep>'2011-12-31')

dat <- dat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n/2) 

dat$year <- year(dat$datep)

dat_eff <- dat %>% group_by(species_id,phenophase,year) %>% summarise(effort = sum(prop))

ggplot(dat_eff) + geom_boxplot(aes(x = factor(species_id), y = effort, group = species_id)) + facet_grid(~phenophase)



dat <- dat %>% filter(species_id %in% c(9,10,12,15,16,17))  %>% filter(datep > '2007-12-31')

dat$value[dat$value == 2] <- 1

dat <- dat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 

dat$year <- year(dat$datep)

dat_eff <- dat %>% group_by(species_id,phenophase,year) %>% summarise(effort = sum(prop))

ggplot(dat_eff) + geom_boxplot(aes(x = factor(species_id), y = effort, group = species_id)) + facet_grid(~phenophase)


ggplot(dat_eff) + geom_bar(aes(x = factor(species_id),y = effort, group = species_id), stat = 'identity') + facet_grid(phenophase~year)




