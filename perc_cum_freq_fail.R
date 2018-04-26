library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
rm(list = ls())

dat <- read.csv('collated1.csv',stringsAsFactors = F)

dat$leaf_fall <- dat$leaves_old
library(plyr)
dat$leaf_fall <- mapvalues(dat$leaf_fall, from = c('Y','N',0,1,2), to = c(0,2,2,0,0))
phendat <- gather(dat[,-(2:5)], key = 'phenophase',value = 'value', c(3:8,12))
#data.frame(table(phendat$value))
unique(phendat$value)
phendat$value <- factor(phendat$value)
spinfo <- read.csv('C:/Users/Abinand Reddy/Desktop/RV Phenology/Raw data/species_info.csv')
phendat$value <- mapvalues(phendat$value,from = c('Y','N',0,1,2), to = c(1,0,0,1,1))
detach("package:plyr")
phendat$value <- as.numeric(as.character(phendat$value))


########################

smooth <- function(dt,fnd,repl){
  dt[is.na(dt)] <- 'n'
  temp <- paste(dt,collapse = '')
  temp <- strsplit(gsub(fnd,repl,temp),'')
  dt <- as.integer(temp[[1]])
  dt
}


t <- c(1,0,0,1,0,1,0,0,0,1,1,1)

smooth(t,'0100','0000')

onset <- function(v,d){
  d <- as.Date(d)
  
  v1 <- data.frame(values = v,dates = as.Date(d))
  
  v1 <- v1 %>% arrange(dates) %>% filter(!is.na(values))
  
#  v1$values <- smooth(v1$values, '1101','1111')
  
#  v1$values <- smooth(v1$values, '0100','0000')
  
  rv <- rle(v1$values)
  
  rv1 <- data.frame(rv[1],rv[2]) 
  
  rv1$cs <- cumsum(rv1$lengths) - rv1$lengths
  
  rv1 <- rv1 %>% filter(values == 1, lengths >= 1, cs > 0)
  
  ons <- data.frame(onset_date = as.Date(v1$dates[rv1$cs]),duration = as.numeric(rv1$lengths))
  
  return(ons)
}

onset_dat <- phendat %>% ungroup() %>% filter(!phenophase %in% c('leaves_old','fruit_brown')) %>% group_by(species_id,ind_id, phenophase)  %>% do(ons = onset(.$value,.$datep)) %>% unnest()

onset_dat <- onset_dat %>% separate(onset_date,c('year','month','day'), sep ='-', remove = F)

onset_dat$month <- factor(onset_dat$month)

#ggplot() + geom_bar(data = onset_dat, aes(month)) + facet_grid(phenophase~species_id) + ggtitle('Onset date of individuals in each phenophase')

t <- data.frame(table(onset_dat$species_id,onset_dat$phenophase,onset_dat$month, onset_dat$year))

colnames(t) <- c('spec','phen','month','year','freq')

#ggplot() + geom_line(data = t, aes(as.numeric(month),freq, col = year)) + facet_grid(phen~spec) + ggtitle('Onset date of individuals in each phenophase')

phenprop <- phendat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n*100) 

phenprop$year <- year(phenprop$datep)

phenmax <- phenprop  %>% 
  filter(!species_id %in% c(6,7,18), ! year == 2007, !phenophase %in% c('leaves_old','fruit_brown')) %>% 
  mutate(year = format(as.Date(datep),"%Y")) %>% 
  group_by(species_id,phenophase,year) %>% 
  filter(prop > (max(phenprop$prop)*3/4)|prop == min(prop)) %>% 
  mutate(minmax = ifelse(prop > (max(prop)*3/4),1,0), maxprop = max(prop)) %>%
  group_by(species_id,phenophase) %>% 
  mutate(start = minmax - lead(minmax))%>%
  filter(start == -1) 

phenmax <- phenmax %>% mutate(end = lead(datep,order_by = c(species_id)))

phenmax$end <- as.character(phenmax$end)

phenmax$end[is.na(phenmax$end)] <- as.Date(phenmax$datep[is.na(phenmax$end)]) + 300

phenmax$end <- as.Date(phenmax$end)

phenmax <- phenmax %>% filter(!year == 2007, prop < 11)

phenmax$month <- as.numeric(format(as.Date(phenmax$datep), '%m'))

#ggplot() + geom_boxplot(data = phenmax, aes(species_id,month, group = species_id)) + facet_grid(.~phenophase)  + ggtitle('Start date of each phenophase for population')

phenstart <- phenmax %>% select(species_id,phenophase,datep,end,year,maxprop)

colnames(phenstart)[3] <- 'startdate'

colnames(phenstart)[4] <- 'enddate'

phenstart


cumgen <- function(spec,phen,stdate,endate){
  d1 <- onset_dat %>% 
    filter(species_id == spec,phenophase == phen, between(as.Date(onset_date),as.Date(stdate),min(as.Date(endate),as.Date(stdate) +210))) %>% 
    arrange(onset_date) %>% 
    group_by(onset_date) %>% 
    group_by(ind_id) %>% 
    filter(onset_date == min(onset_date)) %>% 
    ungroup() %>% 
    group_by(onset_date) %>% 
    summarise(ons = n()) %>% 
    separate(onset_date,c('year','month','day'), sep ='-', remove = F)
  d2 <- expand.grid(species_id = spec, phenophase = phen,start_date = stdate,year = unique(d1$year),month = c(1:12),day = c(1,15)) %>% 
    mutate(datep = as.Date(paste(year,month,day,sep = '-'))) %>% 
    filter(between(datep,min(d1$onset_date)-30,max(d1$onset_date)+30)) %>% 
    arrange(datep) %>% 
    select(species_id,phenophase,start_date,datep) %>% 
    left_join(.,d1, by = c('datep' = 'onset_date')) %>% 
    select(species_id,phenophase,start_date,datep,ons) 
  d2$ons[is.na(d2$ons)] <- 0
  d2 <- d2 %>% mutate(cumons = cumsum(ons)) %>% mutate(scalcumons = round(cumons/max(cumons)*100,2))
  return(data.frame(d2))
}


phenstart <- phenstart %>% group_by(species_id,phenophase) %>% filter(n()>1) %>% group_by(species_id,phenophase,year) %>% filter(maxprop > 40)%>% arrange(startdate) %>% slice(1)

cumdat <- phenstart %>% rowwise() %>% do(cumpl = cumgen(.$species_id,.$phenophase,.$startdate,.$enddate)) %>% unnest()

cumdat$year_start <- as.Date(paste(year(cumdat$start_date),'01','01',sep ='-'))

cumdat$jul_datep <- as.numeric(cumdat$datep - cumdat$year_start)

cumdat1 <- cumdat %>% group_by(species_id,phenophase,year_start)

ggplot() + geom_line(data = cumdat1, aes(jul_datep,scalcumons, col = factor(year_start), group = factor(year_start))) + facet_grid(species_id~phenophase) +geom_vline(xintercept =365) + ggtitle('Scaled cumulative onset of phenophases')

get_date <- function(a,b,perc){(as.numeric((b[1] - b[2]))/(a[1]-a[2])*(perc-a[1])) + b[1]}


phenophase_percentile_date <- function(scaled_cum,dates,percentile){
  perc_index <- match(min(scaled_cum[scaled_cum >= percentile]),scaled_cum)
  return(as.Date(get_date(c(scaled_cum[perc_index-1],scaled_cum[perc_index]),c(dates[perc_index-1],dates[perc_index]),percentile)))
}

t <- cumdat1 %>% filter(species_id ==1,phenophase == 'flower_open',year_start == '2013-01-01')

phenophase_percentile_date(t$scalcumons,t$datep,75)

scl_cum_perc_dat <- cumdat1 %>% group_by(species_id,phenophase,year_start) %>% do(thp_25 = phenophase_percentile_date(.$scalcumons,.$datep,25),thp_50 = phenophase_percentile_date(.$scalcumons,.$datep,50),thp_75 = phenophase_percentile_date(.$scalcumons,.$datep,75))

scl_cum_perc_dat$thp_50 <- unlist(scl_cum_perc_dat$thp_50)
scl_cum_perc_dat$thp_25 <- unlist(scl_cum_perc_dat$thp_25)
scl_cum_perc_dat$thp_75 <- unlist(scl_cum_perc_dat$thp_75)

scl_cum_perc_dat$thp_25 <- as.Date(scl_cum_perc_dat$thp_25, origin = '1970-01-01')
scl_cum_perc_dat$thp_50 <- as.Date(scl_cum_perc_dat$thp_50, origin = '1970-01-01')
scl_cum_perc_dat$thp_75 <- as.Date(scl_cum_perc_dat$thp_75, origin = '1970-01-01')

scl_cum_perc_dat$thp_25_julian <- as.numeric(round(scl_cum_perc_dat$thp_25 -scl_cum_perc_dat$year_start,0))
scl_cum_perc_dat$thp_50_julian <- as.numeric(round(scl_cum_perc_dat$thp_50 -scl_cum_perc_dat$year_start,0))
scl_cum_perc_dat$thp_75_julian <- as.numeric(round(scl_cum_perc_dat$thp_75 -scl_cum_perc_dat$year_start,0))

write.csv(scl_cum_perc_dat,'onset_percentile_data.csv', row.names = F)




