library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
rm(list = ls())


bnm <- function(x){
  y <- exp(x)/(1+exp(x))
  return(y)
}

dat <- read.csv('C:/Users/Abinand Reddy/Desktop/RV Phenology/Analysis/collated1.csv',stringsAsFactors = F)

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
  
  v1$values <- smooth(v1$values, '1101','1111')
  
  v1$values <- smooth(v1$values, '0100','0000')
  
  rv <- rle(v1$values)
  
  rv1 <- data.frame(rv[1],rv[2]) 
  
  rv1$cs <- cumsum(rv1$lengths) - rv1$lengths
  
  rv1 <- rv1 %>% filter(values == 1, lengths > 2, cs > 0)
  
  ons <- data.frame(onset_date = as.Date(v1$dates[rv1$cs]),duration = as.numeric(rv1$lengths))
  
  return(ons)
}

onset_dat <- phendat %>% ungroup() %>% filter(!phenophase %in% c('leaves_old','fruit_brown')) %>% group_by(species_id,ind_id, phenophase)  %>% do(ons = onset(.$value,.$datep)) %>% unnest()

onset_dat <- onset_dat %>% separate(onset_date,c('year','month','day'), sep ='-', remove = F)

onset_dat$month <- factor(onset_dat$month)

ggplot() + geom_bar(data = onset_dat, aes(month)) + facet_grid(phenophase~species_id) + ggtitle('Onset date of individuals in each phenophase')

t <- data.frame(table(onset_dat$species_id,onset_dat$phenophase,onset_dat$month, onset_dat$year))

colnames(t) <- c('spec','phen','month','year','freq')

ggplot() + geom_line(data = t, aes(as.numeric(month),freq, col = year)) + facet_grid(phen~spec) + ggtitle('Onset date of individuals in each phenophase')

phenprop <- phendat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n*100) 

phenprop$year <- year(phenprop$datep)

phenmax <- phenprop  %>% 
  filter(!species_id %in% c(6,7,18), ! year == 2007, !phenophase %in% c('leaves_old','fruit_brown')) %>% 
  mutate(year = format(as.Date(datep),"%Y")) %>% 
  group_by(species_id,phenophase,year) %>% 
  filter(prop %in% c(max(prop),min(prop))) %>% 
  mutate(minmax = ifelse(prop == max(prop),1,0), maxprop = max(prop)) %>%
  group_by(species_id,phenophase) %>% 
  mutate(start = minmax - lead(minmax)) %>%
  filter(start == -1) 

phenmax <- phenmax %>% mutate(end = lead(datep,order_by = c(species_id)))

phenmax$end <- as.character(phenmax$end)

phenmax$end[is.na(phenmax$end)] <- as.Date(phenmax$datep[is.na(phenmax$end)]) + 300

phenmax$end <- as.Date(phenmax$end)

phenmax <- phenmax %>% filter(!year == 2007, prop < 11)

phenmax$month <- as.numeric(format(as.Date(phenmax$datep), '%m'))

ggplot() + geom_boxplot(data = phenmax, aes(species_id,month, group = species_id)) + facet_grid(.~phenophase)  + ggtitle('Start date of each phenophase for population')

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


phenstart <- phenstart %>% group_by(species_id,phenophase) %>% filter(n()>3) %>% group_by(species_id,phenophase,year) %>% filter(maxprop > 40)%>% arrange(startdate) %>% slice(1)
  
cumdat <- phenstart %>% rowwise() %>% do(cumpl = cumgen(.$species_id,.$phenophase,.$startdate,.$enddate)) %>% unnest()

cumdat$year_start <- as.Date(paste(year(cumdat$start_date),'01','01',sep ='-'))

cumdat$jul_datep <- as.numeric(cumdat$datep - cumdat$year_start)

cumdat1 <- cumdat %>% group_by(species_id,phenophase,year_start) %>% filter(max(cumons) > 15)

ggplot() + geom_line(data = cumdat1, aes(jul_datep,scalcumons, col = factor(year_start), group = factor(year_start))) + facet_grid(species_id~phenophase) +geom_vline(xintercept =365) + ggtitle('Scaled cumulative onset of phenophases')

get_date <- function(a,b,perc){(as.numeric((b[1] - b[2]))/(a[1]-a[2])*(perc-a[1])) + b[1]}


phenophase_percentile_date <- function(scaled_cum,dates,percentile){
  perc_index <- match(min(scaled_cum[scaled_cum >= percentile]),scaled_cum)
  return(get_date(c(scaled_cum[perc_index-1],scaled_cum[perc_index]),c(dates[perc_index-1],dates[perc_index]),percentile))
}

t <- cumdat1 %>% filter(species_id ==1,phenophase == 'flower_open',year_start == '2013-01-01')

phenophase_percentile_date(t$scalcumons,t$datep,50)

cumdat1$year <- year(cumdat1$year_start)

phen_perc_dat <- cumdat1 %>% group_by(species_id,phenophase,year) %>% summarise(perc_dat = phenophase_percentile_date(scalcumons,datep,50))

phen_perc_dat$julian <- round(phen_perc_dat$perc_dat - as.Date(paste(phen_perc_dat$year,'01-01',sep = '-')),0)

ggplot() + geom_boxplot(data = phen_perc_dat, aes(phenophase,julian)) + facet_grid(.~species_id)  + ggtitle('Median Onset date of populations in each phenophase')

############## coefficient of variation table for median onset
phen_var <- phen_perc_dat %>% group_by(species_id,phenophase) %>% summarise(var = var(julian), sd = sd(julian))
##############
t <- onset_dat %>% group_by(species_id,phenophase,ind_id,year) %>% summarise(n = n(), dur = paste0(duration,collapse = ','))


############## coefficient of variation table for duration
dur_dat <- onset_dat %>% group_by(species_id,phenophase,ind_id,year) %>% summarise(ind_dur = sum(duration)) %>% 
  group_by(species_id,phenophase,year) %>% summarise(mean_dur = mean(ind_dur), var_dur = var(ind_dur),sd_dur = sd(ind_dur)) %>%
  mutate(coef_var = var_dur/mean_dur*100) %>%   filter(!species_id %in% c(6,7,18), ! year == 2007, !phenophase %in% c('leaves_old','fruit_brown'))

nind_phen <- phendat %>% filter(value == 1) %>% mutate(year = year(datep)) %>% filter(!year == 2007) %>% group_by(species_id,phenophase,year) %>% summarise(n_ind = n_distinct(ind_id)) %>% group_by(species_id,phenophase) %>% summarise(mean_ind = mean(n_ind),var_ind = var(n_ind),sd_ind = sd(n_ind)) %>% mutate(coef_var_ind = var_ind/mean_ind*100) %>% filter(!phenophase %in% c('leaf_fall','leaves_old','fruit_brown'))

phendat_sp <- phendat %>% filter(species_id ==1, ind_id == 1)

lag_extract <- function(species,ind, phen1, phen2){
  phendat_sp_phen1 <- phendat %>% filter(species_id == species[1], ind_id == ind[1],phenophase == phen1, !is.na(value))
  phendat_sp_phen2 <- phendat %>% filter(species_id == species[1], ind_id == ind[1],phenophase == phen2, value == 1)
  t <- rle(phendat_sp_phen1$value)
  phen1rle <- data.frame(lengths = t[1], value = t[2]) 
  phen1rle$lengths <- cumsum(phen1rle$lengths) 
  phen1rle <- phen1rle %>% filter(values == 0)
  if(max(phen1rle$lengths) == nrow(phendat_sp_phen1)) {phen1rle <- phen1rle[-nrow(phen1rle),]} 
  if(nrow(phen1rle) <= 1 | nrow(phen1rle) == 0) {
    lagdat <- data.frame(phen1 = NA, phen2 = NA, lag = NA)
    colnames(lagdat) <- c(phen1,phen2,'lag')
    return(lagdat)
    }
  lagdat <- data.frame(phen1 = phendat_sp_phen1$datep[phen1rle$lengths+1],phen2 = NA, stringsAsFactors = F) %>% filter(!is.na(phen1))
  for(i in 1:nrow(lagdat)){
    lagdat$phen2[i] <- min(phendat_sp_phen2$datep[phendat_sp_phen2$datep >= lagdat$phen1[i]])
  }

  lagdat$lag <- as.numeric(as.Date(lagdat$phen2) - as.Date(lagdat$phen1))
  colnames(lagdat) <- c(phen1,phen2,'lag')
  return(lagdat)
}


flowerlagdat <- phendat %>% filter(!species_id %in% c(6,7,18)) %>% group_by(species_id,ind_id) %>% do(lag_extract(.$species_id,.$ind_id,'flower_bud','flower_open')) 

flowerlagdat1 <- flowerlagdat %>% group_by(species_id,ind_id,flower_open) %>% arrange(lag) %>% slice(1) %>% filter(lag < 300) %>% separate(flower_bud,into = c('Year','Month','Day'),remove = F) 

ggplot(data = flowerlagdat1, aes(x = Year,y = lag)) +geom_boxplot() + facet_wrap(~species_id, scales = 'free') + ggtitle('Onset lag from flower bud to flower open')

fruitlagdat <- phendat %>% group_by(species_id,ind_id) %>% do(lag_extract(.$species_id,.$ind_id,'fruit_green','fruit_brown'))

fruitlagdat1 <- fruitlagdat %>% group_by(species_id,ind_id,fruit_brown) %>% arrange(lag) %>% slice(1) %>% filter(lag < 300)%>% separate(fruit_brown,into = c('Year','Month','Day'),remove = F) 

ggplot(data = fruitlagdat1, aes(x = Year,y = lag)) +geom_boxplot() + facet_wrap(~species_id, scales = 'free')+ ggtitle('Onset lag from unripe to ripe fruit')

flfrlagdat <-  phendat %>% group_by(species_id,ind_id) %>% do(lag_extract(.$species_id,.$ind_id,'flower_open','fruit_green'))

flfrlagdat1 <- flfrlagdat %>% group_by(species_id,ind_id,fruit_green) %>% arrange(lag) %>% slice(1) %>% filter(lag < 300) %>% separate(flower_open,into = c('Year','Month','Day'),remove = F) 

ggplot(data = flfrlagdat1, aes(x = Year,y = lag)) +geom_boxplot() + facet_wrap(~species_id, scales = 'free')+ ggtitle('Onset lag from flower open to unripe fruit')


lfbudlagdat <-  phendat %>% group_by(species_id,ind_id) %>% do(lag_extract(.$species_id,.$ind_id,'leaves_new','flower_bud'))

lfbudlagdat1 <- lfbudlagdat %>% group_by(species_id,ind_id,flower_bud) %>% arrange(lag) %>% slice(1) %>% filter(lag < 300) %>% separate(leaves_new,into = c('Year','Month','Day'),remove = F) 

ggplot(data = lfbudlagdat1, aes(x = Year,y = lag)) +geom_boxplot() + facet_wrap(~species_id, scales = 'free')+ ggtitle('Onset lag from leaf flush to flower bud')



################weather

weather <- read.csv('C:/Users/Abinand Reddy/Desktop/RV Phenology/Analysis/August 2017 compiled.csv')

colnames(weather) <- c("Date","Time","AM/PM","Wind.Direction","PAR","Rain.mm","Voltage","Current","Wind.Speed","Gust.Speed","Temp","RH","Batt.V")

#write.csv(weather,'August 2017 compiled.csv', row.names = F)

weather$datep <- ifelse(day(weather$Date) < 15, paste(year(weather$Date),month(weather$Date),'01',sep = '-'),paste(year(weather$Date),month(weather$Date),'15',sep = '-'))

weather$datep <- as.Date(weather$datep)

#weathersum <- weather %>% filter(Temp< 50) %>% group_by(datep) %>% summarise(rainfall = sum(Rain.mm, na.rm = T), mintemp = min(Temp, na.rm = T), maxtemp = max(Temp, na.rm = T))

manweather <- read.csv('C:/Users/Abinand Reddy/Desktop/RV Phenology/Analysis/rvs weather 2014.csv', stringsAsFactors = F)
manweather$Date <- as.Date(manweather$Date, format='%d.%m.%Y')
manweather$datep <- ifelse(day(manweather$Date) < 15, paste(year(manweather$Date),month(manweather$Date),'01',sep = '-'),paste(year(manweather$Date),month(manweather$Date),'15',sep = '-'))
manweather <- manweather %>% filter(Date > '2007-12-01', Date < min(weather$Date))
manweather$Max.temp <- as.numeric(manweather$Max.temp)
manweather$Min.temp <- as.numeric(manweather$Min.temp)
manweather$Rainfall.mm <- as.numeric(manweather$Rainfall.mm)
manweather <- manweather %>% select(Date,Rainfall.mm,Min.temp,Max.temp,datep)


#weather1 <- weather %>% filter(Temp..Â.C < 50) %>% group_by(Date) %>% summarise(maxtemp = max(Temp..Â.C), mintemp = min(Temp..Â.C)) 

#plot(as.Date(weather1$Date),weather1$maxtemp, typ = 'l', col = 'red', ylim = c(0,50))
#lines(as.Date(weather1$Date),weather1$mintemp, typ = 'l', col = 'blue')
