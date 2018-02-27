###### Extracting onset information 
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())


smooth <- function(dt,fnd,repl){
  dt[is.na(dt)] <- 'n'
  temp <- paste(dt,collapse = '')
  temp <- strsplit(gsub(fnd,repl,temp),'')
  dt <- as.integer(temp[[1]])
  dt
}


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


dat <- read.csv('C:/Users/Abinand Reddy/Desktop/RV Phenology/Analysis/collated1.csv')

phendat <- gather(dat[,-(2:5)], key = 'phenophase',value = 'value', 3:8)

#data.frame(table(phendat$value))

unique(phendat$value)

phendat$value <- factor(phendat$value)

spinfo <- read.csv('C:/Users/Abinand Reddy/Desktop/RV Phenology/Raw data/RV_phenology_cleaned_2016/species_info.csv')

#valuepa are values matched to 0,1 or simply just presence and absence
phendat$valuepa <- mapvalues(phendat$value,from = c('Y','N',0,1,2), to = c(1,0,0,1,1))

phendat$value.02 <- mapvalues(phendat$value,from = c('Y','N',0,1,2), to = c(0,0,0,0,1))

phendat$valuepa <- as.numeric(as.character(phendat$valuepa))

phendat$value.02 <- as.numeric(as.character(phendat$value.02))

phendat1 <- phendat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop.pa = sum(valuepa, na.rm = T)/n*100, prop.02 = sum(value.02, na.rm = T)/n*100) 

phendat1 <- left_join(phendat1,spinfo)

phendat1$phenophase <- factor(phendat1$phenophase, levels = c('flower_bud','flower_open','fruit_green','fruit_brown','leaves_new','leaves_old'))

phendat1$species_name <- factor(phendat1$species_name, levels = spinfo$species_name)

ggplot(data = phendat1)+ geom_line(aes(x = as.Date(datep), y = prop.02, col = 'dark red')) + geom_line(aes(x = as.Date(datep), y = prop.pa)) +facet_grid(species_name~phenophase) + theme_bw()+ theme(strip.text.y = element_text(angle = 360))


########################

#flowerdat <- phendat %>% filter(phenophase %in% c('flower_bud')) 

#flowering <- flowerdat %>%  group_by(datep,species_id,tag_id) %>% summarise(phenophase = 'flower', valuepa = max(valuepa))


onset_dat <- phendat %>% ungroup() %>% group_by(species_id,ind_id,phenophase) %>% filter(!species_id %in% c(6,7,18)) %>% do(ons = onset(.$valuepa,.$datep)) %>% unnest()
