######### phenology sequence alignment
rm(list = c())
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
phendat1 <- phendat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n*100) 
strdat <- phendat %>% group_by(species_id,ind_id,phenophase) %>% summarise(phen = paste(value, collapse = '')) %>% droplevels()

phendat <- left_join(datesdat,phendat) %>% arrange(species_id,ind_id,phenophase,datep)
phendat1 <- left_join(datesdat,phendat1) %>% arrange(species_id,phenophase,datep)

test1 <- phendat %>% filter(!is.na(ind_id), !ind_id %in% c('10','10a'),species_id == 1) %>% group_by(ind_id,phenophase) %>% summarise(phen = paste(value,collapse = ''),id = paste(ind_id[1],phenophase[1], sep = '_'))
distmat <- adist(test1$phen,test1$phen)

distmat1 <- test1 %>% mutate(dist = phen %>% map(~adist(.x,test1$phen)) )


#distmatp <- adist(test1$phen,test1$phen, partial = T)

distmat <- as.matrix(distmat)
row.names(distmat) <- test1$id
colnames(distmat) <- test1$id
clust <- as.dist(distmat)

mds.coor <- cmdscale(clust)
plot(mds.coor[,1], mds.coor[,2], type="n", xlab="", ylab="")
text(jitter(mds.coor[,1]), jitter(mds.coor[,2]),
     rownames(mds.coor), cex=0.8)
abline(h=0,v=0,col="gray75")

x <- matrix(c(1,1,0,0), nrow = 2)
y <- matrix(c(1,0,1,0), nrow = 2)

library(hclust)

t <- hclust(clust)

plot(t,labels = F)
labels(t)


