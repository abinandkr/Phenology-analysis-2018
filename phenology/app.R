library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)

phendat <- read.csv('D:/github repos/Phenology-analysis-2018/collated1.csv', stringsAsFactors =  F)
phendat <- gather(phendat[,-(2:5)], key = 'phenophase',value = 'value', 3:8)
phendat$value <- factor(phendat$value)
library(plyr)
phendat$value <- mapvalues(phendat$value,from = c('Y','N',0,1,2), to = c(1,0,0,1,1))
detach("package:plyr")
phendat$value <- as.numeric(as.character(phendat$value))
phendat <- phendat %>% group_by(species_id,phenophase,datep) %>% summarise(n = n(),prop = sum(value, na.rm = T)/n) 
phendat <- phendat %>% filter(datep > '2007-11-31', datep < '2017-01-01') 
phendat$datep <- as.Date(phendat$datep)
phendat$month <- month(phendat$datep)
phendat$season <- cut(phendat$month,breaks = c(1,2,5,8,11,12), include.lowest = T)
library(plyr)
phendat$season <- mapvalues(phendat$season,from = c('[1,2]', '(2,5]', '(5,8]', '(8,11]','(11,12]'), to = c('cd','hd','sw','ne','cd'))
detach("package:plyr")

wdat <- read.csv('D:/github repos/Phenology-analysis-2018/weather_photoperiod.csv', stringsAsFactors = F)
wdat$datep <- as.Date(wdat$datep)

wdat$temp <- (as.numeric(wdat$Min.temp) + as.numeric(wdat$Max.temp))/2




ui <- fluidPage(
   # Application title
   titlePanel("Rishi Valey Phenology"),
        fluidRow(column(3,selectInput('spcs','Select Species',choices = unique(phendat$species_id))),
                 column(3,selectInput('phen','Select Species',choices = unique(phendat$phenophase))),
                 column(3,selectInput('weat','Select Weather variable',choices = c('rain' = 'Rainfall.mm', 'temp' = 'temp', 'mintemp' = 'Min.temp','maxtemp'='Max.temp'))),
                 column(3,sliderInput('raincut','Min weather',min = 0, max = 100, step = 1,value = c(10,40)))),
        plotOutput("phenplot", height = 600)
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$phenplot <- renderPlot({
     phendat1 <- phendat %>% filter(species_id == input$spcs,phenophase == input$phen)
     wdat$year <- year(wdat$datep)
     wdat$Date <- as.Date(wdat$Date)
     raindat <- wdat %>% filter(year > 2006) %>% select_(input$weat,'Date')
     raindat <- raindat[raindat[,1] >= input$raincut[1]&raindat[,1] <= input$raincut[2],]
     ifelse(input$weat == 'Rainfall.mm', weather <- as.numeric(raindat[,1])/100, weather <- as.numeric(raindat[,1])/50)
     date <- as.Date(raindat$Date)
     plot(phendat1$prop~as.Date(phendat1$datep), typ = 'l', ylab = 'Proportion',xlab = 'Date')
     lines(weather~date, col = 'blue', typ = 'h')
     lines((c(NA,diff(wdat$photoperiod)^2))~wdat$Date, typ = 'l', col = 'darkorange',lty = 3)
     abline(v = as.Date(paste(c(2007:2017),'01','01',sep = '-')), col = 'orange',lty = 2)
     abline(h = 1, lty = 3, col = 'grey')
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

