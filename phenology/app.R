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

wdat <- read.csv('D:/github repos/Phenology-analysis-2018/weather_man_aut_day_compiled.csv', stringsAsFactors = F)
wdat$datep <- as.Date(wdat$datep)


ui <- fluidPage(
   # Application title
   titlePanel("Rishi Valey Phenology"),
        fluidRow(column(4,selectInput('spcs','Select Species',choices = unique(phendat$species_id))),
                 column(4,selectInput('phen','Select Species',choices = unique(phendat$phenophase))),
                 column(4,sliderInput('raincut','Min rainfall',min = 0, max = 100, step = 1,value = 10))),
        plotOutput("phenplot", height = 600),
   plotOutput('rainplot')
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$phenplot <- renderPlot({
     phendat1 <- phendat %>% filter(species_id == input$spcs,phenophase == input$phen)
     wdat$year <- year(wdat$datep)
     raindat <- wdat %>% filter(year > 2006,Rainfall.mm >= input$raincut) %>% group_by(year)
     plot(phendat1$prop~as.Date(phendat1$datep), typ = 'l', ylab = 'Proportion',xlab = 'Date')
     lines(raindat$Rainfall.mm/100~raindat$datep, col = 'blue', typ = 'h')
     abline(v = as.Date(paste(c(2007:2017),'01','01',sep = '-')), col = 'orange',lty = 2)
     abline(h = 1, lty = 3, col = 'grey')
   })
   
   wdat1 <- reactive({
     wdat1 <- wdat %>% group_by(datep) %>% summarise(Rainfall.mm = sum(Rainfall.mm))
     wdat1$raindays <- 0
     wdat1$lastrain <- wdat1$Rainfall.mm
     for(i in 2:nrow(wdat1)){
       if(wdat1$Rainfall.mm[i]<20 & wdat1$Rainfall.mm[i-1]>input$raincut) wdat1$raindays[i] <- -1
       if(wdat1$Rainfall.mm[i]>input$raincut & wdat1$Rainfall.mm[i-1]>input$raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]+1
       if(wdat1$Rainfall.mm[i]<input$raincut & wdat1$Rainfall.mm[i-1]<input$raincut) wdat1$raindays[i] <- wdat1$raindays[i-1]-1
       if(wdat1$lastrain[i] < input$raincut) wdat1$lastrain[i] <- wdat1$lastrain[i-1]
     }
     wdat1
   })
   
   output$rainplot <- renderPlot({
     phendat1 <- phendat %>% filter(species_id == input$spcs,phenophase == input$phen)
     phendat1 <- left_join(phendat1,wdat1())
     plot(phendat1$prop~phendat1$raindays)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

