library(tidyverse)
library(lubridate)

dat <- read.csv('D:/github repos/Phenology-analysis-2018/onset_percentile_data.csv', stringsAsFactors = F)
dat$year <- year(dat$year_start)

wdat <- read.csv('D:/github repos/Phenology-analysis-2018/weather_man_aut_day_compiled.csv')
wdat$year <- year(wdat$Date)
wdat <- wdat %>% filter(year > 2007)
wdat$julian <- as.numeric(as.Date(wdat$Date) - as.Date(paste(wdat$year,'01','01',sep = '-')))



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   titlePanel("percentile data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput('spcs','select species',choices = unique(dat$species_id)),
         selectInput('phen','select phenophase',choices = unique(dat$phenophase)),
         sliderInput('raincut','Magnitude first rainfall',min = 0,max = 100,step = 1, value = 10)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("firstrain",width =  800,height = 800)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$firstrain <- renderPlot({
     spc_dat <- dat %>% filter(species_id == input$spcs, phenophase == input$phen)
     raindat <- wdat %>% filter(Rainfall.mm >= input$raincut) %>% group_by(year) %>% slice(1)
     spc_dat <- left_join(spc_dat,raindat)
     spc_dat$julian <- spc_dat$julian - 15
     plot(spc_dat$thp_50_julian~spc_dat$julian, xlim = c(0,200), ylim = c(0,200), xlab = 'Days to first rain', ylab = 'nth percentile population onset day', pch = 16)
     par(new = TRUE)
     plot(spc_dat$thp_25_julian~spc_dat$julian, xlim = c(0,200), ylim = c(0,200), col = 'blue', xlab = NA, ylab = NA)
     par(new = TRUE)
     plot(spc_dat$thp_75_julian~spc_dat$julian, xlim = c(0,200), ylim = c(0,200), col = 'red', xlab = NA, ylab = NA)
     text(spc_dat$julian,spc_dat$thp_50_julian,labels = spc_dat$year, cex = .7, pos = 2)
     legend(170,25,c('25th percentile','50th percentile','75th percentile'), col = c('blue','black','red'), pch = c(1,16,1), bty = 'n')
     abline(1,1, lty = 2)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

