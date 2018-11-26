# ISYE 601 Final Project
# Fall 2018
# Katie Mummah

# Where does your electricity come from? 

library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggthemes)
library(readxl)
library(urbnmapr)
library(waffle)

# Data
annual_state_electricity <- read_excel("../annual_generation_state-2.xls", skip = 2)

#make as tibble
annual_state_electricity <- as.tbl(annual_state_electricity)

annual_state_electricity <- annual_state_electricity %>% mutate(Source = recode(Source, "'Wood and Wood Derived Fuels'='Wood Derived Fuels'")) %>% mutate(Source = recode(Source, "'Hydroelectric Conventional'='Hydroelectric'")) %>% mutate(Source = recode(Source, "'Solar Thermal and Photovoltaic'='Solar Thermal and PV'")) 


# min and max year
min_year <- min(annual_state_electricity$Year)
max_year <- max(annual_state_electricity$Year)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("ISYE 601 Project"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      position = "right",
      sidebarPanel(
        
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

