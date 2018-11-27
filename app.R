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
#library(urbnmapr) not made for use with shiny
library(waffle)
library(leaflet)
library(maps)
library(geojson)
library(geojsonio)
library(tigris)
library(DT)
library(openintro)
library(censusapi)

# Data
annual_state_electricity <- read_excel("../annual_generation_state-2.xls", skip = 2)

#make as tibble, tidy
annual_state_electricity <- as.tbl(annual_state_electricity)
#annual_state_electricity <- annual_state_electricity %>% mutate(Source = recode(Source, "'Wood and Wood Derived Fuels'='Wood Derived Fuels'")) %>% mutate(Source = recode(Source, "'Hydroelectric Conventional'='Hydroelectric'")) %>% mutate(Source = recode(Source, "'Solar Thermal and Photovoltaic'='Solar Thermal and PV'")) 

# Only using total electric power industry
total_industry <- filter(annual_state_electricity, Type == "Total Electric Power Industry") %>% select(-Type)

# make column with state name
total_industry <- total_industry %>% rename(State_abbv =  State) %>% mutate(name = abbr2state(State_abbv)) %>%select(Year, name, State_abbv, Source, Gen)

# min and max year
min_year <- min(annual_state_electricity$Year)
max_year <- max(annual_state_electricity$Year)


#
# read state data
#

url <- "http://leafletjs.com/examples/choropleth/us-states.js"
# read as text file
doc <- readLines(url)
# remove the javascript assignment at the front 
doc2 <- gsub("var statesData = ", "", doc)
# write out as a temp file and read
write(doc2, file = "tempgeo.json")
states <- geojson_read("tempgeo.json", what = "sp")


#
# Prep for census data
#

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="YOURKEYHERE")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")


################################################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ISYE 601 Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    position = "right",
    sidebarPanel(
      sliderInput(inputId = "year", 
                  label = "Year:", 
                  min = min_year, max = max_year, 
                  ticks = FALSE,
                  sep = "",
                  value = 2015,
                  animate= animationOptions(interval = 750, loop = TRUE, playButton = NULL,
                       pauseButton = NULL)),
      
      #checkbox
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = FALSE),
      
      # Built with Shiny by RStudio
      br(), br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("mymap"),
      DT::dataTableOutput(outputId = "data")
    )
  )
)

################################################################################################################

#country_max_power <- max_power_source %>% left_join(states, by = "state_abbv") %>% 
#  ggplot() + geom_polygon(mapping = aes(x = long, y = lat, group = group, fill=factor(Source)), col = "black", size=0.2) +
#  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
#  #scale_alpha_continuous(name = "State Power", range = c(0.6,1)) +
#  scale_fill_discrete(name = "") +
#  ditch_the_axes +
#  labs(title = paste("Largest Electricity Generating Source in Each State", max_power_source$Year, sep=", "))

#View(states)

################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  states_year <- reactive({
    req(input$year)
    total_industry %>% filter(Year == input$year) %>% filter(Source != "Total")
  })
  
  state_pop <-  reactive({
    getCensus(name="acs5", 
              vintage = input$year,
              key=census_key, 
              vars=c("NAME", "B01003_001E"), 
              region="state:*") 
  })
  
  max_power_source <- reactive({
    states_year() %>% group_by(name) %>% mutate(max_gen = max(Gen)) %>% filter(Gen == max_gen & !(name == "US-Total" | name == "DC")) %>% 
      ungroup() %>% select(-max_gen)
  })
  
  states_joined <- reactive({
    req(max_power_source())
    geo_join(states, max_power_source(), by_sp = "name", by_df = "name", by = "name") 
  })
  
    
  output$mymap <- renderLeaflet({
    library(viridis) # My favorite palette for maps
    #wardpal <- colorFactor(viridis(7), domain = states_joined$Source)
    pal <- colorNumeric("Greens", domain=states_joined()$Gen)
    leaflet(states_joined()) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% addPolygons(fillColor = ~pal(states_joined()$Gen),
                                                                          weight = 2,
                                                                          opacity = 1,
                                                                          color = "white",
                                                                          fillOpacity = 0.7)
  })
  
  df <- eventReactive(input$show_data, {
    req(input$year)
    DT::datatable(data = data.frame(states_joined()), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$data <- DT::renderDataTable({
    if(input$show_data){
      df()
    }
    
  })
  
}

################################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)

