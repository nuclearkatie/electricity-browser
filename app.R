# ISYE 601 Final Project
# Fall 2018
# Katie Mummah

# Where does your electricity come from? 

library(shiny)
library(shinythemes)
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
library(viridis) 
library(waffle)

# Data
annual_state_electricity <- read_excel("electricity_data.xls", skip = 2)

#make as tibble, tidy
annual_state_electricity <- as.tbl(annual_state_electricity)
annual_state_electricity <- annual_state_electricity %>% mutate(Source = recode(Source, 'Wood and Wood Derived Fuels'='Wood')) %>% mutate(Source = recode(Source, 'Hydroelectric Conventional'='Hydroelectric')) %>% mutate(Source = recode(Source, 'Solar Thermal and Photovoltaic'='Solar Thermal and PV')) 

annual_state_electricity <- annual_state_electricity %>% #group_by(Year) %>% group_by(State) %>% 
  mutate(Source = recode(Source, "Other Biomass" = "Other")) %>% 
  mutate(Source = recode(Source, "Wood Derived Fuels" = "Other")) %>%
  mutate(Source = recode(Source, "Geothermal" = "Other")) %>%
  mutate(Source = recode(Source, "Pumpted Storage" = "Other")) %>%
  mutate(Source = recode(Source, "Other Gases" = "Other")) 

annual_state_electricity <- aggregate(data = annual_state_electricity, Gen~Year+State+Source+Type,FUN=sum)


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
Sys.setenv(CENSUS_KEY="b718c87cbb8f0984438f47eb48ac5000a126b054")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

state_pop <-  as_tibble(getCensus(name="acs5", 
                      vintage=2015,
                      vars=c("NAME", "B01003_001E"), 
                      region="state:*")) %>% rename(name = NAME, pop = B01003_001E)



################################################################################################################
################################################################################################################
################################################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
  
  # Application title
  titlePanel("Where Does Your Electricity Come From?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    position = "right",
    sidebarPanel(
      sliderInput(inputId = "year", 
                  label = "Year:", 
                  min = min_year, max = max_year, 
                  ticks = FALSE,
                  sep = "",
                  value = 2017,
                  animate= animationOptions(interval = 600, loop = FALSE, playButton = NULL,
                       pauseButton = NULL)),
      
      #checkbox
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = FALSE),
      conditionalPanel(
        condition = "input.show_data == true",
        actionButton("update_data_table", "Update data table")
      ),
      
      
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
      
      tabsetPanel(type = "tabs",
                  tabPanel("Largest Source of Energy",
                           selectInput(input = "regularvsclean",
                                       label = "Low-carbon sources only?",
                                       choices = c("All sources" = "all", "Low-carbon only" = "lc"),
                                       selected = "all"), 
                           leafletOutput("largestmap"),
                           imageOutput(outputId = "waffle"),
                           h2(textOutput("datayear1")),
                           DT::dataTableOutput(outputId = "largestdata")
                  ),
                  
                  tabPanel("Electricity use per capita",
                           selectInput(inputId = "sources", label = "Electricity Source:", 
                                       choices = c("Coal", "Natural Gas", "Hydroelectric", "Nuclear", "Wind",
                                                   "Solar Thermal and PV", "Petroleum", "Other Biomass")),
                          leafletOutput("percentmap"),
                           #leafletOutput("mymap"),
                          h2(textOutput("datayear")),
                          DT::dataTableOutput(outputId = "percenttable")
                      )
                  )
    )
  )
)


################################################################################################################
################################################################################################################
################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  dy <- eventReactive(input$update_data_table, {paste0(input$year, ", ", input$sources)})
  dy1 <- eventReactive(input$update_data_table, {paste0(input$year)})
  
  output$datayear <- renderText(dy())
  output$datayear1 <- renderText(dy1())
  
  states_year <- reactive({
    req(input$year)
    total_industry %>% filter(Year == input$year) %>% 
      filter(Source != "Total" & !(name == "US-Total" | name == "DC"))
  })
  
  #gen_per_capita
  pcdata <- reactive({
    req(input$year)
    total_industry %>% 
      filter(Year == input$year & Source %in% c("Total", "Other Gases", "Pumped Storage", "Other") & !(name == "US-Total" | name == "District of Columbia")) %>% 
      left_join(state_pop, by = 'name') %>% 
      mutate(plot_gen = Gen / pop) %>% 
      select(-state)
  })
  
  state_total_gen <- reactive({
    req(input$year)
    total_industry %>% filter(Year == input$year) 
  })
  
  percentdata <- reactive({
    req(input$year) 
    total_industry %>% filter(Year == input$year & !(name == "US-Total" | name == "District of Columbia")) %>% 
      group_by(name) %>% mutate(tot = max(Gen)) %>% ungroup() %>% mutate(percent = (Gen / tot) * 100) %>% select(-tot)
  })
  
  source_percentdata <- reactive({
    req(input$year)
    req(input$sources)
    percentdata() %>% filter(Source == input$sources)
  })
  
  largestdata <- max_power_source <- reactive({
    largestdata_clean_checkbox() %>% group_by(name) %>% mutate(max_gen = max(Gen)) %>% filter(Gen == max_gen & !(name == "US-Total" | name == "DC")) %>% 
      ungroup() %>% select(-max_gen) %>% left_join(state_pop, by = 'name') %>% 
      mutate(plot_gen = Gen / pop) %>% 
      select(-state)
  })
  
  largestdata_clean_checkbox <- reactive({
    if(input$regularvsclean == "lc"){states_year() %>% filter(Source %in% c("Hydroelectric","Nuclear","Wind","Solar Thermal and PV"))} else{states_year()}})
  
  # join data
  
  pc_states_joined <- reactive({
    geo_join(states, pcdata(), by_sp = "name", by_df = "name", by = "name") 
  })
  
  largest_states_joined <- reactive({
    geo_join(states, largestdata(), by_sp = "name", by_df = "name", by = "name") 
  })
  
  percentdata_joined <- reactive({
    geo_join(states, source_percentdata(), by_sp = "name", by_df = "name", by = "name")
  })
  
  # label data for hover
  
  pc_labels <- reactive({paste0(
    sprintf("<strong>%s</strong><br/>Gen per capita: %.2f", states$name, pc_states_joined()$plot_gen)) %>% 
      lapply(htmltools::HTML)
  })
  
  largest_labels <- reactive({paste0(
    sprintf("<strong>%s</strong><br/>%s", states$name, largest_states_joined()$Source)) %>% 
      lapply(htmltools::HTML)
  })
  
  percent_labels <- reactive({paste0(
    sprintf("<strong>%s</strong><br/>%s: %.2f&#37;", states$name, percentdata_joined()$Source, percentdata_joined()$percent)) %>% 
      lapply(htmltools::HTML)
  })
  
  ################################################################################################################
  ################################################################################################################
  ################################################################################################################

  output$mymap <- renderLeaflet({
      #wardpal <- colorFactor(viridis(7), domain = states_joined$Source)
      bins <- c(0,10,15,20,25,35,Inf)
      #pal <- colorNumeric("Greens", domain=states_joined()$plot_gen, bins=bins)
      pal <- colorBin("Blues", domain=pc_states_joined()$plot_gen, bins=bins)
      leaflet(pc_states_joined()) %>%
        setView(-96, 37.8, 4) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
        addPolygons(fillColor = ~pal(pc_states_joined()$plot_gen),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 3,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = pc_labels()) %>%
        addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                  position = "bottomright") 
      })
  
  largest_source_of_power_map <- renderLeaflet({
    #wardpal <- colorFactor(viridis(7), domain = states_joined$Source)
    bins <- c(0,10,15,20,25,35,Inf)
    #pal <- colorNumeric("Greens", domain=states_joined()$plot_gen, bins=bins)
    pal <- colorBin("Blues", domain=pc_states_joined()$plot_gen, bins=bins)
    leaflet(pc_states_joined()) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
      addPolygons(fillColor = ~pal(pc_states_joined()$plot_gen),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = pc_labels()) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright") 
  })
  
  mycols <- c( "#000004FF", "#440154FF", "#482878FF",  "#26828EFF", "#20A486FF",
               "#35B779FF", "#5DC863FF", "#8FD744FF", "#FDE725FF","#FDE725FF")
  
  #[1] "#440154FF" "#481F70FF" "#443A83FF" "#3B528BFF" "#31688EFF" "#287C8EFF" "#21908CFF" "#20A486FF" "#35B779FF" "#5DC863FF" "#8FD744FF"
  #[12] "#C7E020FF" "#FDE725FF"
  
  output$largestmap <- renderLeaflet({
    pal <- colorFactor(mycols, domain = states_year()$Source)
    #pal <- colorNumeric("Greens", domain=states_joined()$plot_gen, bins=bins)
    #pal <- colorBin("Blues", domain=states_joined()$plot_gen, bins=bins)
    leaflet(largest_states_joined()) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
      addPolygons(fillColor = ~pal(largest_states_joined()$Source),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = largest_labels()) %>%
      addLegend(pal = pal, values = ~Source, opacity = 0.7, title = NULL,
                position = "bottomright", labels =  ~states_year()$Source)
  })
  
  output$percentmap <- renderLeaflet({
    #wardpal <- colorFactor(viridis(7), domain = states_joined$Source)
    bins <- c(0,5,10,20,30,40,50,100)
    #pal <- colorNumeric("Greens", domain=states_joined()$plot_gen, bins=bins)
    pal <- colorBin("Blues", domain=percentdata_joined()$percent, bins=bins)
    leaflet(percentdata_joined()) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
      addPolygons(fillColor = ~pal(percentdata_joined()$percent),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = percent_labels()) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = "Percent",
                position = "bottomright") 
  })
  
  
  observeEvent(input$largestmap_shape_click, {
    click <- input$largestmap_shape_click
    print(click$featureId)
  })
  
  # data for waffle
  
  output$selected_state <- reactive({click$Id})
  
  waffle_state <- reactive({
    filter(states_year(), State == click$Id, Source != "Total", Source != "Other Gases", Source != "Pumped Storage", Source != "Other")
  })
  
  waffle_setup <- reactive({
    waffle_state() %>%  mutate(percent_gen = Gen * 100 / sum(Gen), percent_gen_rounded = round(norm_gen, digits = 2)) %>%
      mutate(Source_with_percentage = paste0(paste(Source, percent_gen_rounded, sep= " = "),"%"))
  })
  
  waffle <- reactive({
    data.frame(
      names = waffle_setup()$Source_with_percentage,
      vals = waffle_setup()$percent_gen*2
  ) })
  
  parts <- c(80, 30, 20, 10)
  
  output$waffle <- renderImage({
    width  <- session$clientData$output_waffle_width
    pixelratio <- session$clientData$pixelratio
    
    
    list(src = normalizePath(file.path('.', 'Arizona_waffle.png')), width= width * pixelratio, alt = paste0("waffle"))
  }, deleteFile = FALSE)

  
  
  
  
  
  
  
  pcdf <- eventReactive(input$update_data_table, {
    req(input$year)
    DT::datatable(data = data.frame(pc_states_joined()), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$pcdata <- DT::renderDataTable({
    if(input$show_data){
      pcdf()
    }
  })
  
  ####
  
  largest_states_table <- reactive({
    largestdata() %>% select("name", "Source", "Gen") %>% rename("State" = "name", "Largest Source" = "Source", "Generation (MWh)" = "Gen")
  })
  
  largestdf <- eventReactive(input$update_data_table, {
    req(input$year)
    DT::datatable(data = data.frame(largest_states_table()), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$largestdata <- DT::renderDataTable({
    if(input$show_data){
      largestdf()
    }
  })
  
  ####
  
  percent_table <- reactive({
    source_percentdata() %>% select("name", "Gen") %>% rename("State" = "name", "Generation (MWh)" = "Gen")
  })
  
  percentdf <- eventReactive(input$update_data_table, {
    req(input$year)
    DT::datatable(data = data.frame(percent_table()), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$percenttable <- DT::renderDataTable({
    if(input$show_data){
      percentdf()
    }
  })
  
}

################################################################################################################
################################################################################################################
################################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)

