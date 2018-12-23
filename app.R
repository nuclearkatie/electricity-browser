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
library(shinyWidgets)

# Data
annual_state_electricity <- read_excel("electricity_data.xls", skip = 2)

#make as tibble, tidy
#annual_state_electricity <- as.tbl(annual_state_electricity)
annual_state_electricity <- annual_state_electricity %>% 
  mutate(Source = dplyr::recode(Source, 'Wood and Wood Derived Fuels'='Wood')) %>% 
  mutate(Source = dplyr::recode(Source, 'Hydroelectric Conventional'='Hydroelectric')) %>% 
  mutate(Source = dplyr::recode(Source, 'Solar Thermal and Photovoltaic'='Solar Thermal and PV')) 

annual_state_electricity <- annual_state_electricity %>% #group_by(Year) %>% group_by(State) %>% 
  mutate(Source = dplyr::recode(Source, "Other Biomass" = "Other")) %>% 
  #mutate(Source = dplyr::recode(Source, "Wood Derived Fuels" = "Other")) %>%
  #mutate(Source = recode(Source, "Geothermal" = "Other")) %>%
  mutate(Source = dplyr::recode(Source, "Pumped Storage" = "Other")) %>%
  mutate(Source = dplyr::recode(Source, "Other Gases" = "Other")) 

annual_state_electricity <- aggregate(data = annual_state_electricity, Gen~Year+State+Source+Type,FUN=sum)


# Only using total electric power industry
total_industry <- filter(annual_state_electricity, Type == "Total Electric Power Industry") %>% select(-Type)

# make column with state name
total_industry <- total_industry %>% rename(State_abbv =  State) %>% mutate(name = abbr2state(State_abbv)) %>% select(Year, name, State_abbv, Source, Gen)

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

#state_pop <-  as_tibble(getCensus(name="acs5", 
#                      vintage=2015,
#                      vars=c("NAME", "B01003_001E"), 
#                      region="state:*")) %>% rename(name = NAME, pop = B01003_001E)



################################################################################################################
################################################################################################################
################################################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
                
                tags$head(
                  tags$style(HTML(".leaflet-container { background: #f5f5f5d; }"))
                ),
                
                # Application title
                titlePanel("Where Does Your Electricity Come From?"),
                
                div("Electricity is fundamental to modern life in the U.S. We rely on it every time we flip the lights on, plug in our phones, turn on the coffeemaker, or do almost anything else. However, many of us get little formal education in where our electricity is coming from. Do you know the breakdown of energy sources in your state?"),
                #br(),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  position = "right",
                  sidebarPanel(
                    
                    
                    #checkbox
                    #checkboxInput(inputId = "show_data",
                    #              label = "Show data table",
                    #              value = FALSE),
                    #conditionalPanel(
                    #  condition = "input.show_data == true",
                    #  actionButton("update_data_table", "Update data table")
                    #),
                    h4("Key insights"),
                    radioGroupButtons(inputId = "keyinsight", 
                                      label = "Click a button to learn more",
                                      choices = c("None" = "", "1","2","3","4","5","6","7"),
                                      selected = "",
                                      #individual=TRUE,
                                      #inline = TRUE,
                                      #justified=TRUE,
                                      width = "100%"),
                    htmlOutput("key_insight"),
                    br(),
                    p("Note: You may have to scroll to see the options below the maps"),
                    
                    # Built with Shiny by RStudio
                    br(), br(), 
                    p("Built with",
                      img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                      "by",
                      img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", 
                          height = "25px"),
                      "."),
                    p("Data source: ", 
                      img(src ="http://www.ewriteonline.com/wp-content/uploads/2015/04/eia_logo_.png",
                          height = "30px")),
                    p("By Katie Mummah, University of Wisconsin-Madison")
                    
                  ),
                  
                  
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                tabPanel(tags$b("Largest Source of Energy"),
                                         leafletOutput("largestmap"),
                                         selectInput(input = "regularvsclean",
                                                     label = "Low-carbon sources only?",
                                                     choices = c("All sources" = "all", "Low-carbon only" = "lc"),
                                                     selected = "all"), 
                                         sliderInput(inputId = "year", 
                                                     label = "Year:", 
                                                     min = min_year, max = max_year, 
                                                     ticks = FALSE,
                                                     width = '100%',
                                                     sep = "",
                                                     value = 2017,
                                                     animate= animationOptions(interval = 600, loop = FALSE, playButton = NULL,
                                                                               pauseButton = NULL)),
                                         br(),
                                         h2(textOutput("datayear1")),
                                         DT::dataTableOutput(outputId = "largestdata")
                                ),
                                
                                tabPanel(tags$b("Percent use by source"),
                                         leafletOutput("percentmap"),
                                         #leafletOutput("mymap"),
                                         selectInput(inputId = "sources", label = "Electricity Source:", 
                                                     choices = c("Coal", "Natural Gas", "Hydroelectric", "Nuclear", "Wind",
                                                                 "Solar Thermal and PV", "Petroleum", "Geothermal", "Other")),
                                         sliderInput(inputId = "year1", 
                                                     label = "Year:", 
                                                     min = min_year, max = max_year, 
                                                     ticks = FALSE,
                                                     width = '100%',
                                                     sep = "",
                                                     value = 2017,
                                                     animate= animationOptions(interval = 600, loop = FALSE, playButton = NULL,
                                                                               pauseButton = NULL))
                                         #br()
                                ),
                                
                                tabPanel(tags$b("State-by-state breakdown"),
                                         fluidRow(   
                                           column(12,
                                                  HTML("<div style='height: 450px;'>"),
                                                  #HTML(textOutput("pastewafflesize")),
                                                  imageOutput(outputId = "waffle"),
                                                  HTML("</div>")
                                           )
                                         ), 
                                         #textOutput("pastewafflesize"),
                                         br(), 
                                         radioGroupButtons(inputId = "chooseastate", 
                                                           label = "Choose a state",
                                                           choices = c("AK","AL","AR","AZ","CA",
                                                                       "CO","CT","DC","DE","FL",
                                                                       "GA","HI","IA",
                                                                       "ID","IL","IN","KS","KY",
                                                                       "LA","MA","MD","ME","MI",
                                                                       "MN","MO","MS",
                                                                       "MT","NC","ND","NE","NH",
                                                                       "NJ","NM","NV","NY","OH",
                                                                       "OK","OR","PA",
                                                                       "RI","SC","SD","TN","TX",
                                                                       "UT","VA","VT","WA","WI",
                                                                       "WV","WY","US-Total"),
                                                           selected = "WI",
                                                           #individual=TRUE,
                                                           #inline = TRUE,
                                                           #justified=TRUE,
                                                           width = "100%")
                                )#,
                                #tabPanel("Data",
                                #        h2(textOutput("datayear")),
                                #       DT::dataTableOutput(outputId = "percenttable")
                                #      )
                    )
                  )
                ))


################################################################################################################
################################################################################################################
################################################################################################################

# Define server logic 
server <- function(input, output, session) {
  
  
  states_year <- reactive({
    req(input$year)
    total_industry %>% filter(Year == input$year) %>% 
      group_by(name) %>% mutate(tot = max(Gen)) %>% ungroup() %>% mutate(percent = (Gen / tot) * 100) %>% select(-tot) %>%
      filter(Source != "Total" & !(name == "US-Total" | name == "DC"))
  })
  
  #gen_per_capita
  #  pcdata <- reactive({
  #    req(input$year)
  #    total_industry %>% 
  #      filter(Year == input$year & Source %in% c("Total", "Other Gases", "Pumped Storage", "Other") & !(name == "US-Total" | name == "District of Columbia")) %>% 
  #      left_join(state_pop, by = 'name') %>% 
  #      mutate(plot_gen = Gen / pop) %>% 
  #      select(-state)
  #  })
  
  state_total_gen <- reactive({
    req(input$year)
    total_industry %>% filter(Year == input$year) 
  })
  
  percentdata <- reactive({
    req(input$year) 
    total_industry %>% filter(Year == input$year1 & !(name == "US-Total" | name == "District of Columbia")) %>% 
      group_by(name) %>% mutate(tot = max(Gen)) %>% ungroup() %>% mutate(percent = (Gen / tot) * 100) %>% select(-tot)
  })
  
  source_percentdata <- reactive({
    req(input$year)
    req(input$sources)
    percentdata() %>% filter(Source == input$sources)
  })
  
  largestdata <- reactive({
    largestdata_clean_checkbox() %>% group_by(name) %>% mutate(max_gen = max(Gen)) %>% filter(Gen == max_gen & !(name == "US-Total" | name == "DC")) %>% 
      ungroup() %>% select(-max_gen) %>% mutate(plot_Gen=Gen)
    #left_join(state_pop, by = 'name') %>% mutate(plot_gen = Gen / pop) %>% 
    #select(-state)
  })
  
  largestdata_clean_checkbox <- reactive({
    if(input$regularvsclean == "lc"){states_year() %>% filter(Source %in% c("Hydroelectric","Nuclear","Wind","Solar Thermal and PV", "Geothermal"))} else{states_year()}})
  
  largest_source_label <- reactive({
    if(input$regularvsclean == "lc"){"Largest clean source"} else if(input$regularvsclean == "all"){"Largest source"}
  })
  
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
    sprintf("<strong>%s, %s</strong><br/>%s: %.2f&#37;", states$name, input$year, largest_states_joined()$Source, largest_states_joined()$percent)) %>% 
      lapply(htmltools::HTML)
  })
  
  percent_labels <- reactive({paste0(
    sprintf("<strong>%s, %s</strong><br/>%s: %.2f&#37;", states$name, input$year, percentdata_joined()$Source, percentdata_joined()$percent)) %>% 
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
      addLegend(pal = pal, values = ~Source, opacity = 0.7, title = largest_source_label(),
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
  
  wafflesize <- reactive({
    width  <- session$clientData$output_waffle_width
    pixelratio <- session$clientData$pixelratio
    wafflesize <- width * pixelratio
  })
  
  
  output$waffle <- renderImage({
    width  <- session$clientData$output_waffle_width
    pixelratio <- session$clientData$pixelratio
    width_scaled <- width * pixelratio
    #width_scaled <- if(width_scaled > 2000){width_scaled * 0.17} 
    list(src = normalizePath(file.path('ironed_out', paste0(input$chooseastate,'_waffle.png'))), 
         width= width_scaled, 
         alt = paste0("waffle"))
  }, deleteFile = FALSE)
  
  
  output$pastewafflesize <- reactive({paste0("\"<div style='height:", wafflesize(), "px;'>\"")})
  
  
  
  
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
  
  dy <- eventReactive(input$update_data_table, {paste0(input$year, ", ", input$sources)})
  dy1 <- eventReactive(input$update_data_table, {paste0(input$year)})
  
  output$datayear <- renderText(dy())
  output$datayear1 <- renderText(dy1())
  
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
  
  output$key_insight <- renderText({
    if(input$keyinsight == "1")
    {"In 1990, all but 11 states had a fossil fuel source as their largest source of electricity. In 2017, it was still only 15 states. This is because a state's primary energy source is almost always going to come from a \"baseload\" or always-generating power source. Which two low-carbon sources are baseload? Hint: check out Washington (state) and Illinois"}
    else if(input$keyinsight == "4")
    {"Check out the percent use by source tab. In 1990, solar power was all but non-existent. Today, many states have some form of solar thermal or photovoltaic (PV) power on the grid, although it typically powers only a percent or less. However, several states have made great strides in brining up their solar generation in the last few years. Which states are these?"}
    else if(input$keyinsight == "3")
    {"Check out \"low-carbon sources only\" from the drop-down menu below the map (you may have to scroll down) on the first tab. What regional trends do you see?"}
    else if(input$keyinsight == "2")
    {"Click the play button on the right side of the year slider below the map (you may have to scroll down a little) and watch the map change over time. Have there been any big changes in the past few decades? There's been a definite increase in natural gas, but coal is still king in many states"}
    else if(input$keyinsight == "6")
    {"Go to the state-by-state breakdown tab and check out your home state (or your favorite state). What stands out to you? Now compare your states to others, which ones are overwhelmingly powered by one source (try West Virginia, Rhode Island, or Hawaii) and which states are more mixed (try New York, California, and Alabama)?"}
    else if(input$keyinsight == "5")
    {"Go to the percent use by source tab and check out petroleum. Most petroleum products are used for transportation (i.e. gasoline or jet fuel), heating and lighting (kerosene), and the petrochemical industry. However, remote places are more likely to rely on petroleum due to its very high energy density. Which states use a higher than average amount of petroleum? (hint: you may have to scroll out a bit)"}
    else if(input$keyinsight == "7")
    {"Under state-by-state breakdown, check out the US-total if you haven't. Are there any final takeaways from this chart?"}
  })
  
}

################################################################################################################
################################################################################################################
################################################################################################################

# Run the application 
shinyApp(ui = ui, server = server)

