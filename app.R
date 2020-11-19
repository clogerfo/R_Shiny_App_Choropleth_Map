#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)

states <- readRDS("states_complete.Rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("MLB Free Agents by State (2010 - 2017)"),
   
   fluidRow(
     column(7, offset = 1,
            br(),
            div(h4(textOutput("title"), align = "center"), style = "color:black"),
            div(h5(textOutput("period"), align = "center"), style = "color:black"),
            br())),
   fluidRow(
     column(7, offset = 1,
            leafletOutput("map", height="530")),
     column(3,
            uiOutput("categoryOut", align = "left")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #render the map
  output$map <- renderLeaflet({
    leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) #%>% addPolygons()
  })
  
  #map observe. Add color, interaction, and legend to map (basically fill in the data)
  observe <- ({
    
    labels <- sprintf(
      "<strong><center>%s</center></strong> <center>%s Unique Free Agent Contracts</center><br/>
      %g Million: AAV FA Contract (USD)<br/>
      %g Million: State's Largest Contract by AAV (USD)<br/>%s : Player Receiving Top Contract <br/>
      %s : State's Most Common Position",
      states@data$NAME, states@data$num_players, states@data$mean_sal, states@data$top_contract_aav,
      states@data$name, states@data$position
    ) %>% lapply(htmltools::HTML)
    
    bins <- c(0, .001, 3, 6, 8, Inf)
    pal <- colorBin("YlOrRd", domain = states@data$mean_sal, bins = bins)
    
    leafletProxy("map", data = states) %>%
      addPolygons( fillColor = ~pal(mean_sal),
                   weight = 2, opacity = 1,
                   color = "white", dashArray = "3", fillOpacity = 0.7,
                   highlight = highlightOptions( weight = 5, color = "#666",
                                                 dashArray = "", fillOpacity = .7,
                                                 bringToFront = TRUE),
                   label = labels,
                   labelOptions = labelOptions(style = list(
                     "font-weight" = "normal", padding = "3px 8px"), textsize = "15px",
                     direction = "auto"),
                   layerId = ~states$region) %>%
      addLegend(pal = pal, values = ~mean_sal, opacity = 0.7,
                title = "AAV FA Contarct (Millions)", position = "bottomright")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

