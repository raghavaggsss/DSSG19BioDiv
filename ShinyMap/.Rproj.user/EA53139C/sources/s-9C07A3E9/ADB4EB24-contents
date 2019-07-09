server <- function(input,output, session){
  
  data <- reactive({
    x <- df
  })
  
  output$mymap <- renderLeaflet({
    df <- data()
    
    m <- leaflet(data = df) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude,
                 lat = ~Latitude,
                 popup = paste("Species", df$species, "<br>",
                               "Common Name:", df$common),
                 clusterOptions = markerClusterOptions())
    m
  })
}