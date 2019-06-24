library(leaflet)
library(sp)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-123.259378, lat=49.263710, popup="The birthplace of R")
m  # Print the map

leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))
setView()

df = data.frame(Lat = 1:10, Long = rnorm(10))
leaflet() %>% addCircles(data = df, lat = ~ Lat, lng = ~ Long)

m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))