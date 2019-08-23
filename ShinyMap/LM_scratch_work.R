# plotting improvements

# data processing 
a <- filter(df2, species=="Selasphorus rufus" & year>=2010 & year<=2018)
b <- as.data.frame(table(a$year))
gbif_count <- c()  
for(i in seq(from = 2010, to = 2018, by = 1)){
  c <-  nrow(filter(df2, year==i))
  gbif_count <- c(gbif_count, c)
  
}
b <- cbind(b, gbif_count)

b <- mutate(b, normalized = Freq/gbif_count)

# the plot 
ggplot(b, aes(x = Var1, y = normalized, group = 1))+
  geom_line()+
  geom_point()+
  labs(title = "Reported Species Occurrence Over Time",
       x = "Year",
       y = "Reported Sightings")+
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 14))+
  expand_limits(y = 0)







# group 1
x <- filter(df2, species=="Selasphorus rufus" & year>=2010 & year<=2018)
y <- as.data.frame(table(x$year))
gbif_count <- c()  
for(i in seq(from = 2010, to = 2018, by = 1)){
  d <-  nrow(filter(df2, year==i))
  gbif_count <- c(gbif_count, d)
  print(gbif_count)
}
y <- cbind(y, gbif_count)

y <- mutate(y, normalized = Freq/gbif_count)








# group 2
a <- filter(df2, species=="Corvus caurinus" & year>=2010 & year<=2018)
b <- as.data.frame(table(a$year))
gbif_count2 <- c()  
for(i in seq(from = 2010, to = 2018, by = 1)){
  d <-  nrow(filter(df2, year==i))
  gbif_count2 <- c(gbif_count2, d)
  print(gbif_count2)
}
b <- cbind(b, gbif_count2)

b <- mutate(b, normalized = Freq/gbif_count2)
colnames(b) <- c("Var1", "Freq", "gbif_count", "normalized")



c <- rbind(y, b)
c$species <- c(rep("Selasphorus rufus", length(seq(from = 2010, to = 2018, by = 1))),
                   rep("Corvus caurinus", length(seq(from = 2010, to = 2018, by = 1))))


ggplot(c, aes(x = Var1, y = normalized, group = species))+
  geom_line(aes(color=species))+
  geom_point(aes(color = species))




# a test shiny app
ui <- fluidPage(
  titlePanel(title = "Species Observations Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "species", 
                  label = "Select a Species",
                  choices = unique(df2$species),
                  multiple = F)),
    mainPanel(
      plotOutput("chart"),
      leafletOutput("mymap",height = 1000)
    )
  )
  
  
)

server <- function(input,output, session){
  
  output$chart <- renderPlot({
    a <- filter(df2, species==input$species & year>=2010 & year<=2018)
    b <- as.data.frame(table(a$year))
    gbif_count <- c()  
    for(i in seq(from = 2010, to = 2018, by = 1)){
      d <-  nrow(filter(df2, year==i))
      gbif_count <- c(gbif_count, d)
    }
    b <- cbind(b, gbif_count)
    
    b <- mutate(b, normalized = Freq/gbif_count)
    
    ggplot(b, aes(x = Var1, y = normalized, group = 1))+
      geom_line()+
      geom_point()
    
  })
  
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
shinyApp(ui = ui, server=server)




