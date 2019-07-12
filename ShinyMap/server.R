server <- function(input,output, session){
  output$plot1 = renderPlot({
    x = df2 %>%
      filter(species %in% input$species, year>=input$year[1], year<=input$year[2]) %>%
      group_by(species, year) %>%
      tally() %>%
      drop_na() %>%
      add_zeros()
    y <- yearly_obs[which(yearly_obs$year %in% x$year),]
    x <- x %>% mutate(normalized = x$n/y$n)
    
    plot_normalize <- if (nrow(x) > 1) {
      ggplot(x, aes(x=year, y=normalized, color = species)) + 
        geom_line() + 
        geom_point() + 
        scale_x_continuous() + 
        scale_y_continuous() + 
        labs(title = "Reported Species Occurrence Over Time",
             x = "Year",
             y = "Reported Sightings")
    }else {
      ggplot(x, aes(x=year, y=normalized, color = species)) + 
        geom_point() + 
        scale_x_continuous() +
        scale_y_continuous() + 
        labs(title = "Reported Species Occurence Over Time",
             x = "Year",
             y = "Reported Sightings"
        )
    }
    
    
    plot_raw <- if (nrow(x) > 1) {
      ggplot(x, aes(x=year, y=n, color = species)) + 
        geom_line() + 
        geom_point() + 
        scale_x_continuous() + 
        scale_y_continuous() + 
        labs(title = "Reported Species Occurrence Over Time",
             x = "Year",
             y = "Reported Sightings")
    }else {
      ggplot(x, aes(x=year, y=n, color = species)) + 
        geom_point() + 
        scale_x_continuous() +
        scale_y_continuous() + 
        labs(title = "Reported Species Occurence Over Time",
             x = "Year",
             y = "Reported Sightings"
        )
    }
    
    
    if (input$counts== "Total Species Observations per Year"){
      plot_normalize
      
      }else{
        plot_raw
      }
    })
}











# server <- function(input,output, session){
#   output$plot1 = renderPlot({
#     x = df2 %>%
#       filter(species == input$species) %>%
#       group_by(year) %>%
#       tally() %>%
#       drop_na()
#     
#     if (nrow(x) > 1) {
#       ggplot(x, aes(x=year, y=n)) + 
#         geom_line() + 
#         geom_point()
#     }
#     else {
#       ggplot(x, aes(x=year, y=n)) + 
#         geom_point()
#     }
#   })
# }

