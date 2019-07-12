server <- function(input,output, session){
  output$plot1 = renderPlot({
    raw = df2 %>%
      filter(species %in% input$species, year>=input$year[1], year<=input$year[2]) %>%
      group_by(species, year) %>%
      tally() %>%
      drop_na() %>%
      add_zeros()
    x = NULL
    for (sp in unique(raw$species)) {
      sdf = raw[which(raw$species==sp),]
      sdf = sdf[order(sdf$year),]
      y = yearly_obs[which(yearly_obs$year %in% sdf$year),]
      sdf <- sdf %>% mutate(normalized = sdf$n/y$n)
      x = rbind(x, sdf)
    }
    
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
      year_seq = (x$year-1):(x$year+1)
      obsr_seq = (x$n-1):(x$n+1)
      ggplot(x, aes(x=year, y=normalized, color = species)) + 
        geom_point() + 
        scale_x_continuous(breaks = year_seq, limits = year_seq) +
        scale_y_continuous(breaks = obsr_seq, limits = obsr_seq) +
        coord_cartesian(xlim = year_seq, ylim = obsr_seq) +
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
      year_seq = (x$year-1):(x$year+1)
      obsr_seq = (x$n-1):(x$n+1)
      ggplot(x, aes(x=year, y=n, color = species)) + 
        geom_point() + 
        scale_x_continuous(breaks = year_seq, limits = year_seq) +
        scale_y_continuous(breaks = obsr_seq, limits = obsr_seq) +
        coord_cartesian(xlim = year_seq, ylim = obsr_seq) +
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

