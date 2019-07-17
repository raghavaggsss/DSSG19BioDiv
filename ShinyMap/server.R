server <- function(input,output, session){
  df_region <- reactiveValues(df=df_orig)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    print(query['municipality'][[1]])
    if (!is.null(query['municipality'][[1]])) {
      df_region$df = df_orig[which(df_orig$municipality == as.integer(query['municipality'][[1]])),]
    }
  })

  output$plot1 = renderPlot({
    raw = df_region$df %>%
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
             y = "Proportion of Total Species Observations")+
        theme(plot.title = element_text(face = "bold", size = 18),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text = element_text(face = "bold", size = 14),
              legend.title = element_text(face = "bold", size = 12),
              legend.text = element_text(size = 12))+
        expand_limits(y = 0)
      
      
    }else {
      year_seq = (x$year-1):(x$year+1)
      obsr_seq = 0:(2*x$normalized)
      ggplot(x, aes(x=year, y=normalized, color = species)) + 
        geom_point() + 
        scale_x_continuous(breaks = year_seq, limits = year_seq) +
        scale_y_continuous(breaks = obsr_seq, limits = obsr_seq) +
        coord_cartesian(xlim = year_seq, ylim = obsr_seq, expand = T) +
        labs(title = "Reported Species Occurence Over Time",
             x = "Year",
             y = "Proportion of Total Species Observations"
        )+
        theme(plot.title = element_text(face = "bold", size = 18),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text = element_text(face = "bold", size = 14),
              legend.title = element_text(face = "bold", size = 12),
              legend.text = element_text(size = 12))
    }
  
    
    plot_raw <- if (nrow(x) > 1) {
      ggplot(x, aes(x=year, y=n, color = species)) + 
        geom_line() + 
        geom_point() + 
        scale_x_continuous() + 
        scale_y_continuous() + 
        labs(title = "Reported Species Occurrence Over Time",
             x = "Year",
             y = "Number of Reported Observations")+
        theme(plot.title = element_text(face = "bold", size = 18),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text = element_text(face = "bold", size = 14),
              legend.title = element_text(face = "bold", size = 12),
              legend.text = element_text(size = 12))+
        expand_limits(y = 0)
      
    }else {
      year_seq = (x$year-1):(x$year+1)
      #obsr_seq = (x$n-1):(x$n+1)
      obsr_seq = 0:(x$n+1)
      ggplot(x, aes(x=year, y=n, color = species)) + 
        geom_point() + 
        scale_x_continuous(breaks = year_seq) +
        scale_y_continuous(breaks = obsr_seq) +
        coord_cartesian(xlim = year_seq, ylim = obsr_seq) +
        labs(title = "Reported Species Occurence Over Time",
             x = "Year",
             y = "Number of Reported Observations"
        )+
        theme(plot.title = element_text(face = "bold", size = 18),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text = element_text(face = "bold", size = 14),
              legend.title = element_text(face = "bold", size = 12),
              legend.text = element_text(size = 12))
    }
    
    
    if (input$counts== "Total Species Observations per Year"){
      plot_normalize
      
      }else{
        plot_raw
      }
    })
}

