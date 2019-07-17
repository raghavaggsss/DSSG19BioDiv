server <- function(input,output, session){
  # This observe statement changes the options provided to the user in the "Members" dropdown menu based on what they have selected from the "Category" dropdown menu
  observe({
    # Retrieve the selected category
    cat = input$category
    # Set "cat" to blank if nothing selected
    if (is.null(cat)) {
      cat = character(0)}
    # Change the member choices based on the category selection
    if (cat == "Custom Tags") {
      updateSelectInput(session, "member",
                        choices = sort(colnames(dfsp)[19:ncol(dfsp)]),
                        selected = head(colnames(dfsp)[19:ncol(dfsp)], 1)
      )
    } else {
      updateSelectInput(session, "member",
                        choices = sort(unique(dfsp[,cat])),
                        selected = head(dfsp[,cat], 1)
                        )
    }
  })
  
  df_region <- reactiveValues(df=df_orig)
  
  # This is the observer that keeps track of the url and grabs any municipality information being transmitted through it for use down the line - it then filters the data into 
  observe({
    query <- parseQueryString(session$clientData$url_search)
    print(query['municipality'][[1]])
    if (!is.null(query['municipality'][[1]])) {
      df_region$df = df_orig[which(df_orig$municipality == as.integer(query['municipality'][[1]])),]
    }
  })
  
  # df1 changes only with "category" and adds columns that gives member values for each category that could be chosen
  df1 = reactive({
    df1 = df_region$df
    # This if branch applies specifically for Custom Tags, since they work differently
    if (input$category == "Custom Tags") {
      sp_col = which(colnames(dfsp)=="species")
      # Each member results in its own column filled with True and False values
      for (mem in input$member) {
        compat_sp = dfsp[which(dfsp[,mem] == 1),"species"]
        df1[which(df1$species %in% compat_sp),ncol(df1)+1] = TRUE
        df1[which(is.na(df1[,ncol(df1)])),ncol(df1)] = FALSE
        colnames(df1)[ncol(df1)] = mem
      }
      # This line removes rows that are False for all custom tags (works differently if you have 1 vs. +1 tags selected)
      if (length(input$member)==1) {df1 = df1[which(df1[,ncol(df1)]==T),]}
      else {df1 = df1[which(apply(df1[,(ncol(df1)+1-(length(input$member))):ncol(df1)], 1, any)),]}
      return(df1)
    } 
    # The code is for taxonomy levels
    else {
      df1$member = NA
      # Unlike above, only one column is produced - the number of members determines the number of unique possible string values in rows can possess within this column
      for (mem in input$member) {
        compat_sp = dfsp[which(dfsp[,input$category] == mem),"species"]
        df1$member[which(df1$species %in% compat_sp)] = mem
      }
      return(df1[which(!is.na(df1$member)),])        
    }
  }
  )
  
  # df2 is the aggregated data file, with one row per yearXmember
  df2 = reactive({
    if (input$category == "Custom Tags") {
      df2 = NULL
      for (mem in input$member) {
        memdf = df1()[which(df1()[,mem] == T),] %>% 
          filter(year>=input$year[1], year<=input$year[2]) %>%
          group_by(year) %>%
          tally() %>%
          add_zeros2(max(min(df1()[,"year"]), input$year[1]), min(max(df1()[,"year"]), input$year[2]))
        memdf$member = mem
        df2 = rbind(df2, memdf)
      }
      return(df2)
    }
    else {
      df1() %>%
        filter(member %in% input$member, year>=input$year[1], year<=input$year[2]) %>%
        group_by(member, year) %>%
        tally() %>%
        drop_na() %>%
        add_zeros1()
    }
    })

  # d3 is the normalized data
  df3 = reactive({
    x = NULL
    for (mem in unique(df2()[,"member"])) {
      sdf = df2()[which(df2()[,"member"]==mem),]
      sdf = arrange(sdf, year)
      y = yearly_obs
      colnames(y)[2] = "all_obs"
      sdf = merge(sdf, y, by = "year", all.x = T)
      sdf = sdf %>% mutate(normalized = sdf$n/sdf$all_obs)
      x = rbind(x, sdf)
    }
    return(x)
  })
  
  
  output$plot1 = renderPlot({
    
    plot_normalize = if(nrow(df3()) > 1) {
      if (nrow(df3()) > 1) {
        ggplot(df3(), aes(x=year, y=normalized, color = member)) + 
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
                legend.text = element_text(size = 12))
      } else {
        year_seq = (df3()$year-1):(df3()$year+1)
        obsr_seq = (df3()$n-1):(df3()$n+1)
        ggplot(df3(), aes(x=year, y=normalized, color = member)) + 
          geom_point() + 
          scale_x_continuous(breaks = year_seq, limits = year_seq) +
          scale_y_continuous(breaks = obsr_seq, limits = obsr_seq) +
          coord_cartesian(xlim = year_seq, ylim = obsr_seq) +
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
    } 
    
    plot_raw = if(nrow(df3()) > 1) {
        ggplot(df3(), aes(x=year, y=n, color = member)) + 
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
                legend.text = element_text(size = 12))
      } else {
        year_seq = (df3()$year-1):(df3()$year+1)
        obsr_seq = (df3()$n-1):(df3()$n+1)
        ggplot(df3(), aes(x=year, y=n, color = member)) + 
          geom_point() + 
          scale_x_continuous(breaks = year_seq, limits = year_seq) +
          scale_y_continuous(breaks = obsr_seq, limits = obsr_seq) +
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
    } else {
      plot_raw
    }
  })
}

