server <- function(input, output, session){
  # This observe statement changes the options provided to the user in the "Members" dropdown menu based on what they have selected from the "Category" dropdown menu
  observe(priority = 1, {
    # Retrieve the selected category
    cat = input$category
    # Set "cat" to blank if nothing selected
    if (is.null(cat)) {
      cat = character(0)}
    # Change the member choices based on the category selection
    if (cat == "Custom Tags") {
      updateSelectInput(session, "member",
                        choices = tag_list,
                        selected = tag_list[[1]]
                        #selected = head(colnames(dfsp)[tag_columns], 1)
      )
    } 
    else {
      # If "species" is selected, load Anna's Hummingbird by default
      if (input$category == "species") {ch = "Calypte anna"} else {ch = head(dfsp[,cat], 1)}
      updateSelectInput(session, "member",
                        choices = sort(unique(dfsp[,cat])),
                        selected = ch
                        )
    }
  })
  
  # This is the observer that keeps track of the url and grabs any municipality information being transmitted through it for use down the line - it then filters the data into 
  df_region <- reactiveValues(df=df_orig)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    print(query['municipality'][[1]])
    print(query['region'][[1]])
    if (!is.null(query['municipality'][[1]])) {
      df_region$df = df_orig[which(df_orig$municipality == as.integer(query['municipality'][[1]])),]
    }
    if (!is.null(query['region'][[1]])) {
      coord = as.double(strsplit(query['region'][[1]], ",")[[1]])
      minx = min(coord[c(1,3)])
      maxx = max(coord[c(1,3)])
      miny = min(coord[c(2,4)])
      maxy = max(coord[c(2,4)])
      df_region$df = df_orig %>% filter(decimalLongitude > minx, decimalLongitude < maxx, decimalLatitude > miny, decimalLatitude < maxy)
    }
  })
  
  # df1 changes only with "category" and adds columns that gives member values for each category that could be chosen
  df1 = reactive({
    # This first line essentially prevents this from running until the "member" input has been registered, to prevent error messages resulting from putting the cart before the horse - the second line prevents a strange bug  where when "Custom Tags" is selected for category, member does not update from where it was previously right away, resulting in error messages
    if (is.null(input$member)) {return()}
    if (input$category=="Custom Tags" & !any(input$member %in% tag_list)) {return()}
    df1 = df_region$df
    # This if branch applies specifically for Custom Tags, since they work differently
    if (input$category == "Custom Tags") {
      sp_col = which(colnames(dfsp)=="species")
      # Each member results in its own column filled with True and False values
      for (mem in input$member) {
        compat_sp = dfsp[which(dfsp[,mem] == 1),sp_col]
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
  
  # df2 is the aggregated data frame, with one row per yearXmember
  df2 = reactive({
    if (is.null(df1())) {return()}
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
    if (is.null(df2())) {return()}
    x = NULL
    for (mem in unique(df2()[,"member"])) {
      sdf = df2()[which(df2()[,"member"]==mem),]
      sdf = arrange(sdf, year)
      y = yearly_obs
      colnames(y)[2] = "all_obs"
      # Adds a column with the number of observations per year for every year in the dataframe
      sdf = merge(sdf, y, by = "year", all.x = T)
      # Then uses that to find what percentage of the total observations belonged to the given member
      sdf = sdf %>% mutate(normalized = sdf$n/sdf$all_obs)
      x = rbind(x, sdf)
    }
    # This line is debateable - in order both to ensure a continuous line plot and to prevent Shiny from throwing an error message, the normalized values of NA (years in which there were no observations at all and thus 0/0 = NA) are changed to values of 0
    #if (any(is.na(x$normalized))) {x[which(is.na(x$normalized)),"normalized"] = 0}
    return(x)
  })
  
  
  output$plot1 = renderPlot({
    
    #if (is.null(df3())) {return()}
    
    # Based on the radio button clicked, uses different y aesthetics and labels
    if (input$counts == "Total Species Observations per Year") {
      aesthetic = aes(x = year, y = normalized, color = member)
      yl = "Proportion of Total Species Observations"
    }
    else {
      aesthetic = aes(x = year, y = n, color = member)
      yl = "Number of Reported Observations"
    }
    
    if(nrow(df3()) > 1) {
      if (nrow(df3()) > 1) {
        ggplot(df3(), aesthetic) + 
          geom_line() + 
          geom_point() + 
          scale_x_continuous() + 
          scale_y_continuous() + 
          labs(title = "Reported Species Occurrence Over Time",
               x = "Year",
               y = yl)+
          theme(plot.title = element_text(face = "bold", size = 18),
                axis.title.x = element_text(face = "bold", size = 14),
                axis.title.y = element_text(face = "bold", size = 14),
                axis.text = element_text(face = "bold", size = 14),
                legend.title = element_text(face = "bold", size = 12),
                legend.text = element_text(size = 12))
      } else {
        year_seq = (df3()$year-1):(df3()$year+1)
        obsr_seq = 0:(df3()$n+1)
        ggplot(df3(), aesthetic) + 
          geom_point() + 
          scale_x_continuous(breaks = year_seq, limits = year_seq) +
          scale_y_continuous(breaks = obsr_seq, limits = obsr_seq) +
          coord_cartesian(xlim = year_seq, ylim = obsr_seq) +
          labs(title = "Reported Species Occurence Over Time",
               x = "Year",
               y = yl)+
          theme(plot.title = element_text(face = "bold", size = 18),
                axis.title.x = element_text(face = "bold", size = 14),
                axis.title.y = element_text(face = "bold", size = 14),
                axis.text = element_text(face = "bold", size = 14),
                legend.title = element_text(face = "bold", size = 12),
                legend.text = element_text(size = 12))
      }
    }
  })
}

