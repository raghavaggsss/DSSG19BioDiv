server <- function(input, output, session){
  
  df_region <- reactiveValues(df=df_orig, dfsp=dfsp, tags=tag_list, use_tags=T)
  
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
                        choices = df_region$tags,
                        selected = df_region$tags[1]
                        #selected = head(colnames(df_region$dfsp)[tag_columns], 1)
      )
    } 
    else {
      # If "species" is selected, load Anna's Hummingbird by default
      if (input$category == "species") {ch = "Calypte anna"} else {ch = head(df_region$dfsp[,cat], 1)}
      updateSelectInput(session, "member",
                        choices = sort(unique(df_region$dfsp[,cat])),
                        selected = ch
                        )
    }
  })
  
  # This is the observer that keeps track of the url and grabs any municipality information being transmitted through it for use down the line - it then filters the data into 
  observe({
    query <- parseQueryString(session$clientData$url_search)
    #print(query['municipality'][[1]])
    #print(query['region'][[1]])
    if (!is.null(query['municipality'][[1]])) {
      df_region$df = df_orig[which(df_orig$municipality == as.integer(query['municipality'][[1]])),]
    }
    #if (!is.null(query['region'][[1]])) {
    #  coord = as.double(strsplit(query['region'][[1]], ",")[[1]])
    #  minx = min(coord[c(1,3)])
    #  maxx = max(coord[c(1,3)])
    #  miny = min(coord[c(2,4)])
    #  maxy = max(coord[c(2,4)])
    #  df_region$df = df_orig %>% filter(decimalLongitude > minx, decimalLongitude < maxx, decimalLatitude > miny, decimalLatitude < maxy)
    #}
    if (!is.null(query['coords'][[1]])) {
      # Create a SpatialPolygons object from the user's selection
      coords = jsonlite::fromJSON(query['coords'][[1]])
      sel = SpatialPolygons(list(Polygons(list(Polygon(coords)),1)))
      # Create a SpatialPoints object from df_orig
      spatialDF = SpatialPointsDataFrame(coords = df_region$df[,3:4], data = df_region$df[,-(3:4)])
      df_region$df = df_region$df[as.integer(row.names(spatialDF[sel,])),]
    }
    # These lines of code update the two variables that determine the choices users are given in the dropdown menus so if they narrow their selection, they aren't given options that correspond to empty data
    df_region$dfsp = dfsp[which(dfsp[,"species"] %in% df_region$df[,"species"]),]
    removal_tags = c()
    for (i in 1:length(tag_list)) {
      if (!(T %in% unique(df_region$dfsp[,tag_list[[i]]]))) {removal_tags = c(removal_tags,i)}
    }
    if (length(removal_tags) != 0) {df_region$tags = df_region$tags[-removal_tags]}
    if (length(df_region$tags) == 0) {df_region$use_tags = F} else {df_region$use_tags = T}
  })
  
  # df1 changes only with "category" and adds columns that gives member values for each category that could be chosen
  df1 = reactive({
    # This first line essentially prevents this from running until the "member" input has been registered, to prevent error messages resulting from putting the cart before the horse - the second line prevents a strange bug  where when "Custom Tags" is selected for category, member does not update from where it was previously right away, resulting in error messages
    if (is.null(input$member)) {return()}
    if (input$category=="Custom Tags" & !any(input$member %in% df_region$tags)) {return()}
    df1 = df_region$df
    # This if branch applies specifically for Custom Tags, since they work differently
    if (input$category == "Custom Tags") {
      sp_col = which(colnames(df_region$dfsp)=="species")
      # Each member results in its own column filled with True and False values
      for (mem in input$member) {
        compat_sp = df_region$dfsp[which(df_region$dfsp[,mem] == 1),sp_col]
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
        compat_sp = df_region$dfsp[which(df_region$dfsp[,input$category] == mem),"species"]
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
    # Validate sends a more helpful message and prevents Shiny from trying to create the graph if the user has selected incompatible category and normalization options
    shiny::validate(
      need(!((input$category %in% c("Custom Tags","kingdom") & input$normalization == "Proportion of Kingdom Observations") | (input$category %in% c("Custom Tags","kingdom", "phylum","class") & input$normalization == "Proportion of Class Observations")),
      "Please select a different normalization option for this category")
    )
    
    if (is.null(df2())) {return()}
    x = NULL
    for (mem in unique(df2()[,"member"])) {
      sdf = df2()[which(df2()[,"member"]==mem),]
      sdf = arrange(sdf, year)
      y = combined_norm
      # norm_col is a vector of integers representing the columns that correspond to the appropraite normalizations.
      # This mean at least "Total", and possible "kingdom" and "class" normalizations based on the category.
      norm_col = c(1,2)
      if (!(input$category %in% c("Custom Tags", "kingdom"))) {
        king_name = unique(dfsp$kingdom[which(dfsp[,input$category]==input$member)])
        norm_col = c(norm_col, which(colnames(y)==king_name))
        if (!(input$category %in% c("phylum", "class"))) {
          class_name = unique(dfsp$class[which(dfsp[,input$category]==input$member)])
          norm_col = c(norm_col, which(colnames(y)==class_name))
        }
      }
      y = y[,norm_col]
      # Adds a column with the number of observations per year for every year in the dataframe
      sdf = merge(sdf, y, by = "year", all.x = T)
      # Then uses that to find what percentage of the total observations belonged to the given member
      for (base in colnames(sdf)[4:ncol(sdf)]) {
        sdf = sdf %>% mutate(sdf$n/sdf[,base])
        if (base == "Total") {colnames(sdf)[ncol(sdf)] = "Total_norm"}
        else if (base %in% dfsp$kingdom) {colnames(sdf)[ncol(sdf)] = "Kingdom_norm"}
        else if (base %in% dfsp$class) {colnames(sdf)[ncol(sdf)] = "Class_norm"}
      }
      x = rbind(x, sdf)
    }
    # This line is debateable - in order both to ensure a continuous line plot and to prevent Shiny from throwing an error message, the normalized values of NA (years in which there were no observations at all and thus 0/0 = NA) are changed to values of 0
    #if (any(is.na(x[,4:ncol(x)]))) {x[which(is.na(x[,4:ncol(x)])),4:ncol(x)] = 0}
    return(x)
  })
  
  
  output$plot1 = renderPlot({
    
    if (is.null(df3())) {return()}

    # Based on the radio button clicked, uses different y aesthetics and labels
    if (input$normalization == "Raw Counts") {
      aesthetic = aes(x = year, y = n, color = member)
      yl = "Number of Reported Observations"
      obsr_seq = 0:(df3()$n[1]+1)
    }
    else if (input$normalization == "Proportion of Total Observations") {
      aesthetic = aes(x = year, y = Total_norm, color = member)
      yl = "Proportion of Total Observations"
      obsr_seq = 0:(df3()$Total_norm[1]*2)
    }
    else if (input$normalization == "Proportion of Kingdom Observations") {
      aesthetic = aes(x = year, y = Kingdom_norm, color = member)
      yl = "Proportion of Kingdom Observations"
      obsr_seq = 0:(df3()$Kingdom_norm[1]*2)
    }
    else if (input$normalization == "Proportion of Class Observations") {
      aesthetic = aes(x = year, y = Class_norm, color = member)
      yl = "Proportion of Class Observations"
      obsr_seq = 0:(df3()$Class_norm[1]*2)
    }
    
    if(nrow(df3()) > 1) {
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
              legend.text = element_text(size = 12))+
        expand_limits(y = 0)
      
    }else {
      year_seq = (df3()$year-1):(df3()$year+1)
      ggplot(df3(), aesthetic) + 
        geom_point() + 
        scale_x_continuous(breaks = year_seq) +
        scale_y_continuous(breaks = obsr_seq) +
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
  })
}

