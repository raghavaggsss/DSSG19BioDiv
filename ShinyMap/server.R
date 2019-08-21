# Server Reactive Flow:
## The inputs are as follows: year (2-item integer vector), normalization (single character string) category (single character string), and member (vector of character strings ranging from 0 to infinity in length).
## The output is plot1, which renders a plot and reacts to df3 and input$normalization (to determine which column to plot on the y axis)
## df3 is a dataframe of summarized data for members of interest with attached normalizations. It reacts to df2 from which it gets the summarized data. It uses the original non-reactive dfsp but will not run in response to dfsp changing (which should never happen).
## df2 is a dataframe of summarized data for members of interest. It reacts to df1 (non-summarized data for members of interest), as well as input$category (it has an alternate branch if custom tags are selected), and in the custom tags branch it also reacts to input$member.
## df1 is a dataframe of non-summarized data for members of interest. It reacts to reac$df_time (fully filtered data) as well as input$member and input$category.
## reac is a container for reactive values.
## reac$df_time is fully filtered data. It reacts to df_space (data filtered only for space) and input$year.
## reac$df_space is partially filtered data. It reacts to URL queries (coordinates of polygons) and takes select information from the Heroku database 

server <- function(input, output, session){
  
  reac <- reactiveValues(df_space=data.frame(), df_time=data.frame(), dfsp=dfsp, tags=tag_list)
  
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
                        choices = reac$tags,
                        selected = reac$tags[1]
      )
    } 
    else {
      # If "species" is selected, load Anna's Hummingbird by default
      if (input$category == "species") {ch = "Calypte anna"} else {ch = head(sort(unique(reac$dfsp[,cat])), 1)}
      updateSelectInput(session, "member",
                        choices = sort(unique(reac$dfsp[,cat])),
                        selected = ch
      )
    }
  })
  
  # This is the observer that keeps track of the url so that it can derive coordinate information and filter the data to the selected region 
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query['coords'][[1]])) {
      # Create a wellknown-text (WKT) object from the user's selection
      coords = jsonlite::fromJSON(query['coords'][[1]])
      print(coords)
      poly = SpatialPolygons(list(Polygons(list(Polygon(coords)), ID = 1)))
      polyWKT = writeWKT(poly)
      # Use the WKT to select a subset of the data from Heroku (data cannot be loaded all at once)
      df_temp = as.data.frame(dbGetQuery(db, sprintf("SELECT * FROM biodivmap_gbifsummaryfull WHERE ST_Within(point, 'SRID=4326;%s')", polyWKT)))
      colnames(df_temp)[colnames(df_temp) %in% c("lon","lat")] = c("decimalLongitude","decimalLatitude")
      reac$df_space = df_temp
    }
  })
  
  # "react$df_time" is generated from "react$df_space" and the input years, meaning that it will recalculate as soon as either the year slider has changed or a different spatial selection is made. It feeds directly into the df1 function.
  observe({
    if (nrow(reac$df_space)==0) {return()}
    reac$df_time = reac$df_space[which((reac$df_space$year >= input$year[1]) & (reac$df_space$year <= input$year[2])),]
    reac$dfsp = dfsp[which(dfsp[,"species"] %in% reac$df_time[,"species"]),]
    # These lines of code remove custom tags that are left without tagged species after filtration
    removal_tags = c()
    for (i in 1:length(tag_list)) {
      if (!(T %in% unique(reac$dfsp[,tag_list[[i]]]))) {removal_tags = c(removal_tags,i)}
    }
    if (length(removal_tags) != 0) {reac$tags = tag_list[-removal_tags]} else {reac$tags = tag_list}
  })
  
  # df1 changes only with "category" and adds columns that gives member values for each category that could be chosen
  df1 = reactive({
    # Instead of throwing an error message, tells the user to select coordinates in the event that none are provided
    shiny::validate(
      need(nrow(reac$df_time)!=0,
           "Please select coordinates"))
    # This first line essentially prevents this from running until the "member" input has been registered, to prevent error messages resulting from putting the cart before the horse - the second line prevents a strange bug  where when "Custom Tags" is selected for category, member does not update from where it was previously right away, resulting in error messages
    if (is.null(input$member)) {return()}
    if (input$category=="Custom Tags" & !any(input$member %in% reac$tags)) {return()}
    df1 = reac$df_time
    # This if branch applies specifically for Custom Tags, since they work differently
    if (input$category == "Custom Tags") {
      sp_col = which(colnames(reac$dfsp)=="species")
      # Each member results in its own column filled with True and False values
      for (mem in input$member) {
        compat_sp = reac$dfsp[which(reac$dfsp[,mem] == 1),sp_col]
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
        compat_sp = reac$dfsp[which(reac$dfsp[,input$category] == mem),"species"]
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
      # "removal_names" is used to collect the names of the columns slated for removal later, after the proportions have been calculated and the totals are no longer needed.
      removal_names = "Total"
      if (!(input$category %in% c("Custom Tags", "kingdom"))) {
        king_name = unique(dfsp$kingdom[which(dfsp[,input$category]==mem)])
        norm_col = c(norm_col, which(colnames(y)==king_name))
        removal_names = c(removal_names, king_name)
        if (!(input$category %in% c("phylum", "class"))) {
          class_name = unique(dfsp$class[which(dfsp[,input$category]==mem)])
          norm_col = c(norm_col, which(colnames(y)==class_name))
          removal_names = c(removal_names, class_name)
        }
      }
      # Picks out the "total" columns corresponding to the kingdom/class our "member" belongs to (as well as the overall total column) before matching them all to the data.
      y = y[,norm_col]
      sdf = merge(sdf, y, by = "year", all.x = T)
      # Then uses that to find what percentage of the total observations belonged to the given member
      for (base in colnames(sdf)[4:ncol(sdf)]) {
        sdf = sdf %>% mutate(sdf$n/sdf[,base])
        if (base == "Total") {colnames(sdf)[ncol(sdf)] = "Total_norm"}
        else if (base %in% dfsp$kingdom) {colnames(sdf)[ncol(sdf)] = "Kingdom_norm"}
        else if (base %in% dfsp$class) {colnames(sdf)[ncol(sdf)] = "Class_norm"}
      }
      sdf[,removal_names] = NULL
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
