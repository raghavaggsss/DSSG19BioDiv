server <- function(input,output, session){
  
  #data <- reactive({
  #  switch(df_yearagg,
  #         "year" = year,
  #         "freq" = freq)
  #})
  
  
  
  output$plot1 = renderPlot({
    #hist(df_yearagg$year[which(df_yearagg$year>input$year)], breaks = seq(1800, 2020, length.out = 100))
    df %>%
      filter(species == input$species) %>%
      group_by(year) %>%
      tally() %>%
      ggplot(aes(x=year, y=n)) + geom_line() + scale_x_discrete(limits=seq(1800,2020,5))
  })
}


#,
#year >= input$year[1],
#year <= input$year[2]