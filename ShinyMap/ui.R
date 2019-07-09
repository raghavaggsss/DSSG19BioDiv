ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Year Range",
                  min = 1800,
                  max = 2019,
                  step = 1,
                  value = c(1800,2019)),
      selectInput("species", "Species", choices = df$species)
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)
