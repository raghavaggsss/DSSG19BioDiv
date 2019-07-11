ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Year Range",
                  min = 1800,
                  max = 2019,
                  step = 1,
                  value = c(1800,2019),
                  sep = ""),
      selectInput(inputId = "species", 
                  label = "Species", 
                  choices = sort(unique(df2$species)),
                  selected = "Abagrotis baueri",
                  multiple = T),
      radioButtons(inputId = "counts",
                   label = "Normalization Options",
                   choices = c("Raw Counts", "Total Species Observations per Year"))),
    
    mainPanel(
      plotOutput("plot1")
    )
  )
)


