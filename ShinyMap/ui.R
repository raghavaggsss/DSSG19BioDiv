ui <- fluidPage(
  titlePanel(title = "Species Occurrence Trends"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Year Range",
                  min = 1800,
                  max = 2019,
                  step = 1,
                  value = c(1800,2019),
                  sep = ""),
      selectInput(inputId = "category", 
                  label = "Choose a Category:", 
                  choices = c(colnames(dfsp[,tax_columns]), "Custom Tags"),
                  selected = "species",
                  multiple = F),
      selectInput(inputId = "member", 
                  label = "Select an Option to Plot", 
                  choices = sort(unique(df_orig$species)),
                  selected = "Calypte anna",
                  multiple = T),
      radioButtons(inputId = "counts",
                   label = "Normalization Options",
                   choices = c("Raw Counts", "Total Species Observations per Year"))),
    

    mainPanel(
      plotOutput("plot1")
    )
  )
)
