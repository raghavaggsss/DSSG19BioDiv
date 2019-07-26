ui <- fluidPage(
  titlePanel(title = "Species Occurrence Trends"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Year Range",
                  min = 1800,
                  max = 2019,
                  step = 1,
                  value = c(1980,2018),
                  sep = ""),
      selectInput(inputId = "category", 
                  label = "Choose a Category:", 
                  choices = c(tax_list, "Custom Tags"),
                  selected = "species",
                  multiple = F),
      selectInput(inputId = "member", 
                  label = "Select an Option to Plot", 
                  choices = sort(unique(df_orig$species[which((df_orig$year >= 1980) & (df_orig$year <= 2019))])),
                  selected = "Calypte anna",
                  multiple = T),
      radioButtons(inputId = "normalization",
                   label = "Normalization Options",
                   choices = c("Raw Counts", "Proportion of Total Observations", "Proportion of Kingdom Observations", "Proportion of Class Observations"))),
    

    mainPanel(
      plotOutput("plot1")
    )
  )
)
