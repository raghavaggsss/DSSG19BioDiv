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
                   choices = c("Raw Counts", "Proportion of Total Observations", "Proportion of Kingdom Observations", "Proportion of Class Observations")),
      wellPanel(
        tags$small(paste0(
          "Note: Raw Counts is the number of species records for a given year. Normalizing by Total Observations in a given year corrects for the fact that GBIF occurrence records are increasing every year, regardless whether a given species is actually increasing in abundance. Normalizing by Kingdom or Class corrects for certain organisms being oversampled in certain years compared to others."))
      )),
    

    mainPanel(
      h5("The time series graph below visualizes species occurrence records from the GBIF database for the area selected on the map. You may customize the plot by adjusting the year interval, taxon level (ie genus, species etc.) or choose a custom grouping like pollinators or endangered species. Normalization options are provided to correct for biases when comparing records across years."),
      headerPanel(""),
      plotOutput("plot1")
    )
  )
)
