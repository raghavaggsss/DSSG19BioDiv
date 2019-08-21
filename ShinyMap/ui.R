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
                  label = "Choose a Category", 
                  choices = c(tax_list, "Custom Tags"),
                  selected = "species",
                  multiple = F),
      
      selectInput(inputId = "member", 
                  label = "Select an Option to Plot", 
                  choices = sort(unique(dfsp$species)),
                  selected = "Calypte anna",
                  multiple = T),
      
      radioButtons(inputId = "normalization",
                   label = "Normalization Options",
                   choices = c("Raw Counts", "Proportion of Total Observations", "Proportion of Kingdom Observations", "Proportion of Class Observations")),
      
      bsTooltip(id = "year", 
                title = "Customize the plot by adjusting the year interval.",
                placement = "bottom",
                trigger = "hover"),
      
      bsTooltip(id = "category",
                title = "Select a taxon level (ie genus, species etc.) or choose a custom grouping.",
                placement = "bottom",
                trigger = "hover"),
      
      bsTooltip(id = "member",
                title = "Choose a sub-grouping of the main Category chosen above. IUCN, are species listed as globally endangered by the International Union for Conservation of Nature. SARA are endangered species protected through the federal Canadian Species At Risk Act. BC Red and BC Blue are provincially endangered species. BC Endemic are species that are found only in BC.",
                placement = "bottom",
                trigger = "hover"),
      
      bsTooltip(id = "normalization",
                title = "Note: Raw Counts is the number of species records for a given year. Normalizing by Total Observations in a given year corrects for the fact that GBIF occurrence records are increasing every year, regardless whether a given species is actually increasing in abundance. Normalizing by Kingdom or Class corrects for certain organisms being oversampled in certain years compared to others.",
                placement = "bottom",
                trigger = "hover")
    ),
    
    
    mainPanel(
      bsPopover(id = "plot1",
                title = "How to Interpret this Plot",
                content = "The time series graph above visualizes species occurrence records for the area selected on the map. The y-axis is measuring the abundance of species records and is assumed to only approximate actual species abundance. Normalization options in the panel to the left are provided to correct for biases when comparing records across years.",
                placement = "bottom",
                trigger = "hover"),
      plotOutput("plot1")
    )
  )
)
