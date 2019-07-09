library(shiny)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(gridExtra)
library(grid)
#library(plotrix) for 3D pie charts

# Load in the data 

# load complete GBIF data for MetroVan
gbif <- read.csv("./datasets/gbif_complete.csv", sep = "\t", stringsAsFactors = FALSE, na.strings = c("", " "))

# filter out the observations that have NA, unknown placement, viruses and archaea for kingdom
trimmed_gbif <- filter(gbif, 
                       kingdom!="incertae sedis" & kingdom!="NA" & kingdom!="Archaea" & kingdom!="Viruses")

# select the most relevant columns 
trimmed_gbif <- select(trimmed_gbif, c(datasetKey,kingdom, phylum, class, order,
                                       family,genus,species,taxonRank,locality, decimalLatitude,decimalLongitude,
                                       month, year, basisOfRecord))

# Add a column that strips the var. from any species name 
trimmed_gbif <- mutate(trimmed_gbif, simplified_names = gsub(pattern = " var.*", replacement = "", x = trimmed_gbif$species))

# Drop any NAs from the species and simplified names columns 
trimmed_gbif <- drop_na(trimmed_gbif, species, simplified_names, year)


ui <- fluidPage(
      selectInput(inputId = "kingdom",
                  label = "Select Kingdom",
                  choices = trimmed_gbif$kingdom),
      sliderInput(inputId = "year",
                  label = "Select Year",
                  min = min(trimmed_gbif$year),
                  max = max(trimmed_gbif$year),
                  value = 2019),
      plotOutput("bar_chart"))
      

server <- function(input, output){
      output$bar_chart <- renderPlot({
            x <- filter(trimmed_gbif, kingdom==input$kingdom & year==input$year)
            y <- as.data.frame(table(x$phylum))
            y <- y %>% mutate(name = fct_reorder(Var1, Freq))
            title <- "Distribution of the Phylum Level Observations in a Given Year"
            x_label <- "Phyla"
            y_label <- "Frequency"
            ggplot(y, aes(x = name, y = Freq, color = name))+
                  geom_col(show.legend = F)+
                  labs(x = x_label, 
                       y = y_label,
                       title = title,
                       subtitle = input$year)+
                  theme_dark()+
                  theme(plot.title = element_text(face = "bold"),
                        axis.title.x = element_text(face = "bold"),
                        axis.title.y = element_text(face = "bold"),
                        axis.text = element_text(face = "bold"))
      })
}

shinyApp(ui = ui, server = server)







# test sample 
ui <- fluidPage(
      sliderInput(inputId = "num",
                  label = "Choose a number",
                  value = 25, min = 1, max = 100),
      plotOutput("hist")
)

server <- function(input, output){
      output$hist <- renderPlot({
            title <- "100 random normal values"
            hist(rnorm(input$num), main = title)
      })
}


