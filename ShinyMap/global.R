library(shiny)
library(leaflet)
library(tidyverse)
library(dplyr)
library(tidyverse)

# Read in the data 
df2 <- readRDS( "/Users/lesley/Desktop/datasets/gbif_complete.rds")

# create a dataframe containing total num of observations for each year 
yearly_obs <- group_by(df2, year) %>% tally() %>% drop_na()


# Function for adding 0-value rows to aggregate tally dataframes to fill out the years between the first and last years
# Note: "data" must have exactly columns "species", "year", and "n"
add_zeros = function(data) {
  if (class(data)[1] != "data.frame") {data = as.data.frame(data)}
  if (nrow(data) > 1) {
    for (sp in unique(data$species)) {
      min = min(data$year[which(data$species == sp)], na.rm = T)
      max = max(data$year[which(data$species == sp)], na.rm = T)
      for (yea in (min+1):(max-1)) {
        if (!(yea %in% data$year[which(data$species == sp)])) {
          data = rbind(data, data.frame(species = sp, year = as.integer(yea), n = as.integer(0), stringsAsFactors = F))
        }
      }
    }
  }
  return(data)
}

