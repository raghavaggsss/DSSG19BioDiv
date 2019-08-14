# Load the necessary libraries
library(tidyverse)
library(SSDM)
library(dismo)
library(maptools)
library(rgdal)
library(raster)
library(sp)
library(tmap)
library(rgeos)
library(ggmap)
library(leaflet)
library(knitr)
library(leaflet.opacity)
library(rJava)

gbif_map <- read.csv("./gbif_map.csv", stringsAsFactors = F)



max_lat <- 49.75
min_lat <- 49
max_lon <- -122.
min_lon <- -123.75

geographic_extent <-  extent(x = c(min_lon, max_lon, min_lat, max_lat))