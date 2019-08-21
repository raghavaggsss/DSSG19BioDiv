# Load the necessary libraries
library(shiny)
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
library(shinyBS)
library(tidyverse)
library(SSDM)
#library(rJava)
#library(RPostgreSQL)
library(rpostgis)

# db = dbConnect(
#       Postgres(),
#       user = 'gabe',
#       password = 'p04281088ef1e53efce910757a295fe9dc9c5b04374b8ba5a695b9dab4ae87044',
#       dbname = 'dbsftg98g2ls2b',
#       host = 'ec2-3-225-228-195.compute-1.amazonaws.com',
#       port = 5432,
#       sslmode = 'require'
# )
# 
# df_orig = dbGetQuery(db, "SELECT * FROM biodivmap_gbifsummary")
gbif_map <- read.csv("./gbif_map.csv", stringsAsFactors = F)

df_orig = gbif_map

taxon_freq <- read.csv("~/Desktop/DSSG19BioDiv/ShinyMap/Taxonomy_Freq.csv")

over_threshold <- filter(taxon_freq, freq>=40)

# filter the gbif data to species that have above threshold of observations *************why are there names on taxon_freq that are not in gbif_map?
df_orig <- df_orig[which(df_orig$species%in%over_threshold$simplified_names),]


# set the Metro Vancouver lat/lon limits
max_lat <- 49.75
min_lat <- 49
max_lon <- -122.
min_lon <- -123.75

# create the geographic extent object for Metro Vancouver
geographic_extent <-  extent(x = c(min_lon, max_lon, min_lat, max_lat))

# create a legend for environment variables
climlegend = c("Annual Mean Temperature","Mean Diurnal Range mean of monthly max temp sub min temp","Isothermality","Temperature Seasonality standard deviationx100","Max Temperature of Warmest Month","Min Temperature of Coldest Month","Temperature Annual Range BIO5 sub BIO6","Mean Temperature of Wettest Quarter","Mean Temperature of Driest Quarter","Mean Temperature of Warmest Quarter","Mean Temperature of Coldest Quarter","Annual Precipitation","Precipitation of Wettest Month","Precipitation of Driest Month","Precipitation Seasonality coefficient of variation","Precipitation of Wettest Quarter","Precipitation of Driest Quarter","Precipitation of Warmest Quarter","Precipitation of Coldest Quarter")

# load in the environment data 
# note: function scales/normalizes the predictors by default
# ##### Need to provide a path to the SQL database to load in the separate alt and climate Rasterstacks
predictors <- load_var(path = "~/Desktop/DSSG19BioDiv/distribution_modelling/environment/")
names(predictors) = c(climlegend[c(1,10:19,2:9)], "Altitude")






