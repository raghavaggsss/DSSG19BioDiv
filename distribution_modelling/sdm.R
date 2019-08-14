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

# check out the vignette for SSDM package for more details about the workflow
browseVignettes("SSDM")


######### Section 1: Species Distribution Modelling ###### 


# Prepare the rufus species occurrence data 
# note: the gbif_map.csv can be found in google drive under map_jango data
gbif_map <- read.csv("./gbif_map.csv", stringsAsFactors = F)

# select only long/lat columns and change order of long/lat in dataframe
rufus <- filter(gbif_map, species=="Selasphorus rufus") %>% 
      dplyr::select(species, decimalLatitude, decimalLongitude) %>% 
      rename(latitude = decimalLatitude, longitude = decimalLongitude) 

rufus <- rufus[,c("species", "longitude", "latitude")]

obs <- rufus

# save the occurrence data
write.csv(x = obs, file = "./occurrence/rufus_occurrence.csv", row.names = FALSE)


# Prepare to crop the environment layers to min and max long/lat 

# set the geographic extent for Metro Vancouver 
# max_lat <- ceiling(max(obs$latitude))
# min_lat <- floor(min(obs$latitude))
# max_lon <- ceiling(max(obs$longitude))
# min_lon <- floor(min(obs$longitude))


max_lat <- 49.75
min_lat <- 49
max_lon <- -122.
min_lon <- -123.75

geographic_extent <-  extent(x = c(min_lon, max_lon, min_lat, max_lat))



# Prepare the Environment rasters

# dowload the bioclim data at the highest resolution available
worldclim <- getData(name = 'worldclim', path = "./environment/", var = 'bio', res = 0.5, lon = -123.11934, lat = 49.24966)
climlegend = c("Annual Mean Temperature","Mean Diurnal Range mean of monthly max temp sub min temp","Isothermality","Temperature Seasonality standard deviationx100","Max Temperature of Warmest Month","Min Temperature of Coldest Month","Temperature Annual Range BIO5 sub BIO6","Mean Temperature of Wettest Quarter","Mean Temperature of Driest Quarter","Mean Temperature of Warmest Quarter","Mean Temperature of Coldest Quarter","Annual Precipitation","Precipitation of Wettest Month","Precipitation of Driest Month","Precipitation Seasonality coefficient of variation","Precipitation of Wettest Quarter","Precipitation of Driest Quarter","Precipitation of Warmest Quarter","Precipitation of Coldest Quarter")

# crop the environment layers to the species' extent 
bioclim <- crop(x = worldclim, y = geographic_extent)

# save the cropped raster file 
writeRaster(x = bioclim, filename = c("./environment/bio1.bil",
                                      "./environment/bio2.bil",
                                      "./environment/bio3.bil",
                                      "./environment/bio4.bil",
                                      "./environment/bio5.bil",
                                      "./environment/bio6.bil",
                                      "./environment/bio7.bil",
                                      "./environment/bio8.bil",
                                      "./environment/bio9.bil",
                                      "./environment/bio10.bil",
                                      "./environment/bio11.bil",
                                      "./environment/bio12.bil",
                                      "./environment/bio13.bil",
                                      "./environment/bio14.bil",
                                      "./environment/bio15.bil",
                                      "./environment/bio16.bil",
                                      "./environment/bio17.bil",
                                      "./environment/bio18.bil",
                                      "./environment/bio19.bil"), 
            bylayer = TRUE, overwrite= TRUE)


# load in the altitude rasters
a1 = raster("raw altitude maps/092G02_cdsm_final_w.tif", RAT = T)
a2 = raster("raw altitude maps/092G03_cdsm_final_w.tif", RAT = T)
a3 = raster("raw altitude maps/092G06_cdsm_final_w.tif", RAT = T)
a4 = raster("raw altitude maps/092G07_cdsm_final_w.tif", RAT = T)
a5 = raster("raw altitude maps/092G02_cdsm_final_e.tif", RAT = T)
a6 = raster("raw altitude maps/092G03_cdsm_final_e.tif", RAT = T)
a7 = raster("raw altitude maps/092G06_cdsm_final_e.tif", RAT = T)
a8 = raster("raw altitude maps/092G07_cdsm_final_e.tif", RAT = T)
alt = merge(a1,a2,a3,a4,a5,a6,a7,a8)
alt = brick(alt)

# save the altitude rasterBrick
writeRaster(x = alt, filename = "./environment/cdsm_altitude.bil", format = "EHdr", bylayer = FALSE)


# plot the cropped bioclim data 
tmap_leaflet(qtm(shp = subset(bioclim, 1)))


# load in the cropped environment data
# note: function scales/normalizes the predictors by default
predictors <- load_var(path = "./environment/")
names(predictors) = c(climlegend[c(1,10:19,2:9)], "Altitude")

#Plot the environmental rasters to see what it looks like 
tmap_leaflet(qtm(shp = predictors))

# load in the species occurrence data, allowing spatial thinning
obs <- load_occ(path = "./occurrence/", 
                Env = predictors,
                file = "rufus_occurrence.csv",
                sep = ",",
                Xcol = "longitude",
                Ycol = "latitude",
                Spcol = "species",
                GeoRes = TRUE)


##### Model 1 #####
# build the model using GLM
SDM <- modelling(algorithm = 'GLM',
                 Occurrences = obs,
                 Env = predictors,
                 Xcol = "longitude",
                 Ycol = "latitude")

# check out the model evaluation metrics
knitr::kable(SDM@evaluation)

# evaluate the model in shiny
plot(SDM)

# plot the importance of environmental variables 
pred_importance <- gather(data = SDM@variable.importance, 
                          key = "predictor", 
                          value = "value")

ggplot(pred_importance, aes(x = reorder(predictor, -value), y = value))+
      geom_col()+
      labs(title = "Important Model Predictors",
           x = "Environmental Predictors",
           y = "Percentage Contribution to the Model")+
      coord_flip()


# plot habitat suitability (presence only), heatmap 
plot(SDM@projection)

tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
map <- tm_shape(SDM@projection)+
      tm_layout(title = "Predicted Distribution for Rufus Hummingbird with GLM & 63% Accuracy")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability")

tmap_leaflet(map)





##### Model 2 #####

##### Build the model again with different algorithms ####
# build the model using GAM
SDM_GAM <- modelling(algorithm = 'GAM',
                     Occurrences = obs,
                     Env = predictors,
                     Xcol = "longitude",
                     Ycol = "latitude")


# check out the model metrics
knitr::kable(SDM_GAM@evaluation)

# check shiny app
plot(SDM_GAM)

# plot the importance of environmental variables 
pred_importance_GAM <- gather(data = SDM_GAM@variable.importance, 
                          key = "predictor", 
                          value = "value")

ggplot(pred_importance_GAM, aes(x = reorder(predictor, -value), y = value))+
      geom_col()+
      labs(title = "Important Model Predictors",
           x = "Environmental Predictors",
           y = "Level of Importance")+
      coord_flip()

# plot the model 
tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
map_GAM <- tm_shape(SDM_GAM@projection)+
      tm_layout(title = "Predicted Distribution for Rufus Hummingbird with GAM & 71% Accuracy")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability")


tmap_leaflet(map_GAM)


#### Model 3 #######

# MARS algorithm 
SDM_MARS<- modelling(algorithm = 'MARS',
                     Occurrences = obs,
                     Env = predictors,
                     Xcol = "longitude",
                     Ycol = "latitude")

# look at evaluation metrics 
knitr::kable(SDM_MARS@evaluation)

# open the model metrics in shiny 
plot(SDM_MARS)


# plot the model 
tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
map_MARS <- tm_shape(SDM_MARS@projection)+
      tm_layout(title = "Predicted Distribution for Rufus Hummingbird with MARS and 72% Accuracy")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability")

tmap_leaflet(map_MARS)


##### Model 4 ######

# GBM algorithm 
# Note: This algorithm took a very long time to run on rufus hummingbird
SDM_GBM<- modelling(algorithm = 'GBM',
                     Occurrences = obs,
                     Env = predictors,
                     Xcol = "longitude",
                     Ycol = "latitude")

# look at evaluation metrics 
knitr::kable(SDM_GBM@evaluation)

# open the model metrics in shiny 
plot(SDM_GBM)

# plot the importance of environmental variables 
pred_importance_GBM<- gather(data = SDM_GBM@variable.importance, 
                               key = "predictor", 
                               value = "value")

ggplot(pred_importance_GBM, aes(x = reorder(predictor, -value), y = value))+
      geom_col()+
      labs(title = "Important Model Predictors",
           x = "Environmental Predictors",
           y = "Level of Importance")+
      coord_flip()


# plot the model 
tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
map_GBM <- tm_shape(SDM_MARS@projection)+
      tm_layout(title = "Predicted Distribution for Rufus Hummingbird with GBM and 67% Accuracy")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability")

tmap_leaflet(map_GBM)




###### Section 2: Perform Ensebmle modelling (ESDM) ####
ESDM <- ensemble_modelling(algorithms = c("MARS", "GAM"),
                           Occurrences = obs,
                           Env = predictors,
                           Xcol = "longitude",
                           Ycol = "latitude",
                           tmp = TRUE,
                           rep = 1)
# look at evaluation metrics 
knitr::kable(ESDM@evaluation)

# look at evaluaiton metrics in shiny 
plot(ESDM)


# plot the importance of environmental variables 
pred_importance_ESDM <- gather(data = ESDM@variable.importance, 
                              key = "predictor", 
                              value = "value")

ggplot(pred_importance_ESDM, aes(x = reorder(predictor, -value), y = value))+
      geom_col()+
      labs(title = "Important Model Predictors",
           x = "Environmental Predictors",
           y = "Level of Importance")+
      coord_flip()

# plot the model 
tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
map_ESDM <- tm_shape(ESDM@projection)+
      tm_layout(title = "Predicted Distribution for Rufus Hummingbird with GAM and MARS & ....% Accuracy")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability")

tmap_leaflet(map_ESDM)


tmap_leaflet(qtm(shp = ESDM@uncertainty, 
                 raster = "uncertainty.map",
                 title = "Habitat Suitability for Rufus Hummingbird using GAM and MARS"))








#####Section 3: Perform Stacked SDM (SSDM)######

##### Prepare the Occurrence data ######

# get the species that have over 40 observations, algorithms are better if a frequency threshold is set
taxon_freq = read.csv("Taxonomy_Freq.csv", stringsAsFactors = F)
over_40 <- filter(taxon_freq, freq>=40)

# filter data for 3 different species
obs2 <- filter(gbif_map, species=="Bombus impatiens" | species== "Sambucus racemosa"| species=="Selasphorus rufus") %>% 
      select(species, decimalLatitude, decimalLongitude) %>% 
      rename(latitude = decimalLatitude, longitude = decimalLongitude)

# switch order of columns
obs2 <- obs2[,c("species", "longitude", "latitude")] 

# save the stacked occurrence data 
write.csv(x =obs2,  file = "./occurrence/stacked_species.csv", row.names = FALSE)


# build the model
# load in the cropped environment data
predictors <- load_var(path = "./environment/")
names(predictors) = c(climlegend[c(1,10:19,2:9)], "Altitude")

# load in the stacked species occurrences
obs2 <- load_occ(path = "./occurrence/", 
                 Env = predictors,
                 file = "stacked_species.csv",
                 sep = ",",
                 Xcol = "longitude",
                 Ycol = "latitude",
                 Spcol = "species",
                 GeoRes = TRUE)

# build the model using GAM
SSDM_GAM<- stack_modelling(algorithms = "GAM",
                       Occurrences = obs2,
                       Env = predictors,
                       ensemble.thresh = 0,
                       Xcol = "longitude",
                       Ycol = "latitude",
                       Spcol = "species",
                       rep = 1,
                       method = "pSSDM",
                       tmp = TRUE,
                       cores = 1)

plot(SSDM_GAM)

# look at the evaluation stats
knitr::kable(SSDM_GAM@evaluation)

# set the options for mapping window
tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)

map_SSDM <- tm_shape(SSDM_GAM@diversity.map)+
      tm_layout(title = "Predicted Species Richness for Rufus Hummingbird, Bumblebee and Red Elderberry")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Diversity") 

tmap_leaflet(map_SSDM)




