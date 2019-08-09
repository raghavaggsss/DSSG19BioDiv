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
max_lat <- 49.5
min_lat <- 49
max_lon <- -122.5
min_lon <- -123.5

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
            bylayer = TRUE)


# download the altitude data, place it in same directory as the bioclim data
alt <- getData(name = 'alt', path = "./environment/wc0.5/", country = 'CAN', mask = TRUE)

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

## crop the altitude data
#alt <- crop(x = alt, y = geographic_extent)

# save the cropped altitude data 
writeRaster(x = alt, filename = "./cropped_env/van_alt.bil")

#Plot the cropped altitude raster to see what it looks like 
tmap_leaflet(qtm(shp = alt))

# plot the cropped bioclim data 
tmap_leaflet(qtm(shp = bioclim))

# load in the cropped environment data
# note: function scales/normalizes the predictors by default
predictors <- load_var(path = "./cropped_env/")
names(predictors) = c(climlegend[c(1,10:19,2:9)], "Altitude")

# load in the species occurrence data, allowing spatial thinning
obs <- load_occ(path = "./occurrence/", 
                Env = predictors,
                file = "rufus_occurrence.csv",
                sep = ",",
                Xcol = "longitude",
                Ycol = "latitude",
                Spcol = "species",
                GeoRes = TRUE)

# build the model using GLM
SDM <- modelling(algorithm = 'GLM',
                 Occurrences = obs,
                 Env = predictors,
                 Xcol = "longitude",
                 Ycol = "latitude")

knitr::kable(SDM@evaluation)

# evaluate the model in shiny
plot(SDM)

# plot the importance of environmental variables 
pred_importance <- gather(data = SDM@variable.importance, 
                          key = "predictor", 
                          value = "value")

ggplot(pred_importance, aes(x = reorder(predictor, -value), y = value))+
      geom_col()+
      labs(title = "Importance to the Model of the Predictors",
           x = "Environmental Predictors",
           y = "Level of Importance")+
      coord_flip()


# plot habitat suitability (presence only), heatmap 
plot(SDM@projection)

tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
map <- tm_shape(SDM@projection)+
      tm_layout(title = "Predicted Distribution for Rufus Hummingbird\nwith 90% Accuracy")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability")

tmap_leaflet(map)









##### Build the model again with different algorithms ####
# build the model using GAM
SDM_GAM <- modelling(algorithm = 'GAM',
                     Occurrences = obs,
                     Env = predictors,
                     Xcol = "longitude",
                     Ycol = "latitude")

plot(SDM_GAM)

knitr::kable(SDM_GAM@evaluation)

tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
map_GAM <- tm_shape(SDM_GAM@projection)+
      tm_layout(title = "Predicted Distribution for Rufus Hummingbird\nwith 90% Accuracy")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability")


tmap_leaflet(map_GAM)


# MARS algorithm 
SDM_MARS<- modelling(algorithm = 'MARS',
                     Occurrences = obs,
                     Env = predictors,
                     Xcol = "longitude",
                     Ycol = "latitude")

# open the model metrics in shiny 
plot(SDM_MARS)

# look at evaluation metrics 
knitr::kable(SDM_MARS@evaluation)

# plot the model 
tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
map_MARS <- tm_shape(SDM_MARS@projection)+
      tm_layout(title = "Predicted Distribution for Rufus Hummingbird\nwith MARS")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability")

tmap_leaflet(map_MARS)




###### Section 2: Perform Ensebmle modelling (ESDM) ####
ESDM <- ensemble_modelling(algorithms = c("GLM", "MARS", "GAM"),
                           Occurrences = obs,
                           Env = predictors,
                           Xcol = "longitude",
                           Ycol = "latitude",
                           tmp = TRUE,
                           rep = 1)

# look at evaluaiton metrics in shiny 
plot(ESDM)

# look at evaluation metrics 
knitr::kable(ESDM@evaluation)

# plot the model 
tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
map_ESDM <- tm_shape(ESDM@projection)+
      tm_layout(title = "Habitat Suitability for Rufus Hummingbird using GLM, GAM and MARS")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability")

tmap_leaflet(map_ESDM)


tmap_leaflet(qtm(shp = ESDM@uncertainty, 
                 raster = "uncertainty.map",
                 title = "Habitat Suitability for Rufus Hummingbird using GLM, GAM and MARS"))








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
predictors <- load_var(path = "./cropped_env/")

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
SSDM<- stack_modelling(algorithms = "GAM",
                       Occurrences = obs2,
                       Env = predictors,
                       Xcol = "longitude",
                       Ycol = "latitude",
                       Spcol = "species",
                       rep = 1,
                       tmp = TRUE,
                       cores = 2)

plot(SSDM)

# look at the evaluation stats
knitr::kable(SSDM@evaluation)

# set the options for mapping window
tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)

map_SSDM <- tm_shape(SSDM@projection)+
      tm_layout(title = "Probability Distribution for Rufus Hummingbird\nBumblebee and Red Elderberry")+
      tm_raster(alpha = 0.6, saturation = 1, title = "Probability") 

tmap_leaflet(map_SSDM)




