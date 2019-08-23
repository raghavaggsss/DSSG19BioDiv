library(rgdal)
library(dplyr)
library(rgeos)
library(sp)

sei <- readOGR(dsn = "/Users/raghav/Desktop/DSSG19BioDiv/ShinyMap/MVSEI2014/",
               layer = "MVSEI2014")
sei <- spTransform(sei, CRS("+proj=longlat +datum=WGS84"))
obs_dat = read.csv("gbif_map.csv", stringsAsFactors = F)
obs_dat$ID = 1:nrow(obs_dat)
obs_df = obs_dat
coordinates(obs_dat) = c("decimalLongitude", "decimalLatitude")
proj4string(obs_dat) = CRS("+proj=longlat +datum=WGS84")
obs_dat = spTransform(obs_dat, proj4string(sei))


# Create a list that will hold 27 (spatial polygon) dataframes, each containing the gbif data for a given municipality/that occured within a polygon
sei@data = sei@data[order(sei@data$SEI_PolyNb),]
poly_list = list()

# Fills "poly_list" one item at a time, with each element being a data frame of the data that falls within a given polygon
for (ob in sei@data$SEI_PolyNb) {
  #poly_list[[ob]] = obs_dat[apply(gIntersects(obs_dat, sei[ob,], byid = TRUE), 2, any),]
  poly_list[[ob]] = obs_dat[apply(gIntersects(obs_dat, sei[which(sei@data$SEI_PolyNb==ob),], byid = TRUE), 2, any),]
}
# Delete municipalities containing no observations 
filled_poly  = poly_list[sapply(poly_list, function(x) nrow(x)>0)]

# Now that points and polygons have been matched, coordinates are converted back to the system used by the map viewer and the original GBif data
# Add a column to each of these new dataframes indicating which polygon it belongs to - then extract the data frame from the spatial polygon df object, and add all of them together into a regular output dataframe.
output = NULL
poly_list2 = list()
for (ob in 1:length(poly_list)) {
  if (nrow(poly_list[[ob]]@coords) > 0) {
    poly_list2[[ob]] = spTransform(poly_list[[ob]], CRS("+proj=longlat +datum=WGS84"))
    t = cbind(poly_list2[[ob]]@data, poly_list2[[ob]]@coords)
    output = rbind(output, cbind(t, rep(sei$SEI_PolyNb[ob], length=nrow(t))))
  }
}
colnames(output)[ncol(output)] = "poly_index"


collapse = output[,c(15,20)]
collapse = unique(collapse)


outside = obs_df[which(!(obs_df$ID %in% output$ID)),]
outside$poly_index = NA
output = rbind(output, outside)


# Write the files
write.csv(output, "gbif_sei_poly.csv", row.names = FALSE)
write.csv(collapse, "gbif_sei_poly_collapsed.csv", row.names = FALSE)
write.csv(sei@data, "sei_data.csv", row.names = FALSE)


raster::plot(sei)
points(poly_list2[[muns[count]]], col = "blue")

