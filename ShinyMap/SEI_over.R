library(rgeos)
library(sp)
library(rgdal)

wa.map <- readOGR("/Users/raghav/Desktop/DSSG19BioDiv/ShinyMap/MVSEI2014/", layer="MVSEI2014")
wa.map <- spTransform(wa.map, CRS("+proj=longlat +datum=WGS84"))

wa.map
#sodo <- wa.map[which(wa.map$Location == "Pacific Spirit"),]
sodo <- wa.map
dat = read.csv("gbif_map.csv", stringsAsFactors = F)

coordinates(dat) <- ~ decimalLongitude + decimalLatitude
proj4string(sodo) <- proj4string(dat)


#over(dat, sodo[3,], fn = NULL)

joint_poly = sodo[dat,]
joint_data = dat[sodo,]
x = NULL
for (poly in 1:length(joint_poly)) {
  x = rbind(x, as.data.frame(joint_data[joint_poly[poly,],], poly_index = joint_poly[poly,]$SEI_PolyNb))
}
x$X = NULL

write.csv(x, "gbif_map_poly.csv", row.names = FALSE)

outside_data = dat[which(!(row.names(dat) %in% row.names(joint_data@data))),]
outside_data$poly_index = NA
outside_data$X = NULL
export_data = rbind(x, outside_data)

write.csv(export_data, "gbid_map_polyfull.csv", row.names = F)

write.csv(wa.map@data, "sei_data.csv", row.names = FALSE)
