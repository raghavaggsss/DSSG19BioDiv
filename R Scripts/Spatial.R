library(rgdal)
library(dplyr)
library(rgeos)
library(sp)
#library(proj4)
#library(maptools)
#library(gdat.table)

setwd("C:/Users/Gabe/Desktop/DSSG19BioDiv")
gdat = read.csv("GBif June27.csv")
gdatsp = gdat
bound = readOGR(dsn = "Admin Boundaries", layer = "AdminBoundary")

coordinates(gdatsp) = c("decimalLongitude", "decimalLatitude")
proj4string(gdatsp) = CRS("+proj=longlat +datum=WGS84")
gdatsp = spTransform(gdatsp, proj4string(bound))

# This code incorporates the UBC polygon
ubc.coord = matrix(c(-123.26494, 49.26512, -123.26308, 49.25894, -123.25409, 49.25118, -123.22587, 49.23889, -123.19758, 49.23532,-123.1979, 49.24501, -123.2054, 49.24898,-123.20218, 49.25358,-123.20382, 49.25832,-123.21564, 49.25873,-123.21564, 49.26374,-123.22793, 49.26326,-123.22896, 49.26656,-123.22264, 49.26889,-123.2269, 49.27322,-123.22662, 49.27947,-123.24743, 49.2805,-123.26267, 49.27047,-123.26494, 49.26512), ncol = 2, byrow = TRUE)




ubc.coord = matrix(c(-123.223952, 49.278195, -123.224796, 49.272730, -123.222808, 49.270882, -123.222808, 49.268641, -123.219675, 49.268759, -123.219495, 49.267265, -123.215277, 49.267382, -123.215398, 49.258417, -123.203288, 49.258140, -123.202864, 49.256686, -123.196597, 49.256646, -123.196718, 49.250197, -123.204491, 49.249214, -123.198949, 49.246303, -123.196780, 49.246224, -123.196898, 49.238515, -123.198525, 49.238475, -123.198586, 49.236626, -123.196899, 49.236587, -123.196899, 49.234935, -123.206900, 49.237532, -123.209431, 49.235565, -123.219976, 49.239577, -123.221662, 49.236902, -123.248681, 49.249010, -123.250528, 49.250441, -123.252837, 49.251495, -123.253761, 49.252851, -123.254570, 49.254809, -123.264734, 49.265203, -123.265081, 49.266710, -123.264273, 49.267162, -123.262659, 49.270779, -123.260463, 49.271831, -123.253076, 49.275524, -123.252037, 49.276804, -123.247420, 49.279517, -123.236335, 49.279442, -123.234257, 49.279518, -123.227567, 49.278763, -123.225500, 49.278093), ncol = 2, byrow = TRUE)


ubc.poly = Polygon(ubc.coord)
ubc.spoly = SpatialPolygons(list(Polygons(list(ubc.poly), ID = 99)), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
ubc.data = data.frame(OBJECTID=99, MunNum=28, FullName="University Endowment Lands", ShortName="UEL", GlobalID=NA, 
      created_us=NA, created_da=NA, last_edite=NA, last_edi_1=NA, SHAPE_STAr=NA, SHAPE_STLe=NA, row.names = 99)
ubc.spdf = SpatialPolygonsDataFrame(ubc.spoly,ubc.data)
ubc.spdf = spTransform(ubc.spdf, proj4string(bound))
bound.new = rbind(bound,ubc.spdf)
bound = bound.new

# Create a list that will hold 27 (spatial polygon) dataframes, each containing the gbif data for a given municipality/that occured within a polygon
bound@data = bound@data[order(bound@data$OBJECTID),]
mun_list = list()

# Creates a function to take the points that lie inside polygon "a" but not polygon "b" (polygon "b" is always UEL in our case but it could be different if need be)
A_not_B = function(a, b) {
  return(gIntersects(gdatsp, bound[a,], byid = TRUE) & !gIntersects(gdatsp, bound[b,], byid = TRUE))}
# Fills "mun_list" one item at a time, using our custom function for every non-UEL polygon and the 
for (ob in 1:nrow(bound@data)) {
  if (ob != nrow(bound@data)) {mun_list[[ob]] = gdatsp[apply(A_not_B(ob, nrow(bound@data)), 2, any),]}
  else {mun_list[[ob]] = gdatsp[apply(gIntersects(gdatsp, bound[ob,], byid = TRUE), 2, any),]}
}
# Delete municipalities containing no observations 
mun_list  = mun_list[sapply(mun_list, function(x) nrow(x)>0)]

# Now that points and polygons have been matched, coordinates are converted back to the system used by the map viewer and the original GBif data
mun_list2 = list()
for (ob in 1:length(mun_list)) {
  mun_list2[[ob]] = spTransform(mun_list[[ob]], CRS("+proj=longlat +datum=WGS84"))
}

# Add a column to each of these new dataframes indicating which municipality it belongs to - then extract the data frame from the spatial polygon df object, and add all of them together into a regular output dataframe.
output = NULL
for (ob in 1:length(mun_list)) {
  t = cbind(mun_list[[ob]]@data, mun_list[[ob]]@coords)
  output = rbind(output, cbind(t, rep(as.character(bound$FullName[ob]), length=nrow(t))))
}
colnames(output)[ncol(output)] = "municipality"


# This plots the points within all polygons (which should be pretty much all of them, only a few that GBif may have captured that are technically out of bounds)
plotrast = function(lis, muns = c()) {
  raster::plot(bound)
  count = 1
  while (length(muns) > (count - 1)) {
    points(lis[[muns[count]]], col = "blue")
    count = count + 1
  }
}


# Write the file
write.csv(output, "GBif Municipalities.csv", row.names = FALSE)












# Utility function for getting a vector of the number of rows of each element of a list of data frames
row_vec = function(lis) {
  y = c()
  for (m in 1:length(lis)) {y[m] = nrow(lis[[m]])}
  return(y)}
