ubc = read.csv("GBIF_UBC.csv", stringsAsFactors = FALSE)

for(place in unique(data[,c('decimalLatitude','decimalLongitude')]))