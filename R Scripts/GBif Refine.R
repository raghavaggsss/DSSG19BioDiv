bysp = as.data.frame(table(data$scientificName))
bysp = bysp[order(bysp$Freq, decreasing = TRUE),]

# This loop runs over each unique species in the dataset
for (sp in unique(data$scientificName)) {
  # Each occurance of a given species is compared to each other occurance to see if they are close enough.
  df = data[which(data$scientificName==sp),]
  for (row in 1:nrow(df)) {
    for (comp in row:nrow(df)) {
      # This line finds the distance between the two points.
      if (distm(c(df$decimalLatitude[row], df$decimalLongitude[row]), 
                c(df$decimalLatitude[comp], df$decimalLongitude[comp]), fun = distHaversine()) <
          # This is the comparison value to which the distance between the two points will be set.
          # Presently I have it as the minimum of the two precisions.
          min(df$coordinatePrecision[row], df$coordinatePrecision[comp])
      ) {
        
      }
    }
  }
}