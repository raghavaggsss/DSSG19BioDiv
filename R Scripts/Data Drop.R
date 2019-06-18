library(tidyverse)

## TESTING ##

ubc = read.csv("GBIF_UBC.csv", sep = "\t", stringsAsFactors = FALSE)
df = as.data.frame(table(ubc$decimalLatitude, ubc$decimalLongitude))
df = df[df$Freq!= 0,]
df = df[order(df$Freq, decreasing = TRUE),]

# The UBC dataset has 81,794 observations. Of those, there are 1,110 unique latitudes and 1,120 unique longitudes, making for 3,722,940 unique possible combinations of observed longitudes and latitudes. Of these possibilities, it appears as though only 1231 are filled with observations. Which is troublesome because it means there are an average of 66 observations per point.

explore_repeats = function(data, loc_combs, other_vars = c()) {
  unq = 0
  for (loc in 1:nrow(loc_combs)) {
    subs = data[which(data$decimalLatitude==loc_combs[loc,1] & data$decimalLongitude==loc_combs[loc,2]),]
    if (length(other_vars) > 0) {
      for (entry1 in unique(subs %>% pull(other_vars[1]))) {
        subs1 = subs[which(subs %>% select(one_of(other_vars[1]))==entry1),]
        if (length(other_vars) > 1) {
          for (entry2 in unique(subs1 %>% pull(other_vars[2]))) {
            subs2 = subs1[which(subs1 %>% select(one_of(other_vars[2]))==entry2),]
            if (length(other_vars) > 2) {
              for (entry3 in unique(subs2 %>% pull(other_vars[3]))) {
                unq = unq + 1
              }
            }
            else {unq = unq + 1}
          }
        }
        else {unq = unq +1}
      }
    }
    else {unq = unq + 1}
  }
  return(unq)
}

#small = ubc[1:500,]
#small_coor = as.data.frame(table(small$decimalLatitude, small$decimalLongitude))
#small_coor = small_coor[small_coor$Freq!= 0,]
#small_coor = small_coor[order(small_coor$Freq, decreasing = TRUE),]

labels = c("Remove species-person-time duplicates", "Remove species-person duplicates", "Remove species-time duplicates", "Remove person-time duplicates", "Remove species duplicates")
a = explore_repeats(ubc, df, c("species", "recordedBy", "year"))
b = explore_repeats(ubc, df, c("species", "recordedBy"))
c = explore_repeats(ubc, df, c("species", "year"))
d = explore_repeats(ubc, df, c("recordedBy", "year"))
e = explore_repeats(ubc, df, c("species"))
ret_ob = round(c(a,b,c,d,e)/nrow(ubc)*100, 1)
ret_ob = 100 - ret_ob
display_table = cbind(labels,ret_ob)
colnames(display_table) = c("Types of Duplicates Removed", "Percentage of Data Considered Duplicates")
print(display_table)

x = explore_repeats(ubc, df, c("species","year","month"))
# It looks like 45.6% of the data remains if you remove duplicates, considering duplicates to be multiple observations of the same species made in the same location within the same month (of the same year), irrespective of who made the observation.


drop_repeats = function(data) {
  duplicates = NULL
  for (obs in 1:(nrow(data))-1) {
    matches = data[(1+obs):nrow(data),]
    matches = matches[which(matches$year == data$year[obs]),]
    matches = matches[which(matches$month == data$month[obs]),]
    matches = matches[which(matches$species == data$species[obs]),]
    matches = matches[which(matches$decimalLatitude == data$decimalLatitude[obs]),]
    matches = matches[which(matches$decimalLongitude == data$decimalLongitude[obs]),]
    if (nrow(matches) > 0) {duplicates = c(duplicates, as.numeric(rownames(matches)))}
  }
  return(sort(unique(duplicates)))
}

drop_repeats2 = function(data) {
  rr = NULL
  counter = 0
  for (sp in unique(data$species)) {
    dsp = data[which(data$species == sp),]
    for (lat in unique(dsp$decimalLatitude)) {
      dlat = dsp[which(dsp$decimalLatitude == lat),]
      for (long in unique(dlat$decimalLongitude)) {
        dlong = dlat[which(dlat$decimalLongitude == long),]
        for (year in unique(dlong$year)) {
          dyear = dlong[which(dlong$year == year),]
          for (month in unique(dyear$month)) {
            dmonth = dyear[which(dmonth$month == month),]
            rr = rownames(dmonth)[2:nrow(dmonth)]
          }
        }
      }
    }
    counter = counter + 1
    if ((counter %% 100) == 0) {
      print(paste("The program is aproximately ", round(counter/length(unique(data$species)), 1), "% done.", sep = "" ))
    }
  }
  return(rr)
}





## ACTUAL USAGE ##

total = read.csv("GBIF Total.csv", sep = "\t", stringsAsFactors = FALSE)

removal_rows = drop_repeats(total)
