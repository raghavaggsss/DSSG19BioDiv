library(tidyverse)
library(reshape)

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
            dmonth = dyear[which(dyear$month == month),]
            if(nrow(dmonth > 1)) {rr = c(rr, as.numeric(rownames(dmonth)[2:nrow(dmonth)]))}
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

unq_rows = as.numeric(rownames(unique(total[c("species","year","month","decimalLatitude","decimalLongitude")])))
total_unq = total[unq_rows,]

total_unq_trim = total_unq[,c(2,4:10,17:18,25:28)]

write.csv(total_unq_trim, "GBIF Total No-Dupe Trim.csv")




## ACTUAL USEAGE 2 ##

total = read.csv("GBIF Total No-Dupe Trim.csv", stringsAsFactors = FALSE)

dict = cbind(c(1:12),c(rep(c(1:4),each=3)))
colnames(dict) = c("Month","Season")

unq_rows = as.numeric(rownames(unique(total[c("species","year","decimalLatitude","decimalLongitude")])))
total_unq = total[unq_rows,]

total_unq$season = NULL
for (sp in unique(total$species)) {
  for (y in unique(total$year[which(total$species == sp)])) {
    for (lat in unique(total$decimalLatitude[which(total$species == sp & total$year == y)])) {
      for (long in unique(total$decimalLongitude[which(total$species == sp & total$year == y & total$decimalLatitude == lat)])) {
        sub = total$month[which(total$species == sp & total$year == y & total$decimalLatitude == lat &
                  total$decimalLongitude == long)]
        m = unique(sub)
        total_unq$season[which(total_unq$species == sp & total_unq$year == y)] = m
      }
    }
  }
}

total_unq = total_unq[,-c("day","month","X","")]




total$coor = paste0(total$decimalLatitude, total$decimalLongitude)
total_unq$coor = paste0(total_unq$decimalLatitude, total_unq$decimalLongitude)

month_count = total %>% group_by(species, year, coor) %>% summarise(paste(unique(month), collapse = ", "))
colnames(month_count)[ncol(month_count)] = "months"

total_unq = total_unq[order(total_unq$species, total_unq$year, total_unq$coor),]
month_count = month_count[order(month_count$species, month_count$year, month_count$coor),]
total_unq$months = month_count$`paste(unique(month), collapse = ", ")`

sep_months = sapply(total_unq$months, strsplit, ", ")


total_unq$Winter = 0
total_unq$Spring = 0
total_unq$Summer = 0
total_unq$Fall = 0
for (row in 1:nrow(total_unq)) {
  if (any(c(1,2,12) %in% sep_months[row][[1]][1])) {total_unq$Winter[row] = 1}
  if (any(c(3:5) %in% sep_months[row][[1]][1])) {total_unq$Spring[row] = 1}
  if (any(c(6:8) %in% sep_months[row][[1]][1])) {total_unq$Summer[row] = 1}
  if (any(c(9:11) %in% sep_months[row][[1]][1])) {total_unq$Fall[row] = 1}
  if (row %% 15000 == 0) {print(paste("Loop is ", round(row/nrow(total_unq)*100), "% done", sep = ""))}
}

total_unq = total_unq[,-c(1,11:13,16:18,23)]

write.csv(total_unq, "GBif Trim June19.csv", row.names = FALSE)

























library(tidyverse)

total = read.csv("GBif Lesley.csv", stringsAsFactors = FALSE)

unq_rows = as.numeric(rownames(unique(total[c("species","decimalLatitude","decimalLongitude")])))
total_unq = total[unq_rows,]

recency = function(x) {
  if (any(na.omit(x) >= 2000)) {
    if (any(na.omit(x) < 2000)) {return("both")}
    else {return("recent")}
  }
  else {return("old")}
}

y = total %>% group_by(species, decimalLatitude, decimalLongitude) %>% summarise(recency(year))
colnames(y)[ncol(y)] = "years"

total_unq = total_unq[order(total_unq$species, total_unq$decimalLatitude, total_unq$decimalLongitude),]
y = y[order(y$species, y$decimalLatitude, y$decimalLongitude),]
total_unq$recency = y$years
total_unq$year = NULL

seas = aggregate(cbind(Winter,Spring,Summer,Fall) ~ species + decimalLatitude + decimalLongitude, FUN = sum, data = total)
total_unq[,c("Winter","Spring","Summer","Fall")] = seas[,c("Winter","Spring","Summer","Fall")]

# This is just to drop rows without species
x = total_unq[total_unq$species!="",]

write.csv(total_unq, "GBif June26.csv")


