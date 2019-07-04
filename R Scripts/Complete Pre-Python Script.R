library(tidyverse)

## REMOVING species-by-location-by-year-by-month DUPLICATES AND ADDING SEASONS

#Read in data
total = read.csv("GBif_Original.csv", stringsAsFactors = FALSE, sep="\t")
#Remove duplicate rows
unq_rows = as.numeric(rownames(unique(total[c("species","year","decimalLatitude","decimalLongitude")])))
total2 = total[unq_rows,]
#Get a list of months included in each of the new entries
month_count = total %>% group_by(species, year, decimalLatitude, decimalLongitude) %>% summarise(paste(unique(month), collapse = ", "))
colnames(month_count)[ncol(month_count)] = "months"
#Use these months to create Season variables for each of the new entries
total2 = total2[order(total2$species, total2$year, total2$coor),]
month_count = month_count[order(month_count$species, month_count$year, month_count$coor),]
sep_months = month_count$months
total2$Winter = 0
total2$Spring = 0
total2$Summer = 0
total2$Fall = 0
for (row in 1:nrow(total2)) {
  if (any(c(1,2,12) %in% sep_months[row][[1]][1])) {total2$Winter[row] = 1}
  if (any(c(3:5) %in% sep_months[row][[1]][1])) {total2$Spring[row] = 1}
  if (any(c(6:8) %in% sep_months[row][[1]][1])) {total2$Summer[row] = 1}
  if (any(c(9:11) %in% sep_months[row][[1]][1])) {total2$Fall[row] = 1}
  #if (row %% 15000 == 0) {print(paste("Loop is ", round(row/nrow(total2)*100), "% done", sep = ""))}
}
total2$month = NULL


## STOP HERE FOR A DATASET WITH YEARS PRESERVED
## REMOVING species-by-location-by-year DUPLICATES AND ADDING RECENCY

#Remove duplicate rows
unq_rows = as.numeric(rownames(unique(total2[c("species","decimalLatitude","decimalLongitude")])))
total3 = total[unq_rows,]
#Create a function to determine whether a vector of years contains only years prior to 2000, only years after 1999, or both
recency = function(x) {
  if (any(na.omit(x) >= 2000)) {
    if (any(na.omit(x) < 2000)) {return("both")}
    else {return("recent")}
  }
  else {return("old")}
}
#Similar to Seasons above, create a vector of recency and order it in such a way as to match with the new data, then add it as a new column and remove years
y = total %>% group_by(species, decimalLatitude, decimalLongitude) %>% summarise(recency(year))
colnames(y)[ncol(y)] = "recency"
total3 = total3[order(total3$species, total3$decimalLatitude, total3$decimalLongitude),]
y = y[order(y$species, y$decimalLatitude, y$decimalLongitude),]
total3$recency = y$recency
total3$year = NULL
#Aggregate across the rows of the old data to produce a dataframe of only nonduplicates containing 1s and 0s to indicate whether a given season was present in any of the old data rows that correspond to a new entry
seas = aggregate(cbind(Winter,Spring,Summer,Fall) ~ species + decimalLatitude + decimalLongitude, FUN = any, data = total)
total3[,c("Winter","Spring","Summer","Fall")] = seas[,c("Winter","Spring","Summer","Fall")]
#This is just to drop rows without species
total3 = total3[total3$species!="",]

#Finally, write the data
write.csv(total3, "GBif_R.csv", row.names = FALSE)