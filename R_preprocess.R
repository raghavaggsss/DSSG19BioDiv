library(tidyverse)

## ACTUAL USAGE 2 ##

total = read.csv("GBif_Original.csv", stringsAsFactors = FALSE, row.names=NULL)

unq_rows = as.numeric(rownames(unique(total[c("species","year","decimalLatitude","decimalLongitude")])))
total_unq = total[unq_rows,]

total$coor = paste0(total$decimalLatitude, total$decimalLongitude)
total_unq$coor = paste0(total_unq$decimalLatitude, total_unq$decimalLongitude)
month_count = total %>% group_by(species, year, coor) %>% summarise(paste(unique(month), collapse = ", "))
colnames(month_count)[ncol(month_count)] = "months"

total_unq = total_unq[order(total_unq$species, total_unq$year, total_unq$coor),]
month_count = month_count[order(month_count$species, month_count$year, month_count$coor),]
#total_unq$months = month_count$`paste(unique(month), collapse = ", ")`
#sep_months = sapply(total_unq$months, strsplit, ", ")
sep_months = month_count$months

total_unq$Winter = 0
total_unq$Spring = 0
total_unq$Summer = 0
total_unq$Fall = 0
for (row in 1:nrow(total_unq)) {
  if (any(c(1,2,12) %in% sep_months[row][[1]][1])) {total_unq$Winter[row] = 1}
  if (any(c(3:5) %in% sep_months[row][[1]][1])) {total_unq$Spring[row] = 1}
  if (any(c(6:8) %in% sep_months[row][[1]][1])) {total_unq$Summer[row] = 1}
  if (any(c(9:11) %in% sep_months[row][[1]][1])) {total_unq$Fall[row] = 1}
  #if (row %% 15000 == 0) {print(paste("Loop is ", round(row/nrow(total_unq)*100), "% done", sep = ""))}
}

write.csv(total_unq, "GBif_R.csv")
