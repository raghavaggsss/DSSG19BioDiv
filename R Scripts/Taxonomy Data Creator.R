data = read.csv("GBif June27.csv", stringsAsFactors = FALSE)

sp_rows = as.numeric(rownames(unique(data["species"])))
spdata = data[sp_rows,c(7:8,11:17)]

tab = table(data$species)

for (sp in 1:nrow(spdata)) {
  spdata$freq = tab[which(rownames(tab)==spdata$species)]
}

####
# Add BC RainbowList Information Here
####

write.csv(spdata, "Taxonomy Freq.csv", row.names = FALSE)
