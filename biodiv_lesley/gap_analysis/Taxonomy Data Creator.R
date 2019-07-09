library(tidyverse)
# read in the data 
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

# create new column with variety portion of each species name removed 
spdata = mutate(spdata, simplified_names = gsub(pattern = " var.*", replacement = "", x = spdata$species))

# merge the rainbow information for species that match to gbif
spdata = merge(x = spdata, y = rainbow_list, by = "simplified_names", all.x = T, all.y = F)

# keep rainbow list columns that are desired: 
# - Habitat.subtype, origin, breeding, endemic, bc status, regional distrcts 

spdata = select(spdata, c(1:12, 25, 49, 46, 49, 50, 52, 54))

# make new csv
write.csv(spdata, "Taxonomy Freq.csv", row.names = FALSE)
