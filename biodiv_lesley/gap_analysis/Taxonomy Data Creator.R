library(tidyverse)
# read in the data 
data = read.csv("./datasets/GBif June27.csv", stringsAsFactors = FALSE, na.strings = c("", " "))

sp_rows = as.numeric(rownames(unique(data["species"])))
spdata = data[sp_rows,c(7:8,11:17)]

tab = table(data$species)

for (sp in 1:nrow(spdata)) {
  spdata$freq = tab[which(rownames(tab)==spdata$species)]
}

####
# Add BC RainbowList Information Here to the species specific info 

#### Process the rainbow list dataframe 
rainbow_list <- # load BC Red+Blue+ Yellow list (Rainbow list)
      rainbow_list <- read.delim("./datasets/bc_rainbow_list.tsv", sep = "\t", stringsAsFactors = FALSE, na.strings = c("", " "))

# Add a column that strips the var. from any species name 
rainbow_list <- mutate(rainbow_list, simplified_names = gsub(pattern = " var.*", replacement = "", x = rainbow_list$Scientific.Name))



# create new column with variety portion of each species name removed 
spdata = mutate(spdata, simplified_names = gsub(pattern = " var.*", replacement = "", x = spdata$species))

# merge the rainbow information for species that match to gbif
spdata = merge(x = spdata, y = rainbow_list, by = "simplified_names", all.x = T, all.y = F)

# keep rainbow list columns that are desired: 
# - Habitat.subtype, 
# endemic, bc status, regional distrcts, SARA

spdata = select(spdata, c(1:11, 25, 32,46, 49, 54))

# make new csv
write.csv(x = spdata, file = "Taxonomy_Freq.csv", row.names = FALSE)
