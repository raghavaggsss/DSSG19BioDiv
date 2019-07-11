df = read.csv("./GBif_recency.csv", stringsAsFactors = F)
colnames(df)[colnames(df)=="decimalLatitude"] <- "Latitude"
colnames(df)[colnames(df)=="decimalLongitude"] <- "Longitude"

df_sub <- head(df, 1000)

saveRDS(df, "./GBif_recency.rds")





df2 <- read.csv("/Users/lesley/Desktop/datasets/gbif_complete.csv", 
                sep = "\t",
                stringsAsFactors = F,
                na.strings = c("", " "))

# filter out the observations that have NA, unknown placement, viruses and archaea for kingdom
df2 <- filter(df2, 
                  kingdom!="incertae sedis" & kingdom!="NA" & kingdom!="Archaea" & kingdom!="Viruses")

# select the most relevant columns 
df2 <- select(df2, c(datasetKey,kingdom, phylum, class, order,
                                       family,genus,species,taxonRank,locality, decimalLatitude,decimalLongitude,
                                       month, year, basisOfRecord))

# Add a column that strips the var. from any species name 
df2 <- mutate(df2, simplified_names = gsub(pattern = " var.*", replacement = "", x = df2$species))

# Drop any NAs from the species and simplified names columns 
df2 <- drop_na(df2, species, simplified_names)

saveRDS(df2, "/Users/lesley/Desktop/datasets/gbif_complete.rds")
