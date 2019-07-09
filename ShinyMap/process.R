df = read.csv("./GBif_recency.csv", stringsAsFactors = F)
colnames(df)[colnames(df)=="decimalLatitude"] <- "Latitude"
colnames(df)[colnames(df)=="decimalLongitude"] <- "Longitude"

df_sub <- head(df, 1000)

saveRDS(df, "./GBif_recency.rds")
