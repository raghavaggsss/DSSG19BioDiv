# creation of with only selected columns for summary plotting 
summary_df <- read.csv("/Users/lesley/Desktop/DSSG2019/DSSG19BioDiv-master/biodiv_lesley/gbif_complete.csv", 
                sep = "\t",
                stringsAsFactors = F,
                na.strings = c("", " "))
summary_df <- summary_df %>% drop_na(year, species)

summary_df <- summary_df %>% select(datasetKey, species, decimalLatitude, decimalLongitude, year)

write_csv(summary_df, "./gbif_summary_pre.csv")
