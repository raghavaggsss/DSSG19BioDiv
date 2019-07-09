df = read.csv("GBif with Municipalities and Years.csv", stringsAsFactors = F)

df_sub <- head(df, 1000)

saveRDS(df, "./mun_years.rds")

df_yearagg = plyr::count(df, c('year'))
