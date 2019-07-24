df_partialtax = merge(df_orig, dfsp[,c("species","kingdom","class")], by = "species", all.x = T)
yearly_total <- group_by(df_partialtax, year) %>% tally() %>% drop_na()
yearly_king <- group_by(df_partialtax, year, kingdom) %>% tally() %>% drop_na()
yearly_class <- group_by(df_partialtax, year, class) %>% tally() %>% drop_na()

combined_norm = yearly_total
colnames(combined_norm)[2] = "Total"
for (k in unique(yearly_king$kingdom)) {
  combined_norm = merge(combined_norm, yearly_king[which(yearly_king$kingdom == k), c("year", "n")], by = "year", all = T)
  colnames(combined_norm)[ncol(combined_norm)] = k
}
for (c in unique(yearly_class$class)) {
  combined_norm = merge(combined_norm, yearly_class[which(yearly_class$class == c), c("year", "n")], by = "year", all = T)
  colnames(combined_norm)[ncol(combined_norm)] = c
}

write.csv(combined_norm, "norms by year.csv", row.names = F)
saveRDS(combined_norm, "norms by year.rds")
