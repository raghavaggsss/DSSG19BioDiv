df_fulltax = merge(df_orig, dfsp[,c("species","kingdom","phylum","class","order","family","genus")], by = "species", all.x = T)
yearly_king = df_fulltax %>% group_by(year, kingdom) %>% tally()
colnames(yearly_king)[2] = "member"
yearly_king = add_zeros1(yearly_king)
yearly_phyl = df_fulltax %>% group_by(year, phylum) %>% tally()
colnames(yearly_phyl)[2] = "member"
yearly_phyl = add_zeros1(yearly_phyl)
yearly_class = df_fulltax %>% group_by(year, class) %>% tally()
colnames(yearly_class)[2] = "member"
yearly_class = add_zeros1(yearly_class)
