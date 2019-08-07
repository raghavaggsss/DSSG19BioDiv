# Final Report Plots 

# plot the gbif observations for the last 20 years 
filter(df_orig, year >=1998 & year<2019) %>% 
  select(year) %>% 
  table() %>% 
  as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq))+
  geom_col()+
  labs(title = "GBIF Records Over Time",
       x = "Year", 
       y = "Number of Records")+
  theme_classic()
  
