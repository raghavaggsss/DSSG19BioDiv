# Final Report Plots 
library(tidyverse)
library(gridExtra)
library(grid)

df_orig <- readRDS("gbif_summary.rds")

sp_df <- read.csv("Taxonomy_Freq.csv", stringsAsFactors = FALSE)

gbif_complete <- readRDS("~/Desktop/datasets/gbif_complete.rds")

rainbow_list <- read.csv("~/Desktop/datasets/bc_rainbow_list.tsv", sep = "\t", stringsAsFactors = FALSE)



# plot the gbif observations for the last 20 years 
filter(df_orig, year >=1998 & year<2019) %>% 
  select(year) %>% 
  table() %>% 
  as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq))+
  geom_col(show.legend = F)+
  labs(title = "GBIF Records Over Time",
       x = "Year", 
       y = "Number of Records")+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold"))




# make a frequency table of the dataset names 
dataset_freq <- as.data.frame(table(df_orig$datasetName))
colnames(dataset_freq) <- c("Dataset Name", "Number of Records")

# make a table of the top 20 datasources
top_10_datasets <- head(arrange(dataset_freq, desc(`Number of Records`)), 10)

# make a table plot of the top 10 datasets 
grid.table(top_10_datasets)


# get the gbif raw plant observations 
plants_raw <- filter(gbif_complete, kingdom=="Plantae")









