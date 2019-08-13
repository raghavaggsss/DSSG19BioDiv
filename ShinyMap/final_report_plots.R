# Final Report Plots 
library(tidyverse)
library(gridExtra)
library(grid)
library(scales)
library(VennDiagram)
library(ggthemes)
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


# make a df of BC list species that are tagged as MVRD
bc_mvrd <- rainbow_list[grep("MVRD", rainbow_list$Regional.Dist),]


# compare the bc mvrd list to gbif species
# how many of the bc mvrd species are found in gbif?
overlap <- sum(bc_mvrd$Scientific.Name%in%unique(sp_df$scientific_name))

# what percent of the bc mvrd list is found in gbif?
sum(bc_mvrd$Scientific.Name%in%unique(sp_df$scientific_name))/nrow(bc_mvrd)*100

# make a Venn diagram of the overlap 
draw.pairwise.venn(area1 = nrow(bc_mvrd),
                   area2 = length(unique(sp_df$simplified_names)),
                   cross.area = ceiling(overlap),
                   category = c("GBIF", "BC MVRD"),
                   lty = rep("blank", 2),
                   fill = c("blue", "cyan"),
                   alpha = rep(0.5, 2),
                   cat.dist = rep(0.025, 2),
                   scaled = FALSE,
                   cat.fontface = rep("bold", 2),
                   cat.cex = 2,
                   cex = rep(2, 3), 
                   cat.pos = c(-80, 80))
 




