library(tidyverse)
########## SEI and species prediction #########

# read in SEI data 
df_sei_polygons <- read.csv("sei_data.csv", stringsAsFactors = F)

# read in s
















# create a sample dataframe of the input data I will receive 

# make sample vectors 
poly_index <- c(1:24, 1)
poly_type1 <- c(rep(c("OF", "MF", "WD", "RI", "WN", "HB", "SV", "ES", "IT", "FW", "AP", "ME"), 2), "OF")
poly_comp1 <- c(rep(1, 12), 0.8, rep(1, 11),1)
poly_type2 <- c(rep(NA, 12),"WD", rep(NA, 11), NA)
poly_comp2 <- c(rep(NA, 12),0.2, rep(NA, 11), NA)
species <- c(rep("Abagrotis baueri", 3), rep("Abies balsamea", 4), "Abies bracteata", "Acanthocalyx alba", rep("Acacia pravissima", 3),
             rep("Abagrotis baueri", 3), "Acaena splendens", "Abrostola urentis", rep("Abies balsamea", 2), "Abies bracteata", "Acanthocalyx alba", rep("Acacia pravissima", 3),
             "Acanthocalyx alba")

# create the whole species_SEI dataframe 
df_polygons <- data.frame(poly_index, poly_type1, poly_comp1, poly_type2, poly_comp2, species)

# filter by species
filter(df_polygons, species == "Abagrotis baueri") %>% 
    filter(poly_type1 == "OF") %>% 
  select(poly_index) %>% 
  nrow()





                            