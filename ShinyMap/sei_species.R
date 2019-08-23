library(tidyverse)
########## SEI and Species Prediction #########

# read in SEI data 
df_sei_polygons <- read.csv("sei_data_full.csv", stringsAsFactors = F)


# select the SEI columns that are relevant
trim_df_sei_polygons <- df_sei_polygons %>% select(7, 15:17,21:23, 27:29, 64:66)

# convert the polygon component deciles into proportions 
trim_df_sei_polygons <- mutate(trim_df_sei_polygons, 
                               comp1_prop = SEDec_1/10,
                               comp2_prop = SEDec_2/10,
                               comp3_prop = SEDec_3/10)
# truncated SEI data 
######df_sei_polygons <- read.csv("sei_data_truncated.csv", stringsAsFactors = F)

# rename the poly index column to have the same column name as df_obs index column
trim_df_sei_polygons <- rename(trim_df_sei_polygons, poly_index = SEI_PolyNb)

# gather the proportion information for comp1 -comp3 into the same column
trim_df_sei_polygons <- cbind(trim_df_sei_polygons, gather(trim_df_sei_polygons[,14:16], key = "component", value = "component_prop"))


# read in species mapped to polygon index data 
df_obs <- read.csv("gbif_map_poly.csv", stringsAsFactors = F)

# select the most relevant columns of df_obs
df_obs <- select(df_obs, 16, 18)

# remove duplicate rows
df_obs <- unique(df_obs)

# truncated species data 
######df_obs <- read.csv("gbif_sei_poly_collapsed.csv", stringsAsFactors = F)

# merge the SEI data into the species observation data 
df_obs <- merge(x = df_obs, 
                  y = trim_df_sei_polygons, 
                  by = "poly_index")



SEI_class_prop <- c()
for (i in unique(trim_df_sei_polygons$SECl_1)) {
  se_class_proportions <- c()

  # filter data to single SEI class
  SE_class <- filter(trim_df_sei_polygons, SECl_1 == i)

  # get number of polygons in single SEI class
  count_polys_in_class <- sum(SE_class$component_prop)

  for(k in unique(df_obs$species)){

    # filter data to a single organism
    organism_df <- filter(df_obs, species ==k)
    
    # find the indices where organism occurs in given SEI class
    ind <- which(organism_df$poly_index %in% SE_class$poly_index)
    
    # sum the number of polys the organism occurs in for given SEI class
    num_organism_in_class <- sum(organism_df[ind,]$component_prop)

    # calculate proportion of organism sightings in the given SEI class polygons (including all polygons belonging to the SEI class)
    prop_in_class <- num_organism_in_class/count_polys_in_class
    
    # create the proportion vector for given SEI class
    se_class_proportions <- c(se_class_proportions, prop_in_class)
  }

  # combine the proportion calculations for each species for a class into a dataframe
  SEI_class_prop <- cbind(SEI_class_prop, se_class_proportions)

}


# convert the proportion matrix into a dataframe
SEI_class_prop <- as.data.frame(SEI_class_prop)

# add SEI class abbreviations as column names
colnames(SEI_class_prop) <- unique(trim_df_sei_polygons$SECl_1)

# add the unique species column
SEI_class_prop <- cbind(species = unique(df_obs$species), SEI_class_prop)









#########   scratchwork and validation of results ##########
# species 1 = Circus hudsonius

# species 2 = Corvus caurinus

# species 3 = Colaptes auratus

# species 4 = Columba livia

# species 5 = Vanessa annabella

# species 6 = Accipiter cooperi

se_class_proportions <- c()
#class_columns <- c()
#organism_df <- filter(df_obs, species =="Circus hudsonius")

organism_df <- filter(df_obs, species =="Accipiter cooperi")

SE_class <- filter(trim_df_sei_polygons, SECl_1 == "YS")

count_polys_in_class <- nrow(SE_class)


# calculate proportion of organism sightings in the given SEI class polygons (including all polygons belonging to the SEI class)

# which are the polys where "organism" occurs in class "RI"
ind <- which(organism_df$poly_index %in% SE_class$poly_index)

# sum the number of polygons organism occurs in, for given SEI class 
num_organism_in_class <- sum(organism_df[ind,]$comp1_prop)
#num_organism_in_class <- length(which(organism_df$poly_index %in% SE_class$SEI_PolyNb))
prop_in_class <- num_organism_in_class/count_polys_in_class
se_class_proportions <- c(se_class_proportions, prop_in_class)

c(se_class_proportions,
  length(which(organism_df$poly_index %in% SE_class$SEI_PolyNb))/count_polys_in_class)







  



                            