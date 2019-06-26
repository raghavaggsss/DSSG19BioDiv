library(tidyverse)
# Taxonomic Name Resolution Service 
# writing the plant species names to .txt file to upload to the service 

# subset the gbif plantae kingdom 
plants <- subset(trimmed_gbif, trimmed_gbif$kingdom=="Plantae")

# get species names only 
plant_species <- unique(na.omit(plants$species))

# write species name to .txt file 
write.table(x = plant_species, file = "gbif_plant_names.txt", sep = ",", row.names = F, col.names = F)


# read in rainbow list 
rainbow_list <- read.csv("bc_rainbow_list.tsv", sep = "\t", na.strings = "")

# subset rainbow plants 
rainbow_plants <- filter(rainbow_list, Kingdom=="Plantae") %>% 
      filter(Phylum!="Mitosporic fungi" & Phylum!="Ascomycota")

# get rainbow plant species names 
rainbow_plant_names <- na.omit(unique(rainbow_plants$Scientific.Name))

# write rainbow plant species name to file 
write.table(x = rainbow_plant_species, file = "rainbow_plant_names.txt", sep = ",", row.names = F, col.names = F)


##### TNRS Results ######## 


###### gbif plant TNRS results  #### 
# how many plant species names were there on rainbow? 6,901
length(plant_species)

# read in the TNRS results 
gbif_tnrs <- read.csv("gbif_plant_tnrs_results.csv", na.strings = c("", " "), stringsAsFactors = F)

gbif_tnrs_trim <- select(gbif_tnrs, c(Name_number, 
                                            Name_submitted, 
                                            Overall_score,
                                            Name_matched, 
                                            Name_matched_rank,
                                            Name_matched_author,
                                            Name_score, 
                                            Genus_score, 
                                            Specific_epithet_matched,
                                            Specific_epithet_score,
                                            Unmatched_terms,
                                            Taxonomic_status,
                                            Accepted_name,
                                            Accepted_name_rank,
                                            Warnings))

# what percent of gbif doesn't have a match at 100%? 
nrow(filter(gbif_tnrs_trim, Overall_score!=1))/nrow(gbif_tnrs_trim)*100


# how many don't have a full match of 100%? 314
nrow(filter(gbif_tnrs_trim, Overall_score!=1))

# subset the names with 100% overall score 
gbif_100_match <- filter(gbif_tnrs_trim, Overall_score==1)

nrow(gbif_100_match)-nrow(filter(gbif_100_match, Taxonomic_status=="Accepted"))

nrow(filter(gbif_100_match, Taxonomic_status=="Accepted"))/nrow(gbif_100_match)*100


# percent of the names with 100% match and were matched to accepted name 
nrow(filter(gbif_100_match, Taxonomic_status=="Accepted"))/length(plant_species)*100





# of these that don't match overall at 100%, how many have a genus score of <100%, 177
nrow(filter(gbif_tnrs_trim, Genus_score!=1))

# explore the names that don't match on genus level
View(filter(gbif_tnrs_trim, Genus_score!=1))

# of the ones that don't match on genus level, do they have a epithet match? 
# 12 don't have a match of zero. some match 100% but most have partial scores, epithet matches in part
nrow(filter(drop_na(filter(gbif_tnrs_trim, Genus_score!=1), Specific_epithet_score), 
            Specific_epithet_score!=0))


# view the names that do not have a genus match and have an epithet match that isn't zero
View(filter(drop_na(filter(gbif_tnrs_trim, Genus_score!=1), Specific_epithet_score), 
            Specific_epithet_score!=0))

# name # 144: appears to be an algae on algaeBase, wouldn't be a legit match on TNRS 
# name # 



# of the ones that don't match on genus level, how many of these have no match at all in TNRS?
nrow(filter(filter(gbif_tnrs_trim, Genus_score!=1), Overall_score!=0))




nrow(filter(gbif_tnrs_trim, Overall_score==0))



# subset the obs w/o a full 100% match 
gbif_no_100_match <- filter(gbif_tnrs, Overall_score!=1)

# of these that don't have 100% match, how many have a specific epithet score of less than 100%
nrow(filter(gbif_no_100_match, Specific_epithet_score!=1))

# subset the names that do not match on either the genus or specific epithet level 
gbif_no_match_overall <- filter(gbif_no_100_match, Specific_epithet_score!=1)







######## rainbow TNRS results ######
# how many plant species names were there on rainbow? 3,485
length(rainbow_plant_species)

# read in the TNRS results 
rainbow_tnrs <- read.csv("rainbow_plant_tnrs.csv", na.strings = c("", " "), stringsAsFactors = F)

rainbow_tnrs_trim <- select(rainbow_tnrs, c(Name_number, 
                                            Name_submitted, 
                                            Overall_score,
                                            Name_matched, 
                                            Name_matched_rank,
                                            Name_matched_author,
                                            Name_score, 
                                            Genus_score, 
                                            Specific_epithet_matched,
                                            Specific_epithet_score,
                                            Unmatched_terms,
                                            Taxonomic_status,
                                            Accepted_name,
                                            Accepted_name_rank,
                                            Warnings))

# what percent of the rainbow names had an overall score of less than 100%?
nrow(filter(rainbow_tnrs_trim, Overall_score!=1))/nrow(rainbow_tnrs_trim)*100

# subset the names with 100% overall score 
rainbow_100_match <- filter(rainbow_tnrs_trim, Overall_score==1)
nrow(rainbow_100_match)

nrow(filter(rainbow_100_match, Taxonomic_status=="Accepted"))

nrow(filter(rainbow_100_match, Taxonomic_status=="Accepted"))/nrow(rainbow_100_match)*100


# the names with 100% match and were matched to accepted name 
nrow(filter(rainbow_100_match, Taxonomic_status=="Accepted"))/nrow(rainbow_tnrs)*100




# how many don't match 100% at the overall level (includes genus and epithet)?
nrow(filter(rainbow_tnrs_trim, Overall_score!=1))


nrow(filter(rainbow_tnrs_trim, Overall_score!=1))/length(rainbow_plant_species)*100





# subset the rainbow_tnrs that do no have a match of 100% 
rainbow_less_100_overall <- filter(rainbow_tnrs_trim, Overall_score!=1)

# of these that do not match 100%, how many have a genus score of <100%?
nrow(filter(rainbow_less_100_overall, Genus_score!=1))

# take a look at these names that don't match on genus level, do they match on species level?
# no, they don't match on species level either 
View(filter(rainbow_less_100_overall, Genus_score!=1))

# name #: 449, don't think the matched name is a match at all. submitted name is a lichen, 
# matched name is a flowering plant 

# name #: 768, don't think the matched name is a match at all. gbif gives the synonym for the submitted name 
# and it doesn't include the matched name on TNRS

# name # 2209: no google results for this submitted name. probably doesn't actually match anything in TNRS

# name # 2603, no match found in TNRS 


# of these that do not match 100%, how many did not have a specific epithet score of 100%?
# only 8 
nrow(filter(rainbow_less_100_overall, Specific_epithet_score!=1))

# subset the names that do not match on specific epithet level 
rainbow_no_match_epithet <- filter(rainbow_less_100_overall, Specific_epithet_score!=1)

# explore the names that do not match on epithet level. Do they all match on genus level? 
# the only one that doesn't match on genus level was the one that had no match at all in TNRS 
sum(rainbow_no_match_epithet$Genus_score==1)

# look at the names that do not match on epithet level 
View(rainbow_no_match_epithet)

# all of these appear to be true matches where the named matched is probably a synonym to submimtted name 
# would want to compare lists that included these extra names 
