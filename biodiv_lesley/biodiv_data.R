library(tidyverse)
library(gridExtra)

# data exploration for all data sources on Gbif
# What is the nature of all these data sources? 
# what does it record? 
# How was the data collected? 
# what is the quality of the data? 

#########    Data Exploration on GBIF complete    #########

# read in the complete gbif data frame including all ebirds etc for metroVan
gbif <- read.csv("gbif_complete.csv", sep = "\t", stringsAsFactors = FALSE, na.strings = "")

# Taxonomy for Archaea is very spotty. most do not have anything close to a species level 
# identification. 
View(head(filter(gbif, kingdom=="Archaea"), 50))


# filter out the observations that have NA, unknown placement, viruses and archaea for kingdom
trimmed_gbif <- filter(gbif, 
                       kingdom!="incertae sedis" & kingdom!="NA" & kingdom!="Archaea" & kingdom!="Viruses")

trimmed_gbif <- select(trimmed_gbif, c(datasetKey,kingdom, phylum, class, order,
                                       family,genus,species,taxonRank,locality, decimalLatitude,decimalLongitude,
                                       month, year, basisOfRecord))

# A 


# IUCN Red List Data for all species in North America 
red_list <- read.csv("./redlist_all_species/assessments.csv", stringsAsFactors = FALSE, na.strings = "")


# eBird Observation Dataset 
"4fa7b334-ce0d-4e88-aaae-2e0c138d049e"%in%trimmed_gbif$datasetKey






# Geographically tagged INSDC sequences
"ad43e954-dd79-4986-ae34-9ccdbd8bf568"%in%trimmed_gbif$datasetKey
insdc <- filter(trimmed_gbif,
                trimmed_gbif$datasetKey=="ad43e954-dd79-4986-ae34-9ccdbd8bf568")

# how many levels in the INSDC dataset? 
nrow(insdc) # 19130

# what are all the types of records in the this data? what type of data was collected?
levels(insdc$basisOfRecord)
# human observation
# living specimen
# material sample 
# observation 
# preserved  specimen 
# unknown 

# how many observations in the INSDC dataset have incertae sedis (ie. placement unknown) in their scientific name
length(which(insdc$scientificName=="incertae sedis"))   # 34 observations have an unknown placement 

# do the observations with unknown placement also have missing info for the rest of the taxonomic levels?  yes!!!!
filter(insdc, insdc$scientificName=="incertae sedis")

# how much of the data is from preserved specimen?  # 13347
length(which(insdc$basisOfRecord=="PRESERVED_SPECIMEN"))

# what percentage is from preserved specimen?  # ~ 70%
length(which(insdc$basisOfRecord=="PRESERVED_SPECIMEN"))/nrow(insdc)*100  

# what percentage is from Unknown? # none 
length(which(insdc$basisOfRecord=="UNKNOWN"))/nrow(insdc)*100 

# what percentage is from material sample? # ~ 30% 
length(which(insdc$basisOfRecord=="MATERIAL_SAMPLE"))/nrow(insdc)*100 


# Centre for Biodiversity Genomics - Canadian Specimens (10,317)
"f9a70dab-004b-45ad-90cb-24d8ff645b44"%in%trimmed_gbif$datasetKey
CBG <- filter(trimmed_gbif,
              trimmed_gbif$datasetKey=="f9a70dab-004b-45ad-90cb-24d8ff645b44")

# International Barcode of Life (iBOL)
"040c5662-da76-4782-a48e-cdea1892d14c"%in%trimmed_gbif$datasetKey
iBOL <- filter(trimmed_gbif,
               trimmed_gbif$datasetKey=="040c5662-da76-4782-a48e-cdea1892d14c")

# how many unique places was the data collected from?  #6 
length(unique(iBOL$locality))

# Great Backyard Bird Count 
backyard_bird_survey <- filter(trimmed_gbif,
                               trimmed_gbif$datasetKey=="82cb293c-f762-11e1-a439-00145eb45e9a")


# how many unique places was the data collected from? # 667
length(unique(backyard_bird_survey$locality))



# University of British Columbia Herbarium: Vascular plants 
"07fd0d79-4883-435f-bba1-58fef110cd13"%in%trimmed_gbif$datasetKey
ubc_herb_vas_plants <- filter(trimmed_gbif, 
                              trimmed_gbif$datasetKey=="07fd0d79-4883-435f-bba1-58fef110cd13")

# how many unique localities does the data come from?   # ~8,000
length(levels(ubc_herb_vas_plants$locality))

# what type of data is it? basis of the record? 
unique(ubc_herb_vas_plants$basisOfRecord)

# University of British Columbia Herbarium: Fungi collection 
"ca1bcd7e-7387-42f9-81ba-1470db55e3e8"%in%trimmed_gbif$datasetKey

ubc_herb_fungi <- filter(trimmed_gbif, 
                         trimmed_gbif$datasetKey=="ca1bcd7e-7387-42f9-81ba-1470db55e3e8")



# Royal BC museum: invertebrates collection 
royal_bc_mus <- filter(trimmed_gbif,
                       trimmed_gbif$institutionCode=="Royal British Columbia Museum")

# what locations does the data come from?  # no localities are reported 
unique(royal_bc_mus$locality)







###########      BC Rainbow list    ############ 
### this is our master species checklist for BC 

rainbow_list <- read.delim("./bc_rainbow_list.tsv", sep = "\t", stringsAsFactors = FALSE, na.strings = "")

# parsing the rainbow list names 
# some of the plant names in rainbow have the variety included in the name. when string matching with gbif, gbif might not have the variety included in the name
# so there would be a mismatch because the whole string doesn't match.
# use regular expressions to remove the var. part of the species names then compare the rainbow list again to GBIF.
sample_names <- c("Saccharomyces cerevisiaea var. sally", "Abies lasiocarpa var. lasiocarpa")

gsub(pattern = " var.*", replacement = "", x = sample_names)

sample_names_b <- c("Saccharomyces cerevisiaea", "jasmine ono", "Abagrotis apposita")

sum(sample_names_b%in%sample_names)

sample_names %in% sample_names_b



c("Saccharomyces cerevisiaea var. sally", "Abies lasiocarpa var. lasiocarpa")%in%c("Saccharomyces cerevisiaea", "jasmine ono", "Abagrotis apposita")


# add a column that strips the var. from any species name 
rainbow_list <- mutate(rainbow_list, simplified_names = gsub(pattern = " var.*", replacement = "", x = rainbow_list$Scientific.Name))



trim_rainbow_list <- select(rainbow_list, c(Scientific.Name, Global.Status, Prov.Status, 
                                            BC.List, Name.Category, Species.Level, Kingdom, Phylum, Class, Order, Family, simplified_names))




# how many species are on the rainbow list? 14186
nrow(rainbow_list)

#how many unique kingdoms are on rainbow list? 
# only 2, animals and plants 
length(unique(rainbow_list$Kingdom))

# number of unique phyla on rainbow list? 16
length(unique(rainbow_list$Phylum))

# compare the entire rainbow list to the IUCN red list?
# how many species names match? 90 matches on IUCN out of ~14,000 species on rainbow 
sum(na.omit(unique(rainbow_list$Scientific.Name))%in%red_list$scientificName)

# subset red and blue list 
bc_red_blue <- filter(rainbow_list, BC.List=="Blue" | BC.List== "Red")

# out of just the red/blue species on rainbow, how many match up on IUCN? # only 50 matches 
sum(bc_red_blue$Scientific.Name%in%red_list$scientificName) 

# subset the rainbow list to exclude red+blue species, yellow list only 
bc_yellow_etc <- filter(rainbow_list, BC.List!="Red" & BC.List!="Blue")

# make a dataframe of species on bc_yellow only that match IUCN
bc_yellow_matches <- bc_yellow_etc[which(bc_yellow_etc$Scientific.Name%in%red_list$scientificName),]

# what is the bc status of the yellow list matches? 
# 19 species have no Status on BC list but are found on IUCN
# 8 species have an unknown status in BC but are found on IUCN
table(bc_yellow_matches$BC.List)



## Functions 
# determine how many unique items in list x are found in list y
species_contained <- function(x, y){
      intersect <- sum(na.omit(unique(x))%in%y)
      print(intersect)
}

# determine what percent x is of y
percent_species_contained <- function(x, y){
      intersect <- sum(na.omit(unique(x))%in%y)
      percent <- intersect/length(na.omit(unique(y)))*100
      print(percent)
}

species_contained(plants_gbif$species, rainbow_list$Scientific.Name)





# how many of the gbif unique species are found on rainbow list? 
sum(unique(na.omit(trimmed_gbif$species))%in%rainbow_list$Scientific.Name)
num_gbif_on_rainbow <- sum(unique(na.omit(trimmed_gbif$species))%in%rainbow_list$Scientific.Name)

# how many of gbif unique species are found on bc_red_blue list? 
sum(unique(na.omit(trimmed_gbif$species))%in%bc_red_blue$Scientific.Name)

num_gbif_on_redBlue <- sum(unique(na.omit(trimmed_gbif$species))%in%bc_red_blue$Scientific.Name)

# what percent of unique gbif species are found on rainbow list? 
num_gbif_on_rainbow/length(unique(na.omit(trimmed_gbif$species)))*100

# what percent of unique gbif species are found on bc_red_blue list? 
num_gbif_on_redBlue/length(unique(na.omit(trimmed_gbif$species)))*100



##### percent of rainbow or red/blue #####

# how many rainbow list species are on gbif?
sum(rainbow_list$Scientific.Name%in%na.omit(unique(trimmed_gbif$species)))

# what percent of rainbow list is on gbif?
sum(rainbow_list$Scientific.Name%in%na.omit(unique(trimmed_gbif$species)))/length(rainbow_list$Scientific.Name)*100


# how many bc_red_blue species are found on gbif?
sum(bc_red_blue$Scientific.Name%in%na.omit(unique(trimmed_gbif$species)))

# what percent of the red/blue list is on gbif?
sum(bc_red_blue$Scientific.Name%in%na.omit(unique(trimmed_gbif$species)))/length(bc_red_blue$Scientific.Name)*100




##### modified rainbow ###### 
# now that the var. has been stripped from any rainbow species names, now check the intersection of the two lists

# how many rainbow list species are on gbif?
sum(trim_rainbow_list$simplified_names%in%na.omit(unique(trimmed_gbif$species)))

# how many gbif species are on rainbow?
sum(na.omit(unique(trimmed_gbif$species))%in%trim_rainbow_list$simplified_names)


# what percent of modified rainbow list is on gbif?
sum(trim_rainbow_list$simplified_names%in%na.omit(unique(trimmed_gbif$species)))/length(rainbow_list$Scientific.Name)*100

# summary
# when the var. is stripped out of the rainbow species names, you get an increased match of 319 species on the gbif 
# so 319 more species on the rainbow, match to gbif 









######## gbif plants #########
# subset gbif plants 
plants_gbif <- filter(trimmed_gbif, kingdom=="Plantae")

# how many of the unique gbif plant species are found on rainbow list?
sum(na.omit(unique(plants_gbif$species))%in%rainbow_list$Scientific.Name)

gbif_plant_on_rainbow <- sum(na.omit(unique(plants_gbif$species))%in%rainbow_list$Scientific.Name)

# what percent of gbif plants are found on rainbow?
gbif_plant_on_rainbow/length(na.omit(unique(plants_gbif$species)))*100

# plants found on rainbow are what percent of gbif species?
gbif_plant_on_rainbow/length(na.omit(unique(trimmed_gbif$species)))*100



####### gbif animals #######
# subset the gbif animals 
animal_gbif <- filter(trimmed_gbif, kingdom=="Animalia")

# how many of the unique gbif animal species are found on rainbow list?
sum(na.omit(unique(animal_gbif$species))%in%rainbow_list$Scientific.Name)

gbif_animal_on_rainbow <- sum(na.omit(unique(animal_gbif$species))%in%rainbow_list$Scientific.Name)

# what percent of gbif animal are found on rainbow?
gbif_animal_on_rainbow/length(na.omit(unique(animal_gbif$species)))*100

# plants found on rainbow are what percent of gbif species?
gbif_animal_on_rainbow/length(na.omit(unique(trimmed_gbif$species)))*100



####### gbif fungi #######
# subset the gbif fungi 
fungi_gbif <- filter(trimmed_gbif, kingdom=="Fungi")

# unqiue fungi species are what percent of the gbif unique species?
length(na.omit(unique(fungi_gbif$species)))/length(na.omit(unique(trimmed_gbif$species)))*100

# how many of the unique gbif fungi species are found on rainbow list?
sum(na.omit(unique(fungi_gbif$species))%in%rainbow_list$Scientific.Name)

gbif_fungi_on_rainbow <- sum(na.omit(unique(fungi_gbif$species))%in%rainbow_list$Scientific.Name)

# what percent of gbif fungi are found on rainbow?
gbif_fungi_on_rainbow/length(na.omit(unique(fungi_gbif$species)))*100

# fungi found on rainbow are what percent of gbif species?
gbif_fungi_on_rainbow/length(na.omit(unique(trimmed_gbif$species)))*100


# Conclusion: 
# of the GBIF species found on rainbow list, 
# 2% are fungi 
# ~12% are animals 
# ~ 10% are plants 

# ~ 18% of gbif plants are found on rainbow 
# ~ 44% of gbif animals are found on rainbow 
# ~ 12% of gbif fungi are found on rainbow 





# how many of the gbif species are found on the IUCN red list? #79
num_gbif_on_IUCN <- sum(unique(na.omit(trimmed_gbif$species))%in%red_list$scientificName)

# what percent of gbif species are found on IUCN red list? 
num_gbif_on_IUCN/length(red_list$scientificName)*100



#######  subset the plants   ########
rainbow_plants <- filter(rainbow_list, Kingdom=="Plantae")

# how many phyla of plants are there on the rainbow list? 11
length(unique(rainbow_plants$Phylum))


 
length(unique(rainbow_list$Name.Category))
unique(rainbow_list$Name.Category)

# vascular and non vascular plants
# vertebrate and invertebrates 
# fungus 

# identify phyla level gaps across entire dataset: only 4 of our 71 gbif phyla are found on rainbow 
sum(unique(trimmed_gbif$phylum)%in%rainbow_list$Phylum)





########  identify gaps in the Plant data ######

# out of our rainbow species list, how many phyla do we have represented in the data? 
sum(unique(plants$phylum)%in%unique(rainbow_plants$Phylum))

# we have only 2 matches between the two lists 
# so this means that most of the observations in the data don't fall into the phyla categories defined on rainbow list 
which(unique(plants$phylum)%in%unique(rainbow_plants$Phylum))


# need to investigate why there is only an overlap of 2 between the rainbow plant list and the plant dataset
# 1) is it differences in the taxonomy used? are the two lists using different classification schemes? 
# 2) does the BC yellow list actually missing a lot of the native plant species that we have? (we assumed the rainbow list was exhaustive and would mostly represent all of what we have in BC. this may not be the case in reality.)

# the answer to the above two questions is most likely #1 for now. BC rainbow list is using outdated taxonomy at the phylum level which may have consequences for the classification all the way down the taxon levels 

rainbow_plant_phyla <- unique(rainbow_plants$Phylum)
plant_phyla <- unique(plants$phylum)

# how many of our metro van plant species are on the IUCN red list? 196
sum(plants$species%in%red_list$scientificName)



# for plants, how many unique species are there on the rainbow list and for gbif? 
rainbow_plants <- filter(rainbow_list, Name.Category== "Vascular Plant" | Name.Category=="Nonvascular Plant")

# number of unqiue plant species on rainbow list 
length(rainbow_plants$Scientific.Name)

# number of unique plant species on gbif: 6902
length(unique(plants$species))

# how many of gbif plant species are on the rainbow list? 1,227 out of 3,484
# only 35% of the rainbow list finds a match on gbif 
sum(unique(plants$species)%in%rainbow_plants$Scientific.Name)



#### gap BC animal phyla #####

# subset the rainbow animals 
rainbow_animal <- filter(rainbow_list, Kingdom=="Animalia")

# identify the unique animalia phyla on rainbow list: 5 unique phyla 
unique(na.omit(rainbow_animal$Phylum))


# identify the number of unique animal gbif phyla 
length(unique(na.omit(animals$phylum)))

# what are the unique animal gbif phyla?
unique(na.omit(animals$phylum))

# how many animal phyla match betwen gbif and rainbow? only 1
sum(unique(na.omit(animals$phylum))%in%unique(na.omit(rainbow_animal$Phylum)))

unique(na.omit(animals$phylum))[which(unique(na.omit(animals$phylum))%in%unique(na.omit(rainbow_animal$Phylum)))]


###########   identify species level gaps  ########## 

# our data: trimmed_gbif
# rainbow list: rainbow_list 

length(unique(trimmed_gbif$species))  # unique species = 12,705


# get specific names 
gbif_specific_names <- sapply(str_split(trimmed_gbif$species, pattern = " "), function(x){x[2]})

# add the specifc names as a column to the trimmed_gbif 
trimmed_gbif <- mutate(trimmed_gbif, specific_names = gbif_specific_names)


# compare all unique species on gbif to the rainbow list 2985 out of 14,186
# 21% of the rainbow list is represented on gbif 
sum(unique(trimmed_gbif$species)%in%rainbow_list$Scientific.Name)





                 
######## Plotting Play ########
counts <- data.frame(c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                     c(length(unique(na.omit(trimmed_gbif$kingdom))), 
                       length(unique(na.omit(trimmed_gbif$phylum))),
                       length(unique(na.omit(trimmed_gbif$class))),
                       length(unique(na.omit(trimmed_gbif$order))),
                       length(unique(na.omit(trimmed_gbif$family))),
                       length(unique(na.omit(trimmed_gbif$genus))),
                       length(unique(na.omit(trimmed_gbif$species)))))
colnames(counts) <- c("Global Taxonomic Levels", "Counts")

t1 <- grid.table(counts)
                    

# of the observations with taxon rank at species level, how many unique species are there? 
gbif_species <- gbif %>% 
      filter(taxonRank=="SPECIES")


# there are about 12,000 unique species in the dataset 
length(unique(gbif_species$species))  # 12341



############ Plants ########
# subset the plantae kingdom 
plants <- subset(trimmed_gbif, trimmed_gbif$kingdom=="Plantae")

plant_counts <- data.frame(c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                           c(length(unique(na.omit(plants$phylum))),
                             length(unique(na.omit(plants$class))),
                             length(unique(na.omit(plants$order))),
                             length(unique(na.omit(plants$family))),
                             length(unique(na.omit(plants$genus))),
                             length(unique(na.omit(plants$species)))))
colnames(plant_counts) <- c("Plant Taxonomic Levels", "Counts")

# table of plant taxonomy breakdown 
grid.table(plant_counts)

# distribution of the plant phyla
plot_plant <- ggplot(as.data.frame(table(plants$phylum)), aes(x = Var1, y = Freq, color = Var1))+
      geom_col(show.legend = FALSE)+
      labs(x = "Plant Phyla", 
           y = "Frequency",
           title = "Distribution of the Plantae Phyla",
           subtitle = "Metro Vancouver")+
      theme_dark()+
      theme(plot.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))

# plot the distribution of plant phyla excluding: tracheophya/vascular plants 
plot_plant_no_vas <- ggplot(filter(as.data.frame(table(plants$phylum)), Var1!="Tracheophyta"), 
                     aes(x = Var1, y = Freq, color = Var1))+
      geom_col(show.legend = FALSE)+
      labs(x = "Plant Phyla", 
           y = "Frequency",
           title = "Distribution of the Plantae Phyla Excluding Vascular Plants",
           subtitle = "Metro Vancouver")+
      theme_dark()+
      theme(plot.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))

ggplot(as.data.frame(table(plants$year)), aes(x = Var1, y = Freq))+
      geom_point()

     
      
      
############ Animals ########
# subset the animal kingdom 
animals <- subset(trimmed_gbif, trimmed_gbif$kingdom=="Animalia")

animal_counts <- data.frame(c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                            c(length(unique(na.omit(animals$phylum))),
                              length(unique(na.omit(animals$class))),
                              length(unique(na.omit(animals$order))),
                              length(unique(na.omit(animals$family))),
                              length(unique(na.omit(animals$genus))),
                              length(unique(na.omit(animals$species)))))

colnames(animal_counts) <- c("Animal Taxonomic Levels", "Counts")

ggplot(as.data.frame(table(animals$phylum)), aes(x = Var1, y = Freq))+
      geom_col()+
      coord_flip()

plot_animal <- ggplot(filter(as.data.frame(table(animals$phylum)), Var1!="Chordata" & Var1!="Arthropoda"),
                      aes(x = Var1, y = Freq))+
      geom_col()+
      coord_flip()
              


############ Fungi ########
# subset the fungi kingdom 
fungi <- subset(trimmed_gbif, trimmed_gbif$kingdom=="Fungi")

fungi_counts <- data.frame(c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                           c(length(unique(na.omit(fungi$phylum))),
                             length(unique(na.omit(fungi$class))),
                             length(unique(na.omit(fungi$order))),
                             length(unique(na.omit(fungi$family))),
                             length(unique(na.omit(fungi$genus))),
                             length(unique(na.omit(fungi$species)))))

colnames(fungi_counts) <- c("Fungi Taxonomic Levels", "Counts")

plot_fungi <- ggplot(as.data.frame(table(fungi$phylum)), aes(x = Var1, y = Freq, color = Var1))+
      geom_col(show.legend = FALSE)+
      labs(x = "Fungi Phyla", 
           y = "Frequency",
           title = "Distribution of the Fungi Phyla",
           subtitle = "Metro Vancouver")+
      theme_dark()+
      theme(plot.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))

############ Chromista ########
# subset the fungi kingdom 
chromista <- subset(trimmed_gbif, trimmed_gbif$kingdom=="Chromista")

chromista_counts <- data.frame(c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                           c(length(unique(na.omit(chromista$phylum))),
                             length(unique(na.omit(chromista$class))),
                             length(unique(na.omit(chromista$order))),
                             length(unique(na.omit(chromista$family))),
                             length(unique(na.omit(chromista$genus))),
                             length(unique(na.omit(chromista$species)))))

colnames(chromista_counts) <- c("Chromista Taxonomic Levels", "Counts")

# plot the distribution of the chromist phyla 
plot_chromist <- ggplot(as.data.frame(table(chromista$phylum)), aes(x = Var1, y = Freq, color = Var1))+
      geom_col(show.legend = FALSE)+
      labs(x = "Chromist Phyla", 
           y = "Frequency",
           title = "Distribution of the Chromist Phyla",
           subtitle = "Metro Vancouver")+
      theme_dark()+
      theme(plot.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
      

# plot distribution of the chromist phyla excluding Ochrophyta and Myzozoa the most abundant phyla
plot_chromist_exclude_top <- ggplot(filter(as.data.frame(table(chromista$phylum)), 
                                           Var1!="Ochrophyta" & Var1!="Myzozoa"), 
                                    aes(x = Var1, y = Freq, color = Var1))+
      geom_col(show.legend = FALSE)+
      labs(x = "Chromist Phyla", 
           y = "Frequency",
           title = "Distribution of the Chromist Phyla Excluding Ochrophyta and Myzozoa",
           subtitle = "Metro Vancouver")+
      theme_dark()+
      theme(plot.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
      


############ Protozoa ########
# subset the protozoa kingdom 
protozoa <- subset(trimmed_gbif, trimmed_gbif$kingdom=="Protozoa")

protozoa_counts <- data.frame(c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                           c(length(unique(na.omit(protozoa$phylum))),
                             length(unique(na.omit(protozoa$class))),
                             length(unique(na.omit(protozoa$order))),
                             length(unique(na.omit(protozoa$family))),
                             length(unique(na.omit(protozoa$genus))),
                             length(unique(na.omit(protozoa$species)))))

colnames(protozoa_counts) <- c("Protozoa Taxonomic Levels", "Counts")

# distribution of the protist phyla 
ggplot(as.data.frame(table(protozoa$phylum)), aes(x = Var1, y = Freq))+
      geom_col()

############ Bacteria ########
# subset the bacteria kingdom 
bacteria <- subset(trimmed_gbif, trimmed_gbif$kingdom=="Bacteria")

bacteria_counts <- data.frame(c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                           c(length(unique(na.omit(bacteria$phylum))),
                             length(unique(na.omit(bacteria$class))),
                             length(unique(na.omit(bacteria$order))),
                             length(unique(na.omit(bacteria$family))),
                             length(unique(na.omit(bacteria$genus))),
                             length(unique(na.omit(bacteria$species)))))

colnames(bacteria_counts) <- c("Bacteria Taxonomic Levels", "Counts")

# plot the distribution of the bacteria phyla 
ggplot(as.data.frame(table(bacteria$phylum)), aes(x = Var1, y = Freq))+
      geom_col()+
      coord_flip()


# global taxonomic greakdown 
grid.table(counts)

# taxonomic breakdown of all kingdoms 
grid.arrange(
      tableGrob(plant_counts),
      tableGrob(animal_counts),
      tableGrob(fungi_counts),
      tableGrob(chromista_counts), 
      tableGrob(protozoa_counts), 
      tableGrob(bacteria_counts), nrow = 2)

# phyla frequencies for all kingdoms
phyla_freq <- rbind(as.data.frame(table(animals$phylum)),
                    as.data.frame(table(plants$phylum)),
                    as.data.frame(table(fungi$phylum)),
                    as.data.frame(table(chromista$phylum)),
                    as.data.frame(table(protozoa$phylum)),
                    as.data.frame(table(bacteria$phylum)))

# add a column indicating the kingdom of each observation
phyla_freq <- mutate(phyla_freq, kingdom = c(rep("animal", 17),
                                             rep("plant", 7), 
                                             rep("fungi", 4),
                                             rep("chromista", 10),
                                             rep("protozoa", 7),
                                             rep("bacteria", 26)))
colnames(phyla_freq) <- c("Phylum", "Freq", "Kingdom")


# note: there are way too many phyla in total to fit the distributions of all 
# the phyla onto a single faceted plot 

# faceting just the plants, animal and fungi
# doesn't look good. and is even misleading compared to plotting these things separately 
ggplot(filter(phyla_freq, Kingdom==c("animal", "plant", "fungi")),
       aes(x = Phylum, y = Freq))+
      geom_col()+
      facet_wrap(~Kingdom, scales = "free_x")+
      coord_flip()
     
             
#######  Diversity Metrics ######## 
library(vegan)
data("BCI")

# calculate species richness for the entire dataset : 7.281437
# this is probably not a very useful number 
length(unique(trimmed_gbif$species))/sqrt(length(trimmed_gbif$species))







    
      
                   



