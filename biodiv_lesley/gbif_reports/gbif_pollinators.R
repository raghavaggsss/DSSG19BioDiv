library(tidyverse)
library(mapr)
library(rgbif)

# explore the arthropods 

# filter out the observations that have NA, unknown placement, viruses and archaea for kingdom
trimmed_gbif <- filter(gbif, 
                       kingdom!="incertae sedis" & kingdom!="NA" & kingdom!="Archaea" & kingdom!="Viruses")

# subset the arthropods kingdom 
arthropods <- subset(trimmed_gbif, trimmed_gbif$phylum=="Arthropoda")

# note: arthropods are a phylum within the kingdom animalia 

# what are arthropods? 
# they are an invertebrate animal, having an exoskeleton, segemented body, paired joined appendages 
# include: insects, arachnids, crusteaceans etc. 

# how many unique classes of arthropods are there? 
length(unique(na.omit(arthropods$class)))

# what are the class of arthropods? 
unique(arthropods$class)

# what pollinators to explore? 

# insect pollinators 
# include: 

#######   - honey bees    ###### 
# - genus apis 


# - bumblebees
# - genus Bombus


# - pollen wasps family: Masarinae

# - ants family: Formicidae

# - flies (bee fies) family: Bombyliidae

# hover flies: family Syrphidae

# mosquitoes (a type of fly)

# the Order: lepidopterans
# - butterflies families: 
# Hesperiidae, 
# Lycaenidae
# Nymphalidae
# Papilionidae
# Riodinidae
# Pieridae


# the Order: lepidopterans
# - moths 


# - known families of pollinating beetles 
# "Buprestidae" |
#"Cantharidae" |
#"Carambycidae" |
#"Cleridae" |
#"Dermestidae" |
#"Lycidae" |
#"Melyridae" |
#"Mordellidae" |
#"Nitidulidae" |
#"Scarabeidae"

# are there any Apis/honey bees in the dataset? 
"Apis"%in%arthropods$genus

# make a dataframe of the known pollinator families 
# info from wikipedia 
pollinators <- arthropods %>% 
      filter(genus=="Apis" | genus=="Bombus" | 
                   family=="Masarinae" | 
                   family=="Formicidae" | 
                   family=="Bombyliidae" | 
                   family=="Syrphidae" |
                   order=="Lepidoptera" |
                   family=="Buprestidae" |
                   family=="Cantharidae" |
                   family=="Carambycidae" |
                   family=="Cleridae" |
                   family=="Dermestidae" |
                   family=="Lycidae" |
                   family=="Melyridae" |
                   family=="Mordellidae" |
                   family=="Nitidulidae" |
                   family=="Scarabeidae")

pollinators <- rename(pollinators, "latitude" = "decimalLatitude")
pollinators <- rename(pollinators, "longitude" = "decimalLongitude")
pollinators <- mutate(pollinators, name = species)

# how many unique data sources for the pollinators? 
length(unique(pollinators$datasetKey))

# how many unique pollinator families are in the dataset? 
length(unique(na.omit(pollinators$family)))

# how many unique species? 
length(unique(na.omit(pollinators$species)))

# plot the pollinator families
ggplot(as.data.frame(table(na.omit(pollinators$family))), aes(x = Var1, y = Freq, color =Var1))+
      geom_col(show.legend = F)+
      labs(x = "Pollinator Families",
           y = "Number of Observations",
           title = "Pollinator Families Observed in MetroVancouver")+
      coord_flip()


# make a dataframe with only the first 5 families that are observed the most 
trimmed_poll <- filter(as.data.frame(table(pollinators$family)), Freq>=290)

# add the common names for each family to the dataframe
trimmed_poll <- trimmed_poll %>% mutate(group = c("Bumble & Honey Bee", 
                                 "Geometer Moth", 
                                 "Owlet Moth", 
                                 "Brush-footed Butterfly",
                                 "Leafroller Moth"))

# plot 5 families with largest number observations 
ggplot(trimmed_poll, aes(x = group, y = Freq, color =group))+
      geom_col(show.legend = F)+
      labs(x = "Pollinator Families",
           y = "Number of Observations",
           title = "Pollinator Families with Largest Number of Observations", 
           subtitle = "MetroVancouver")+
      coord_flip()


ggplot(data = pollinators, aes(x = longitude, y = latitude))+
      geom_polygon()





# how do the number of pollinator observations change over time? 
# a plot in order of year
filter(pollinators, year>=1990) %>% select(year) %>% table() %>% as.data.frame() %>% 
      ggplot(aes(x = ., y = Freq, color = .))+
      geom_col(show.legend = F)+
      coord_flip()



# a plot with the years ordered by number of observations 
# note: this plot doesn't make a lot of conceptual sense 
filter(pollinators, year>=1990) %>% select(year) %>% table() %>% as.data.frame() %>% 
      mutate(name=fct_reorder(., Freq)) %>% 
      ggplot(aes(x = name, y = Freq))+
      geom_col()+
      coord_flip()




# Vertebrate pollinators 
# - bats 
# - birds 
# --- hummingbirds 
# ---honeyeaters
# -- sunbirds

# note: mapr library allows the plotting of ggplot maps using map_ggplot 

