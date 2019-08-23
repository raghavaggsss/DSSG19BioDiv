library(tidyverse)
library(httr)
library(jsonlite)
library(rgdal)
library(rgeos)
library(sp)
library(geosphere)

## ~~ ATTACHING INFORMATION TO POLYGONS ~~##

# Adding in station-level variables (temperature and rainfall) and elevation
polys = readOGR(dsn = "C:/Users/Gabe/Desktop/DSSG19BioDiv/ShinyMap/MVSEI2014",
                layer = "MVSEI2014")
polys = spTransform(polys, CRS("+proj=longlat +datum=WGS84"))
newpolydata = read.csv("sei_elev.csv", stringsAsFactors = F)
if ("X" %in% colnames(newpolydata)) {newpolydata = newpolydata[,-1]}
polys@data = merge(polys@data, newpolydata, by = "SEI_PolyNb", all.x = T)

# Computing proximity to water
coast = readOGR(dsn = "C:/Users/Gabe/Desktop/DSSG19BioDiv/prediction/clipped coastline",
                layer = "250_CST_PY_polygon")
coast = spTransform(coast, CRS(proj4string(polys)))
coast = coast[c(33,50),] # Most of the polygons in this shapefile represent land (either islands or mainland)
freshw = readOGR(dsn = "C:/Users/Gabe/Desktop/DSSG19BioDiv/prediction/clipped RL",
                 layer = "FWRVRSPL_polygon")
freshw = spTransform(freshw, CRS(proj4string(polys)))
counter = 0
for (p in 1:length(polys)) {
  polys@data$Freshwater_Distance[p] = gDistance(polys[p,], freshw)
  polys@data$Saltwater_Distance[p] = gDistance(polys[p,], coast)
  # It should be noted that gDistance introduces error since it is meant for planar distances (i.e., flat maps), not angular coordinates (i.e., representing positions by their angle away from the equator and the Grenwich mean line, as coordinates on a sphere should be). I could find no function that computes distance between two polygons charted in angular coordinates, but the error introduced should hopefully be small since we are only using a very small area.
  if (p %in% round(seq(0,24759,24759/50))) { # Progress Counter
    counter = counter + 1
    print(paste(round(p/length(polys)*100),"%", sep = ""))
  }
}



## ~~ LOADING IN DATA ~~ ##

df = read.csv("gbif_map_poly.csv", stringsAsFactors = F)
rn = row.names(unique(df[,c("species","poly_index")])) # Collapse across the multiple sightings of the same species within a given polygon
df2 = df[rn,] # Only ~6,000 species (~60,000 observations) were spotted in SEI polygons
df_species = df2
for (sp in unique(df_species$species)) {
  if (length(df_species$poly_index[which(df_species$species==sp)]) < 10) {
    df_species = df_species[-which(df_species$species==sp),]
  }
} # Only ~536 species (~50,000 observations) were spotted in at least 10 polygons
rn = row.names(unique(df[,c("class","poly_index")]))
df_classes = df[rn,] # All 88 classes (5,674 observations) were spotted in SEI polygons
for (cl in unique(df_classes$class)) {
  if (length(df_classes$poly_index[which(df_classes$class==cl)]) < 20) {
    df_classes = df_classes[-which(df_classes$class==cl),]
  }
} # Restricting classes to having been seen in only 10 polygons leaves us with 36 classes - making the threshold 20 leaves us with 25 classes (5,406 observations)
df_species = df_species[,c("species","poly_index")]
df_classes = df_classes[,c("class","poly_index")]

# Ensure we are dealing with a character - it will be changed to a factor later
if (class(polys@data$SECl_1) == "factor") { 
  polys@data$SECl_1 = as.character(polys@data$SECl_1)
  polys@data$SECl_2 = as.character(polys@data$SECl_2)
  polys@data$SECl_3 = as.character(polys@data$SECl_3)
} 
# Collapse across multiple sub-classes of "Modified Ecosystem" (ME)
polys@data[which(polys@data$SE_ME_1 == "ME"),"SECl_1"] = "ME"
polys@data[which(polys@data$SE_ME_2 == "ME"),"SECl_2"] = "ME"
polys@data[which(polys@data$SE_ME_3 == "ME"),"SECl_3"] = "ME"
# Remove SEI polygons that contain no observations to prevent them from distorting the results
polys = polys[which(polys@data$SEI_PolyNb %in% df2$poly_index),]
# Create a list of the classes in order
classes = c("Alpine", "Estuarine", "Freshwater Lakes & Ponds", "Herbaceous", "Intertidal & Shallow Sub-Tidal", "Modified Ecosystems", "Mature Forest", "Old Forest", "Riparian", "Sparsely Vegetated", "Woodland", "Wetlands", "Small Young Forest Patches")
poly_df = polys@data[,c("SEI_PolyNb","SECl_1","SECl_2","SECl_3","SEDec_1","SEDec_2","SEDec_3","Freshwater_Distance","Saltwater_Distance","WSize_SE1_","WSize_SE2_","WSize_SE3_","ContextNo","ConditionN","T_mean_Climatology","Precip_Climatology","elevation")]
poly_df$Size = rowSums(poly_df[,c("WSize_SE1_","WSize_SE2_","WSize_SE3_")])
poly_df[,c("WSize_SE1_","WSize_SE2_","WSize_SE3_")] = NULL
colnames(poly_df)[which(colnames(poly_df) %in% c("ContextNo", "ConditionN", "T_mean_Climatology", "Precip_Climatology"))] = c("Context","Condition", "temp", "precipitation")
poly_df$SECl_1 = as.factor(poly_df$SECl_1) 
poly_df$SECl_2 = as.factor(poly_df$SECl_2) 
poly_df$SECl_3 = as.factor(poly_df$SECl_3) 
# Set XX first so that it is used as the baseline/intercept because we don't care about it
poly_df$SECl_1 = factor(poly_df$SECl_1,levels(poly_df$SECl_1)[c(13,1:12,14)])
poly_df$SECl_2 = factor(poly_df$SECl_2,levels(poly_df$SECl_2)[c(13,1:12,14)])
poly_df$SECl_3 = factor(poly_df$SECl_3,levels(poly_df$SECl_3)[c(13,1:12,14)])

# The above code is all that is needed if you are treating the polygons are categorical (i.e., this one is "alpine", this one is "estuarine"); the section below incorporates the fact that at least a third of polygons contain fractional ecosystems by transforming SEI class from a factor into 14 variables from 1-10, each point representing a decile.
# For unknown reasons, some polygons have second/third class entries that are the same as the first/second class entries - this for loop adds them together in that case.
for (r in 1:nrow(poly_df)) {
  if (!is.na(poly_df[r,"SECl_1"]) & !is.na(poly_df[r,"SECl_2"])) {
    if (poly_df[r,"SECl_1"] == poly_df[r,"SECl_2"]) {
      poly_df[r,"SECl_2"] = NA
      poly_df[r,"SEDec_1"] = poly_df[r,"SEDec_1"] + poly_df[r,"SEDec_2"]
    }
  }
  if (!is.na(poly_df[r,"SECl_1"]) & !is.na(poly_df[r,"SECl_3"])) {
    if (poly_df[r,"SECl_1"] == poly_df[r,"SECl_3"]) {
      poly_df[r,"SECl_3"] = NA
      poly_df[r,"SEDec_1"] = poly_df[r,"SEDec_1"] + poly_df[r,"SEDec_3"]
    }
  }
  if (!is.na(poly_df[r,"SECl_2"]) & !is.na(poly_df[r,"SECl_3"])) {
    if (poly_df[r,"SECl_2"] == poly_df[r,"SECl_3"]) {
      poly_df[r,"SECl_3"] = NA
      poly_df[r,"SEDec_2"] = poly_df[r,"SEDec_2"] + poly_df[r,"SEDec_3"]
    }
  }
}
for (cl in levels(poly_df$SECl_1)) {
  poly_df[,cl] = 0
  poly_df[which(poly_df$SECl_1==cl),cl] = poly_df[which(poly_df$SECl_1==cl),"SEDec_1"]
  poly_df[which(poly_df$SECl_2==cl),cl] = poly_df[which(poly_df$SECl_2==cl),"SEDec_2"]
  poly_df[which(poly_df$SECl_3==cl),cl] = poly_df[which(poly_df$SECl_3==cl),"SEDec_3"]
}
poly_df[,c("SECl_1","SECl_2","SECl_3","SEDec_1","SEDec_2","SEDec_3")] = NULL
# "table(rowSums(poly_df[,10:23]))" can be used to find whether the above code worked (i.e., whether every polygon has a sum total of ten deciles); the result should be that all have a combined rowSum of 10.


## ~~ COMPUTATION FUNCTIONS ~~##


# Convert log odds to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Define the logistic regression functions which the other functions depend on.
log_reg_factor = function(species) {
  # Assigns a variable called 'present' based on whether a species can be found in a given polygon
  foundin = unique(df_species$poly_index[which(df_species$species==species)])
  poly_df$present = F
  poly_df$present[which(poly_df$SEI_PolyNb %in% foundin)] = T
  return(glm(present ~ SECl_1 + Freshwater_Distance + Saltwater_Distance + Condition + Context + Size + temp + precipitation + elevation, data = poly_df, family = "binomial", contrasts = list(SECl_1 = "contr.sum")))
}
log_reg_deciles = function(unit) {
  # Determine which data set you're working with
  if (unit %in% unique(df$species)) {df = df_species
  }  else if (unit %in% unique(df$class)) {df = df_classes
  }  else {print("Please enter valid grouping")}
  # Assigns a variable called 'present' based on whether a species can be found in a given polygon
  foundin = unique(df$poly_index[which(df[,1]==unit)])
  poly_df$present = F
  poly_df$present[which(poly_df$SEI_PolyNb %in% foundin)] = T
  terrain = colSums(poly_df[which(poly_df$present==T),])[2:14]
  var_names = c(names(terrain[which(terrain!=0)]), "Freshwater_Distance", "Saltwater_Distance", "Condition", "Context", "Size", "temp", "precipitation", "elevation")
  f = as.formula(paste("present",
                       paste(var_names, collapse = " + " ),
                       sep = " ~ "))
  return(glm(f, data = poly_df, family = "binomial"))
}
# Important statistical note with the decile regression! Since by both necessity and design all deciles add to the same number (10), we would normally have the problem of multicollinearity - essentially, not all combinations of our 14 class variables are possible since you automatically know the value of the 14th variable if you know the first 13. A more intuitive way of demonstrating the problem is by considering that a parameter of .1 means that for every one decile increase of that class in a polygon, your log odds go up by one. But since the deciles need to add up to 10, a 1 decile increase in this class means another class has to go down 1 - so which class is going down? The parameters only makes sense if we assume that there are 11 deciles in a polygon (more accurately we assume that there can be infinite deciles in any polygon), which we know is false.
# To fix this, XX has been removed as a predictor, which essentially makes it the baseline. Thus, a parameter of .2 for MF means that you would expect a polygon which has one more decile of mature forest *and one less decile of XX* to have an increase of .2 in its log odds. You can also directly compare the predictors. If a polygon is being developed and has thus lost a decile of MF to modified ecosystems but XX is not involved at all, subtract the MF parameter from the ME parameter to get your change in log odds. See https://stats.stackexchange.com/questions/183601/interpreting-proportions-that-sum-to-one-as-independent-variables-in-linear-regr for a better explanation.

# Determine the odds of a given species being reported in each class (holding all non-SEI variables constant)
indv_class_probs = function(species) {
  logit = log_reg_deciles(species)
  # Derive the log-odds for each SEI (this requires adding the intercept to each of the non XX SEI classes)
  logcoef = as.data.frame(summary(logit)$coefficients)$Estimate
  logcoef = logcoef[2:14] + logcoef[1]
  # The following line will calculate the coefficient for the baseline factor level if you are using treatment contrasts; do not use otherwise: logcoef[2:length(logcoef)] = logcoef[2:length(logcoef)] + logcoef[1]
  # Derive probabilities of sightings for polygons of each class
  prob_list = logit2prob(logcoef)
  #nm = levels(poly_df$SECl_1)[1:13]
  sig = as.data.frame(summary(logit)$coefficients)$`Pr(>|z|)`[2:14]
  sig = sig < .05
  df = data.frame(SEI = classes, Probability = prob_list, Significance = sig)
  df = df[order(df$Probability, decreasing = T),]
  return(df)
}

# Create a dataframe storing the parameters for the logistic regression equations, with each row representing a different species and each column representing the parameters of a different predictor
equation_developer = function(grouping) {
  eq_df = data.frame(matrix(ncol = 44, nrow = 0))
  colnames(eq_df) = c("Intercept", colnames(poly_df)[2:22], "Intercept_sig",
                      paste(colnames(poly_df)[2:22], "_sig", sep = ""))
  counter = 1
  if (grouping == "species") {set = unique(df_species$species)
  }  else if (grouping == "class") {set = unique(df_classes$class)}
  set_len = length(set)
  for (unit in set) {
    logit = as.data.frame(summary(log_reg_deciles(unit))$coefficients)
    filled_cols = c(1, which(colnames(eq_df) %in% rownames(logit)))
    eq_df[counter,filled_cols] = logit$Estimate
    eq_df[counter,(filled_cols + 22)] = logit$`Pr(>|z|)`
    if (counter %in% round(seq(0,set_len,set_len/20))) {
      print(paste(round(counter/set_len*100),"%", sep = ""))
    }
    counter = counter + 1
  }
  rownames(eq_df) = set
  # The results are clearly extreme and inaccurate for certain members. Unfortunately I could find no way of testing whether or not a particular run produced an error; while it woul dbe ideal to discard just the equations that threw errors, we must make do by discarding any equation with a value above 8 (from an inspection of the equations, parameters do seem to be fairly bimodal - mostly >10 in the 'error' runs and mostly <1 in the rest)
  for (r in rownames(eq_df)) {
    if (any( abs(eq_df[r,2:ncol(eq_df)]) > 6, na.rm = T)) {eq_df = eq_df[!(row.names(eq_df) %in% r),]}
  } # Helpfulto note that equations with extreme intercepts are not excluded - extremely low intercepts are quite possible under normal conditions, and are simply a sign of a taxon being rare; it is extreme non-intercept parameters we are worried about.
  return(eq_df)
}

# Make predictions based on the stored parameters (see "equation_developer" above)
predict_odds = function(parameters, species, cls, fresh_prox, salt_prox) {
  # Firstly construct a series of zeros with a single one, representing the input values for the SEI class factor
  class_num = which(classes==cls)
  class_seq = c(rep(0,class_num-1),1,rep(0,length(classes)-class_num))
  # Creates a vector of these predictors, starting a "1" for the intercept
  preds = c(1, class_seq, fresh_prox, salt_prox)
  logodds = parameters[species,] %*% preds
  logodds = as.numeric(as.matrix(parameters[species,], nrow = 1) %*% matrix(preds))
  percchance = logit2prob(logodds)
  return(percchance)
}

# Identify the most species most commonly found in a given SEI class (irrespective of other polygon factors)
common_wildlife = function(class) {
  output = NULL
  for (sp in unique(df2$species)) {
    x = class_probs(sp)
    pr = x[x$SEI==class,"Probability"]
    output = rbind(output, cbind(sp, pr))
  }
  colnames(output) = c("Creature","Probability")
  output$Creature = as.character(output$Creature)
  output$Probability = as.character(output$Probability)
  output = output[order(output$Probability),]
  return(output)
}

species_equations = equation_developer("species")
class_equations = equation_developer("class")

# This is the code for replacing 'NA' parameters (i.e., that taxon was never seen in that SEI category) with the lowest SEI class parameter value, to ensure that the equation still gives predictions for such classes but the odds  are never better than for the least likely SEI class.
for (r in 1:nrow(species_equations)) {
  min = min(species_equations[r,2:14], na.rm = T)
  nas = which(is.na(species_equations[r,]))
  nas = nas[nas < 15]
  species_equations[r,nas] = min
}
for (r in 1:nrow(class_equations)) {
  min = min(class_equations[r,2:14], na.rm = T)
  nas = which(is.na(class_equations[r,]))
  nas = nas[nas < 15]
  class_equations[r,nas] = min
}

# This code factors in the significance of parameters by reducing any parameters that are not significant to zero
# NOTE the numbers 22, 23, and 44 below depend on the number of variables and must be updated if variables are added to or subtracted from the data
sig_mat = as.matrix(eq_df[,23:44])
sig_mat = sig_mat < .05
eq_df_sig = eq_df[,1:22] * sig_mat
rr = c()
for (r in 1:nrow(eq_df_sig)) {
  if (all(eq_df_sig[r,]==0)) {
    rr = c(rr,r)
  }
}
eq_df_sig2 = eq_df_sig[-rr,]

# Alternately, if you have no intention of using the signficance value, this code simply removes them
species_equations[,23:44] = NULL
class_equations[,23:44] = NULL

## ~~ EXPORT ~~##
saveRDS(polys, "complete polygons.rds")
poly_inputs = cbind(Intercept = 1, poly_df)
poly_inputs = poly_inputs[,c(2,1,12:24,3:10)]
write.csv(poly_inputs, "polygon inputs.csv", row.names = F)
write.csv(species_equations, "Species Parameters.csv")
write.csv(class_equations, "Class Parameters.csv")


# Example code for deriving ranked species occurances using both a normalized and unnormalized technique (with and without the intercept term)







#elv = get_elev_point(centroids)

#x = as.data.frame(centroids[1:10,])
#colnames(x) = c("longitude", "latitude")
#y = toJSON(x)

#base = "https://api.open-elevation.com/"
#endpoint = "api/v1/lookup"
#location = "41.161758,-8.583933"
#call1 = paste(base, endpoint, "?", "location", "=", location, sep="")

#get_elv = GET(call1)

#request = get(url = path,
#              query = list(
#                longitude = 10,
#                latitude = 10
#              ))
#response = content(request, as = "text", encoding = "UTF-8")
#eldf = fromJSON(response, flatten = TRUE) %>% data.frame()


# Example logistic regression:
# First with a factor:
exdf = data.frame(pass = c(T,F,F,F,T,F,F,F,F,F,T,T,T,T,T,T,T,T,F,T,T,T,T,T,F,F,F,T,T,T), school = c(rep("Atbury",10), rep("Beantown",10), rep("Ceasarean",10)))
exlog = glm(pass ~ school, data = exdf, family = "binomial")
summary(exlog)
# Then with dummy variables
exdf = data.frame(pass = c(T,F,F,F,T,F,F,F,F,F,T,T,T,T,T,T,T,T,F,T,T,T,T,T,F,F,F,T,T,T), Atbury = c(rep(T,10),rep(F,20)), Beantown = c(rep(F,10),rep(T,10),rep(F,10)), Ceasarean = c(rep(F,20),rep(T,10)))
exlog = glm(pass ~ Atbury + Beantown + Ceasarean, data = exdf, family = "binomial")
summary(exlog)


