library(shiny)
library(tidyverse)
library(stringr)
library(rgdal)
library(jsonlite)
library(shinyBS)
library(RPostgres)
library(processx)
library(httr)
library(dbplyr)
library(rgeos)

# Read in the data 
dfsp <- read.csv("Taxonomy_Freq.csv", stringsAsFactors = F)
#colnames(dfsp) <- c("simplified_names", "common", "redList", "kingdom", "phylum", "class", "order","family", "genus","species","freq", "bc_list", "SARA", "regional_dist", "habitat_subtype", "Endemic", "Pollinator_binary", "SARA_binary", "sara_designations", "BC_Red_binary", "BC_Blue_binary", "BC_Endemic_binary", "IUCN_binary")

db = dbConnect(
  Postgres(), 
  user = 'shiny',
  password = 'p457e97104c822a7bd26d8e2dbd80cd3b8ae139685e8e8ac57cd7a5dc464456bd',
  dbname = 'd6n9p4o7gello',
  host = 'ec2-52-44-120-183.compute-1.amazonaws.com',
  port = 5432,
  sslmode = 'require'
)

config <- run("heroku", c("config:get", "DATABASE_URL", "-a", "dssgbiodiv"))

#config = run("heroku", c("config:get", "DATABASE_URL", "-a", "postgresql-perpendicular-84436"))

# Record which columns in dfsp should be treated as taxonomies and which should be treated as custom tags
tax_columns = which(colnames(dfsp) %in% c("kingdom","phylum","order","class","family","genus","species"))
tax_list = colnames(dfsp)[tax_columns]
names(tax_list) = str_to_title(tax_list)
tag_columns = grep("*_binary", colnames(dfsp))
tag_list = colnames(dfsp)[tag_columns]
names(tag_list) = sub("*_binary", "", colnames(dfsp)[tag_columns])
names(tag_list) = sub("_", " ", names(tag_list))

# create a dataframe containing total num of observations for each year 
combined_norm = readRDS("norms_by_year.rds")

##~~ FUNCTIONS ~~##
# Function for adding 0-value rows to aggregate tally dataframes to fill out the years between the first and last years
# Note: "data" must have exactly columns "member", "year", and "n"
add_zeros1 = function(data) {
  if (class(data)[1] != "data.frame") {data = as.data.frame(data)}
  for (mem in unique(data$member)) {
    if(nrow(data[which(data$member==mem),]) > 1) {
      min = min(data$year[which(data$member == mem)], na.rm = T)
      max = max(data$year[which(data$member == mem)], na.rm = T)
      for (yea in (min+1):(max-1)) {
        if (!(yea %in% data$year[which(data$member == mem)])) {
          data = rbind(data, data.frame(member = mem, year = as.integer(yea), n = as.integer(0), stringsAsFactors = F))
        }
      }
    }
  }
  return(data)
}

# For custom tag data, aggregation is done on a column-by-column (tag-by-tag) basis, necessitating a diferent function that accepts the maximum year range of the original data so that it knows to add zeros beyond the range of an individual tag (on the plus side, we don't need to worry about single-data-point cases)
add_zeros2 = function(data, min, max) {
  missing_years = setdiff(min:max, data$year)
  if (length(missing_years) != 0) {
    x = cbind(data.frame(setdiff(min:max, data$year), 0))
    colnames(x) = c("year", "n")
    return(rbind(data, x))
  }
  else {return(data)}
}

# Function for removing decimal places for the sake of labels
no_dec = function(x) {sprintf("%.0f", x)}