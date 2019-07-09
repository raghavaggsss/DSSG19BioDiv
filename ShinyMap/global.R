library(shiny)
library(leaflet)
library(dplyr)
library(tidyverse)

df <- readRDS("./mun_years.rds")

df = df[1:1000,]
#no_year_info = function(x) {
#  lis = c()
#  for (sp in unique(x$species)) {
#    if (sum(!is.na(x$year[which(x$species == sp)]) == 0)) {lis = c(lis, sp)}
#  }
#  return(lis)
#}
df = df[!is.na(df$year),]

df_yearagg = plyr::count(df, c('year'))
