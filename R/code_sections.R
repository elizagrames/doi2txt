# This script is a catch-all for all the fun things you can do with articles once sections have been subsetted
# My suggestions below all relate to the methods section since it is likely most useful for screening and coding articles

# Topic modeling ####


# Re-applying search terms ####


# Geocoding study sites ####

#get dataframe of countries mentioned in article section
get_countries_fr_section<-function(section){
library(tidyverse)
library(maps)
## Loading country data from package maps
data(world.cities)
###Remove punctuation
raw <- gsub("[[:punct:]\n]","",section)
# Split data at word boundaries
raw2 <- strsplit(raw, " ")
# Match on country in world.countries
CountryList_raw <- (lapply(raw2, function(x)x[which(toupper(x) %in% toupper(world.cities$country.etc))]))
plyr::ldply(CountryList_raw, rbind)
}
##Examples
##get a dataframe of all the countries from the methods section
#get_countries_fr_section(methods)
##get a dataframe of all the countries from the all sections
#get_countries_fr_section(articles)
## Current challenges - countries with two part names "South Africa", "South Korea", etc. get omitted because of the strsplit - do I need to do this??
                           

# Coding features from an ontology ####
