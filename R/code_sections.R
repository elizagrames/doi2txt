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
all_countries <- str_c(unique(world.cities$country.etc), collapse = "|")
###Remove punctuation
raw <- gsub("[[:punct:]\n]","",section)

CountryList_raw<-sapply(str_extract_all(raw, all_countries), toString)
str(CountryList_raw)

as_tibble(CountryList_raw)
}
# #Get countries from methods section
# get_countries_fr_section(methods)
# #Get countried from articles
# get_countries_fr_section(articles)
# 
# #Challenges - What should be the output from this - at the moment it is a chr value in a tibble
# #e.g. "Brazil, Brazil, Brazil, Brazil, Brazil, Venezuela, Venezuela, Colombia, Guyana, Brazil, Guyana" 
# #I want to find a way to group countries together and then count the number of times each is recorded            

#get dataframe of cities mentioned in article section

get_cities_fr_section<-function(section){
  library(tidyverse)
  library(maps)
  ## Loading country data from package maps
  data(world.cities)
  all_cities <- str_c(unique(world.cities$name), collapse = "|")
  ###Remove punctuation
  raw <- gsub("[[:punct:]\n]","",section)
  
  CityList_raw<-sapply(str_extract_all(raw, all_cities), toString)
  
  as_tibble(CityList_raw)
}
# #Get cities from methods section
# get_cities_fr_section(methods)
# #Get cities from articles
# get_cities_fr_section(articles)
# 
# #Challenges - As above - how best to output this. For geocode we probably need a unique list of cities in each row. Also it brings in lots of cities that are just nonsense (in the paper not in the real world)

# Coding features from an ontology ####
