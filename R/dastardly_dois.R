# This script is for a function used to get a dastardly list of dois to provide to doi2text functions

# Dastardly dois ####

#' Get a list of dastardly dois to cause problems in the doistext functions
#' @description Using datacite website to extract dois that link to scientific publications (can we extend to reports?)
#' @param limit A numerical value representing how many dois to return. This number will be larger than those returned because the function selects only the dois that lead to scientific articles
#' @return A list of dois
#data not all publications but might be useful in future (and for Living Norway)
#devtools::install_github("ropensci/rdatacite")

get_dastardly_dois <- function(limit) {
library('rdatacite')
library(tidyverse)

dois_list=dc_dois(#query = "birds",
  #sample_size = 1000,
  limit = limit, random=TRUE)
pub_dois<-dois_list$data$attributes

pub_dois<-pub_dois %>%
  filter(pub_dois$types$schemaOrg=="ScholarlyArticle")

pub_dois<-pub_dois %>% select(doi)

pub_dois<-as.list(pub_dois$doi)
}

#some_dois<-get_dastardly_dois(limit=1000)

#articles <- lapply(some_dois, doi2html)
