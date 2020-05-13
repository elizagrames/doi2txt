#' Get a list of dastardly dois to cause problems in the doistext functions
#' @description Using datacite website to extract dois that link to scientific publications (can we extend to reports?)
#' @param limit A numerical value representing how many dois to return. This number will be larger than those returned because the function selects only the dois that lead to scientific articles
#' @param type A string of length 1 indicating the type of document from which to return dois
#' @return A character vector of dois
#' @example get_dastardly_dois(limit=10)
get_dastardly_dois <- function(limit, type="ScholarlyArticle") {

dois_list=rdatacite::dc_dois(
  limit = limit, random=TRUE)

pub_dois<-dois_list$data$attributes

pub_dois<-pub_dois[pub_dois$types$schemaOrg==type,]

return(pub_dois$doi)

}
