# This script is for functions that scrape/read html as the first part of the doi2txt workflow
# Please document/annotate new functions as you write them

# wrapper function to combine all subfunctions
read_html <- function(doi=NULL, title=NULL, year=NULL, author=NULL){

}


# Functions for scraping html from a doi ####

doi2html <- function(doi=NULL, url=NULL){
  if(is.null(doi) & is.null(url)){
    stop(print("Either a doi or a url must be supplied."))
  }
  if(is.null(url)){
    url <- get_url(doi)
  }
  parse_html(url=url)
}


# Functions for looking up doi or scraping html from bibliographic data ####

bib2html <- function(title=NULL, year=NULL, author=NULL){

}

search_lens <- function(title){

}

# Subfunctions that address various common scraping issues encountered ####
