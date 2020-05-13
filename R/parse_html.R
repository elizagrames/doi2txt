# This script is for functions to parse and clean html once it has already been downloaded
# Functions in this script should assume that an html journal article has been successfully scraped
# Remember to document/annotate your functions!

# Parsing functions ####

#' Convert html to plain text
#' @description Removes html from a downloaded article by calling the htm2txt package.
#' @param html A large string of length 1 containing the html for a journal article
#' @return A character vector containing the plain text version of the input html document with paragraphs in separate lines.
parse_html <- function(html = NULL, url = NULL) {
  if (is.null(html) & is.null(url)) {
    stop(print("Either an html text file or a url must be supplied."))
  }
  try(if (is.null(html)) {
    x <- htm2txt::gettxt(url)
  } else{
    if(length(html)>1){
      html <- paste(html, collapse=" ")
    }
    x <- htm2txt::htm2txt(html)
  })

  if (class(x) != "character") {
    site <- NA
  } else{
    site <- doi2txt::clean_html(x)
  }
  return(site)
}

# internal function to remove junk lines that appear when converting html to text
clean_html <- function(x) {
  x <- doi2txt::split_lines(x)
  if (any(x == "")) {
    x <- x[-which(x == "")] # remove blank lines
  }
}

# internal function for splitting by new line marks
split_lines <- function(x) {
  x <- strsplit(x, "\n")[[1]]
}

# makes dois into a url
# set up with lapply so it can be a bunch of dois, or just one
get_url <- function(doi) {
    paste("https://doi.org/", doi, sep = "")
}

