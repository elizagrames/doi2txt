# This script is for functions to parse and clean html once it has already been downloaded
# Functions in this script should assume that an html journal article has been successfully scraped
# Remember to document/annotate your functions!

# Parsing functions ####

#' Convert html to plain text
#' @description Removes html from a downloaded article by calling the htm2txt package.
#' @param htm A large string of length 1 containing the html for a journal article
#' @return A character vector containing the plain text version of the input html document with paragraphs in separate lines.
parse_html <- function(htm) {
  x <- htm2txt::htm2txt(htm)
  doi2txt::clean_html(x)
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
