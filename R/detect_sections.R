# This script is for functions used in detecting sections/subsections of a paper
# Functions in this script should assume that an article exists as a character vector where each line represents one paragraph or header
# Lines may also contain things like figure captions, tables, etc.
# The main thing to keep in mind is that sentences in the same paragraph are one line

# Detecting section headers ####

#' Detect section headers in plain text journal articles
#' @description Given a plain text journal article and a named section header, detects which line is most likely to be the section header. For example, section="methods" will return the line containing a header such as "Materials and Methods".
#' @param section A string of length 1 naming the section to detect; options are introduction, methods, results, discussion, and references.
#' @param text A character vector containing the plain text of a journal article where each line represents one paragraph separated by line breaks.
#' @return An integer containing the line number of the text that is most likely the start of the section.
find_section <- function(section, text) {
  if(class(text)!="character"){
    warning("Please provide a valid character vector of text.")
    return(NA)
  }else{
    lookup <- unlist(doi2txt::sections[which(names(doi2txt::sections) == section)])

    # figure out which lines match to the terms and how many characters they differ by
    candidates <- data.frame(lapply(lookup, function(x) {
      z <- grep(x, text, ignore.case = TRUE)
      rbind(z, nchar(trimws(tm::removeNumbers(tm::removePunctuation(text[z])))) - nchar(x))
    }))

    if(length(candidates)>0){
      # extract only the best match for each term in the lookup vector
      if(section=="abstract"){
        best_guesses <- candidates[1,1] # always take the first instance for abstracts
      }else{
        best_guesses <- candidates[1, which(candidates[2,]==min(candidates[2,]))]
      }

      if(length(best_guesses)>1){
        header <- best_guesses[length(best_guesses)]
      }else{
        header <- best_guesses
      }
      if (length(header) == 0) {
        header <- NA
      }
      # check for identical headers
    }else{
      header <- NA
    }

    # return the line number of the line that has the closest nchar to the lookup vector

    return(as.numeric(header))
  }

}

#' Detect all major section headers in plain text journal articles
#' @description Finds the lines in a plain text scientific journal article that correspond to the start of the introduction, methods, results, discussion, and references.
#' @param text A character vector containing a scientific journal article in plain text format where each line represents one paragraph, section header, or other type of standalone text (e.g. a figure caption).
#' @return A numeric vector of length 5 indicating the lines within the text that are the section headers for the introduction, methods, results, discussion, and literature cited sections, respectively.
detect_sections <- function(text) {
  starts <- NA
  starts <- try(unlist(lapply(names(doi2txt::sections), function(x) {
    doi2txt::find_section(x, text)
  })))
  names(starts) <- names(doi2txt::sections)
  return(starts)
}


remove_junk <- function(text, min_char=50){
    actual_content <- which(nchar(text)>min_char & !grepl("cookie", text))
    startpoint <- min(actual_content[1]-1, actual_content)
    endpoint <- max(actual_content[length(actual_content)]+1, actual_content)

    text[startpoint:endpoint]
}


# Detect tables and figures ####

# Tables and figures could appear embedded in other sections
# So we may need to extract them from within, for example, a results section so they are treated separately

# It would also be good to detect reference lists and convert them to useful bib or ris files
# Though that is probably a problem for a later day and not part of the MVP



# Functions to subset out the sections once they are detected ####

extract_section <- function(text, section, max_lines=10, clean=TRUE, min_words=10, forcestart=FALSE) {
  endline <- NA
if(class(text)!="character"){
  warning("Please provide a valid character vector of text.")
  return(NA)
}else{
  if(clean){
    text <- doi2txt::remove_junk(text)
  }
  headers <- doi2txt::detect_sections(text)

  # check that things go in the right order
  tmp <- headers[!is.na(headers)]
  if(length(tmp)>1){
    for(i in 2:length(tmp)){
      if(any(tmp[i] <= tmp[1:(i-1)])){
        headers[names(tmp[i])] <- NA
      }
    }
  }

  startpoint <- headers[which(names(headers)==section)]
  endpoint <- headers[which(names(headers)==section)+1]-1

  if(is.na(startpoint)){
    if(!forcestart){
      warning(paste("Unable to identify start of ", section, ", returning NA. Consider using forcestart=TRUE if extracting abstracts.", sep=""))
    }else{
      startpoint <- 1
    }
  }


  if(is.na(endpoint)){
    endpoint <- startpoint+max_lines
    warning(paste(
      "Unable to identify the end of ", section, ", returning ", max_lines, " lines following the start of ", section, ".", sep = ""
    ))
  }

  if(any(is.na(startpoint), is.na(endpoint))){
    return(NA)
    }else{
      return(text[startpoint:endpoint])
             }

}
  # removes a bunch of random lines at the start and end of a document that are less than 50 characters, most of which are menu items
  # otherwise, the "start" of each section is consecutive lines in a menu
}
