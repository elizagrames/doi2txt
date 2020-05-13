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
  lookup <- unlist(sections[which(names(sections) == section)])

  # figure out which lines match to the terms and how many characters they differ by
  candidates <- data.frame(lapply(lookup, function(x) {
    z <- grep(x, text, ignore.case = TRUE)
    rbind(z, nchar(trimws(tm::removeNumbers(tm::removePunctuation(text[z])))) - nchar(x))
  }))

if(length(candidates)>0){
  # extract only the best match for each term in the lookup vector
  best_guesses <- candidates[1, which(candidates[2,]==min(candidates[2,]))]

  if(length(best_guesses)>1){
    header <- best_guesses[length(best_guesses)]
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

#' Detect all major section headers in plain text journal articles
#' @description Finds the lines in a plain text scientific journal article that correspond to the start of the introduction, methods, results, discussion, and references.
#' @param text A character vector containing a scientific journal article in plain text format where each line represents one paragraph, section header, or other type of standalone text (e.g. a figure caption).
#' @return A numeric vector of length 5 indicating the lines within the text that are the section headers for the introduction, methods, results, discussion, and literature cited sections, respectively.
detect_sections <- function(text) {
  starts <- try(unlist(lapply(names(sections), function(x) {
    doi2txt::find_section(x, text)
  })))
  names(starts) <- names(sections)
  return(starts)
}


# Detect tables and figures ####

# Tables and figures could appear embedded in other sections
# So we may need to extract them from within, for example, a results section so they are treated separately

# It would also be good to detect reference lists and convert them to useful bib or ris files
# Though that is probably a problem for a later day and not part of the MVP



# Functions to subset out the sections once they are detected ####

extract_section <- function(text, section, max_lines=10) {
  endline <- NA
  if(section=="abstract"){
    output <- doi2txt::get_abstract(text)
  }else{

  startline <- doi2txt::find_section(section, text)
  if (!is.na(startline)) {
    next_section <-
      names(sections)[(which(names(doi2txt::sections) == section) + 1)]

    # gonna need to make this more flexible for sections that were not detected in a document
    # e.g. where do you cut off the methods section if you could not find results?
    if (next_section %in% names(sections)) {
      endline <- doi2txt::find_section(next_section, text) - 1
    }
    if(section=="references"){
      endline <- length(text)
    }
    if (is.na(endline)) {
      endline <- startline+max_lines
      print(paste(
        "Unable to identify the end of", section, "returning", max_lines, "lines following the start of", section, sep = " "
      ))
    }
    output <- text[startline:endline]

    if(section=="references"){
# remove lines that don't have a 1 or 2 since those will appear in basically any reference publication year
      # some things still sneak in, but this should deal with most issues
      output <- output[sort(unique(c(grep(1, output), grep(2, output))))]
    }
  } else{
    output <- NA
  }
  }
  return(output)
}
