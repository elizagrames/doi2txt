# This script is for functions used in detecting sections/subsections of a paper
# Functions in this script should assume that an article exists as a character vector where each line represents one paragraph or header
# Lines may also contain things like figure captions, tables, etc.
# The main thing to keep in mind is that sentences in the same paragraph are one line

# Detecting section headers ####


# I will convert this to .rda later, but for now, here is a minimal lookup table for section headers

sections <- list(
  introduction = c("introduction", "background"),
  methods = c(
    "methods",
    "methodology",
    "materials and methods",
    "material and methods"
  ),
  results = c("results", "results and conclusions"),
  discussion = c("discussion", "conclusions"),
  references = c("literature cited", "works cited", "references", "citations")
)

# some papers do not have a section header for the intro

#' Detect section headers in plain text journal articles
#' @description Given a plain text journal article and a named section header, detects which line is most likely to be the section header. For example, section="methods" will return the line containing a header such as "Materials and Methods".
#' @param section A string of length 1 naming the section to detect; options are introduction, methods, results, discussion, and references.
#' @param text A character vector containing the plain text of a journal article where each line represents one paragraph separated by line breaks.
#' @return An integer containing the line number of the text that is most likely the start of the section.
find_section <- function(section, text) {
  lookup <- unlist(sections[which(names(sections) == section)])

  # figure out which lines match to the terms and how many characters they differ by
  candidates <- lapply(lookup, function(x) {
    z <- grep(x, text, ignore.case = TRUE)
    rbind(z, nchar(text[z]) - nchar(x))
  })

  # extract only the best match for each term in the lookup vector
    best_guesses <- data.frame(lapply(candidates, function(x){
      x[,which.min(x[2,])]
    }))

    # return the line number of the line that has the closest nchar to the lookup vector
    header <- best_guesses[1,which.min(best_guesses[2,])]
    if(length(header)==0){
      header <- NA
    }
    return(header)
}

#' Detect all major section headers in plain text journal articles
#' @description Finds the lines in a plain text scientific journal article that correspond to the start of the introduction, methods, results, discussion, and references.
#' @param text A character vector containing a scientific journal article in plain text format where each line represents one paragraph, section header, or other type of standalone text (e.g. a figure caption).
#' @return A numeric vector of length 5 indicating the lines within the text that are the section headers for the introduction, methods, results, discussion, and literature cited sections, respectively.
detect_sections <- function(text){
  all_sections <- unlist(lapply(names(sections), function(x){
    doi2txt::find_section(x, site)
  }))
}


# Detect tables and figures ####

# Tables and figures could appear embedded in other sections
# So we may need to extract them from within, for example, a results section so they are treated separately

# It would also be good to detect reference lists and convert them to useful bib or ris files
# Though that is probably a problem for a later day and not part of the MVP



# Functions to subset out the sections once they are detected ####

