#' format_authors
#'
#' Formats authors to be in the format FM Last
#' @param string a string

format_authors = function(string){
  authors = trimws(unlist(strsplit(string, ",")))
  authors = unlist(lapply(authors, format_author))
  paste(authors, collapse = ", ")

}

#' format_author
#'
#' format a single author
#' @param author Author name

format_author = function(author){

  words = trimws(unlist(strsplit(author, " ")))
  lastname = words[length(words)]
  first = words[!words %in% lastname]
  first = gsub("\\B[a-z]","",first, perl = TRUE) #select characters not at the start of a word
  first = paste(first, collapse = "")
  trimws(paste(first, lastname))

}
