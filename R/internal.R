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

#' get_doi
#'
#' Finds dois for scholar pubs
#' @param pubs scholar::get_publications output
#' @export

get_doi = function(pubs) {
  sp <- cli::make_spinner("star", template = "Searching for DOIs {spin}")
  results = lapply(seq_along(pubs[, 1]), function(i)
  {
    qry <- rcrossref::cr_works(query = paste(pubs$title[i], pubs$journal[i], sep = ","),
                               limit = 1)$data[1,]
    sp$spin()
    if (stringdist::stringdist(tolower(qry$title), tolower(pubs$title[i])) > 2) {
      return(NA)
    }

    qry$doi

  })
  sp$finish()
  results <- unlist(results)
  results
}

