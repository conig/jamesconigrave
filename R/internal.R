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
                               limit = 5)$data
    qry <- qry[!grepl("supp",qry$doi),][1,]
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

#' altmetric
#'
#' Finds altmetric statistics
#' @param doi
#' @export altmetric

altmetric = function(doi){
  doi = gsub(r"(^(.*?)doi.org\/)","", doi)
  doi = gsub("^/","",doi)
  out <- suppressWarnings(readLines(glue::glue("https://api.altmetric.com/v1/doi/{doi}")))
  stats = jsonlite::fromJSON(out)

  data.frame(tweets = safe_get(stats$cited_by_tweeters_count),
             fb = safe_get(stats$cited_by_fbwalls_count),
             blogs = safe_get(stats$cited_by_feeds_count),
             score = safe_get(stats$score),
             media = safe_get(stats$cited_by_msm_count))
}

safe_get = function(x) ifelse(is.null(x), 0, x)

