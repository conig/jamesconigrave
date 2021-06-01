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
    qry <- rcrossref::cr_works(
      flq = list(
        "query.container-title" = pubs$journal[i],
                 "query.author" = pubs$author_last[i],
                 "query.bibliographic" = pubs$title[i]),
      limit = 5)$data

    if(nrow(qry) == 0){
      qry <- rcrossref::cr_works(
        flq = list(
          "query.author" = pubs$author_last[i],
          "query.bibliographic" = pubs$title[i]),
        limit = 5)$data
    }

    qry <- qry[!grepl("supp",qry$doi),][which.max(qry$score),]
    sp$spin()
    # if (stringdist::stringdist(tolower(qry$title), tolower(pubs$title[i])) > 15) {
    #   return(NA)
    # }

    qry[,c("title","doi")]

  })
  sp$finish()
  results <- do.call(rbind, results)
  results
}

#' altmetric
#'
#' Finds altmetric statistics
#' @param doi a doi
#' @export altmetric

altmetric = function(doi){
  safe_get = function(x) ifelse(is.null(x), 0, x)

  doi = gsub("^(.*?)doi.org\\/","", doi)
  doi = gsub("^/","",doi)
  out <- tryCatch(suppressWarnings(readLines(glue::glue("https://api.altmetric.com/v1/doi/{doi}"))),
                  error = function(e){
                  data.frame(tweets = NA,
                             fb = NA,
                             blogs = NA,
                             score = NA,
                             media = NA)
                  })
  if("data.frame" %in% class(out)) return(out)

  stats = jsonlite::fromJSON(out)

  data.frame(tweets = safe_get(stats$cited_by_tweeters_count),
             fb = safe_get(stats$cited_by_fbwalls_count),
             blogs = safe_get(stats$cited_by_feeds_count),
             score = safe_get(stats$score),
             media = safe_get(stats$cited_by_msm_count))
}


