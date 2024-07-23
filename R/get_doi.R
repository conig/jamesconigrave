#' get_doi
#'
#' Get a doi for a publication
#' @param title title of article
#' @param journal journal of article
#' @param author author name

get_doi <- function(title, journal, author, include_url = FALSE){

  qry <- rcrossref::cr_works(
    flq = list(
      "query.container-title" = journal,
      "query.author" = author,
      "query.bibliographic" = title),
    limit = 5)$data

  if(nrow(qry) == 0){
    qry <- rcrossref::cr_works(
      flq = list(
        "query.author" = author,
        "query.bibliographic" = title),
      limit = 5)$data
  }

  qry <- qry[!grepl("supp",qry$doi),][which.max(as.numeric(qry$score)),]

  if(nrow(qry) == 0) return(NA)
  str_match <- agrepl(title, qry$title)
  
  if(!is.na(str_match))
    if(!str_match) return(NA)

  if(include_url){
    # If empty, leave it empty
    if(qry$doi == "") return(qry$doi)
    return(paste0("https://doi.org/",qry$doi))
  }

  qry$doi

}
