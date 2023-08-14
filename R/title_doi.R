#' title_to_doi
#'
#' Find a doi based on paper title
#' @param title character
#' @export

title_to_doi <- function(title = NULL) {
  if(is.null(title)){
    requireNamespace("clipr", quietly = TRUE)
    title <- clipr::read_clip()
    message("Searching for: ", title)
  }

  doi <- rcrossref::cr_works(query = title)$data$doi[1]
  message(doi)

  paste0("https://www.doi.org/",doi) |>
    browseURL()
}

#' aut_to_doi
#'
#' Find manuscript details based on author year
#' @param string string to search
#' @param n results to return
#' @export

aut_to_doi <- function(string = NULL, n = 3, qry = ""){

  if(is.null(string)){
    requireNamespace("clipr", quietly = TRUE)
    string <- clipr::read_clip()
    message("Searching for: ", string)
  }

  year <- stringr::str_extract(string, "[0-9]+")
  authors <- stringr::str_extract_all(string, "[A-z,&]+")[[1]] |>
    paste(collapse = " ")
  authors <- gsub(" \\band\\b",",", authors)
  authors <- gsub(" \\& ",", ", authors)
  authors <- gsub(",$","",authors)

  authors <- gsub("et\\.? al", "", authors) |>
    paste(collapse = ", ") |>
    trimws()

  res <- rcrossref::cr_works(query = qry, flq = list(query.author = authors, query.bibliographic = year))

  get_dat <- function(x){
    list(
      title = x$title,
      author = paste(unlist(x$author[[1]][1:2]), collapse = " "),
      year = x$published.online,
      doi = x$url
    )
  }

  if(nrow(res$data) < n) return(get_dat(res$data[1,]))

  lapply(1:n, function(x) get_dat(res$data[x,]))
}
