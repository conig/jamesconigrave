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
