#' scopus_FWCI
#'
#' Scopus FWCI past 5 years
#' @export

scopus_FWCI <- function(author_id = 57207914919){
  apikey <- Sys.getenv("Elsevier_API")

 RequestURL <- "https://api.elsevier.com/analytics/scival/author/metrics?metricTypes=FieldWeightedCitationImpact&authors={author_id}&yearRange=5yrsAndCurrentAndFuture&includeSelfCitations=true&byYear=false&includedDocs=AllPublicationTypes&journalImpactType=CiteScore&showAsFieldWeighted=false&indexType=hIndex&apiKey={apikey}
" |> glue::glue()

  response <- httr::GET(RequestURL)

  result <- jsonlite::fromJSON(rawToChar(response$content))
  result$results$metrics[[1]]$value
  }

#' scopus_citations
#'
#' Scopus citations past 5 years
#' @export

scopus_citations <- function(author_id = 57207914919){
  apikey <- Sys.getenv("Elsevier_API")

 RequestURL <- "https://api.elsevier.com/analytics/scival/author/metrics?metricTypes=CitationCount&authors={author_id}&yearRange=5yrsAndCurrent&includeSelfCitations=true&byYear=false&includedDocs=AllPublicationTypes&journalImpactType=CiteScore&showAsFieldWeighted=false&indexType=hIndex&apiKey={apikey}
" |> glue::glue()

  response <- httr::GET(RequestURL)

  result <- jsonlite::fromJSON(rawToChar(response$content))
  result$results$metrics[[1]]$value
}

