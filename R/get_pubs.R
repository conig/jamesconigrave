

#' get_pubs
#'
#'Get and format publications
#' @param id google scholar id
#' @param n how many pubs to retrieve?
#' @param exclude_journals vector of journal names to filter out
#' @param cache_doi cache doi?
#' @param cache_author cache author?

publications <- function(id = "m0d4TKcAAAAJ",
                        n = Inf,
                        journal_exclude = c("ACU Research Bank"),
                        cache_doi = TRUE,
                        cache_author = TRUE) {
  pubs = scholar::get_publications(id, flush = TRUE)

  # remove pubs without a year
  pubs <- pubs[!is.na(pubs$year),]

  pubs = pubs[!pubs$journal %in% journal_exclude,]
  pubs$author_last = gsub("^\\w{1,} *", "", pubs$author)

  doi_path = paste0(system.file("", package = "conig"),
                      "/",
                      id,
                      "_doi.rds")
  author_path = paste0(system.file("", package = "conig"),
                    "/",
                    id,
                    "_authors.rds")


  # merge in DOI information

  if (file.exists(doi_path)) {
    dois <- readRDS(doi_path)
    names(dois) <- c("pubid", "doi","title2")
    pubs <- merge(pubs, dois, all.x = TRUE)
    pubs$title[grepl("…",pubs$title)] <- pubs$title2[grepl("…",pubs$title)]
    pubs$title2 <- NULL
  } else{
    pubs$doi = NA
  }

  pubs[is.na(pubs$doi), ]$doi <- with(pubs[is.na(pubs$doi), ],
                       mapply(get_doi, title, journal, author_last))

  if (cache_doi) {
    to_cache <- pubs[!is.na(pubs$doi), c("pubid", "doi", "title")]

    saveRDS(to_cache, file = doi_path)
  }

  pubs$doi = ifelse(is.na(pubs$doi), "",  paste0("https://doi.org/",pubs$doi))

  # add altmetric --------------------------------------

  pubs <- cbind(pubs, do.call(rbind, lapply(pubs$doi, altmetric)))

  pubs$journal = ifelse(nchar(pubs$journal) > 0,
                        paste0("*", pubs$journal, "*"),
                        pubs$journal)
  pubs$number = ifelse(nchar(pubs$number) > 0,
                       paste0(" *", pubs$number, "*"),
                       pubs$number)

  extra_info = function(row) {
    cid = pubs[row,"cid"]
    cite_link = glue::glue("https://scholar.google.com/scholar?oi=bibs&hl=en&cites={cid}")
    info <- pubs[row, c("cites", "tweets", "media")]
    info[is.na(info)] = 0
    names(info) = c("citation", "tweet", "mainstream media outlet")
    names(info)[info > 1] = paste0(names(info)[info > 1], "s")
    info = subset(info, select = info > 0)
    plain_text <- paste(paste(info, names(info)), collapse = ", ")

  out <-  ifelse(nchar(plain_text) > 0, paste0("--- ", plain_text), "N/A")
  out <- gsub("citation\\b",glue::glue("[citation]({cite_link})"),out)
  out <- gsub("citations",glue::glue("[citations]({cite_link})"),out)
  out

  }

  pubs$extra_info = sapply(seq_along(pubs$doi), extra_info)

  # get compelte authors ---------

  if (file.exists(author_path)) {
    complete_authors <- readRDS(author_path)
    pubs <- merge(pubs, complete_authors, all.x = TRUE)
  } else{
    pubs$complete_authors = NA
  }

  if(any(is.na(pubs$complete_authors))){
  pubs$complete_authors[is.na(pubs$complete_authors)] = scholar::get_complete_authors(id, pubs$pubid[is.na(pubs$complete_authors)])
  }

  if (cache_author) {
    to_cache <- pubs[, c("pubid", "complete_authors")]
    saveRDS(to_cache, file = author_path)
  }

  pubs$author = pubs$complete_authors

  pubs$author = standardise_authors(pubs$author)

  if (is.finite(n)) {
    pubs <- pubs[order(pubs$cites, decreasing = TRUE), ][1:n, ]
  }
  pubs = pubs[order(pubs$author_last),]
  pubs = pubs[order(pubs$year, decreasing = TRUE),]
  pubs
}

standardise_authors = function(x){
  x = gsub("J Conigrave", "JH Conigrave", x)
  x = gsub("JH Conigrave", "<u>JH Conigrave<\\/u>", x)
  x = gsub("KS Lee", "KSK Lee", x)
  x
}

#' predict_new_cites
#'
#' Predict how many cites you'll get in the current year
#' @param end_date date at which to end at. Defaults to end of the year
#' @export

predict_new_cites = function(end_date = NULL, id = "m0d4TKcAAAAJ"){

  current_date <- Sys.Date()
  current_year <- paste0(substring(current_date,1,4))
  start_date <- as.Date(paste(current_year, 1,1, sep = "-"))

  if(is.null(end_date)){
    end_date<- as.Date(paste(current_year, 12,31, sep = "-"))
  }
  so_far <- as.numeric(difftime(current_date, start_date, units = "days"))
  total <- as.numeric(difftime(end_date, start_date, units = "days"))

  cites <- unlist(scholar::get_citation_history(id)["cites"])
  cites <- cites[length(cites)]
  unname(round(cites / (so_far / total),2))
}

#' export_pubs
#'
#' Export publications to ris
#' @param path path to output file
#' @param format format of output file (ris or bib)
#' @export

export_pubs <- function(path = NULL, format = "bib"){
  p <- publications()
  p$bibtype <- "article"
  p$author <- p$complete_authors
  p$author <- gsub("J Conigrave", "JH Conigrave", p$author)
  p$author <- gsub("\\, ", " and ", p$author)
  p$journal <- gsub("^\\*", "" , p$journal)
  p$journal <- gsub("\\*$", "", p$journal)
  p$number <- trimws(p$number)
  p$number <- gsub("^\\*", "" , p$number)
  p$number <- gsub("\\*$", "", p$number)
  p <- p[! p$journal %in% c("PsyArXiv", "Academy of Management Proceedings"), ]
  if(is.null(path)) return(p)
  bib <-  revtools::as.bibliography(p)
  revtools::write_bibliography(bib, path, format = "bib")

}
