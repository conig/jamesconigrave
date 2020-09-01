

#' get_pubs
#'
#'Get and format publications
#' @param id google scholar id
#' @param n how many pubs to retrieve?
#' @param exclude_journals vector of journal names to filter out

publications = function(id = "m0d4TKcAAAAJ",
                        n = Inf,
                        journal_exclude = c("ACU Research Bank"),
                        cache_doi = TRUE) {
  pubs = scholar::get_publications(id, flush = TRUE)
  pubs = pubs[!pubs$journal %in% journal_exclude,]
  pubs$author_last = gsub("^\\w{1,} *", "", pubs$author)

  cache_path = paste0(system.file("", package = "jamesconigrave"),
                      "/",
                      id,
                      "_doi.rds")

  # merge in DOI information

  if (file.exists(cache_path)) {
    dois <- readRDS(cache_path)
    pubs <- merge(pubs, dois, all.x = TRUE)
  } else{
    pubs$doi = NA
  }

  pubs$doi[is.na(pubs$doi)] = get_doi(pubs[is.na(pubs$doi), ])

  if (cache_doi) {
    to_cache <- pubs[, c("pubid", "doi")]

    saveRDS(to_cache, file = cache_path)
  }

  # add altmetric --------------------------------------

  pubs <- cbind(pubs, do.call(rbind, lapply(pubs$doi, altmetric)))

  pubs$journal = ifelse(nchar(pubs$journal) > 0,
                        paste0("*", pubs$journal, "*"),
                        pubs$journal)
  pubs$number = ifelse(nchar(pubs$number) > 0,
                       paste0(" *", pubs$number, "*"),
                       pubs$number)

  extra_info = function(row) {
    info <- pubs[row, c("cites", "tweets", "media")]
    info[is.na(info)] = 0
    names(info) = c("citation", "tweet", "mainstream media outlet")
    names(info)[info > 1] = paste0(names(info)[info > 1], "s")
    info = subset(info, select = info > 0)
    plain_text <- paste(paste(info, names(info)), collapse = ", ")

    ifelse(nchar(plain_text) > 0, paste0("--- ", plain_text), "N/A")
  }

  pubs$extra_info = sapply(seq_along(pubs$doi), extra_info)

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


