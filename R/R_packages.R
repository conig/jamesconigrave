#' R_packages

R_packages <- function(n = Inf){

  info <- "https://api.github.com/users/conig/repos" |>
    readLines() |>
    suppressWarnings() |>
    jsonlite::fromJSON()

  out <-
    info[, c(
      "name",
      "html_url",
      "size",
      "watchers",
      "has_issues",
      "description",
      "updated_at",
      "fork",
      "language",
      "created_at"
    )] |>
    data.table::data.table()

  out <-
    out[order(-updated_at)][!name %in% c("resume", "jamesconigrave", "conig.github.io") &
                              !fork & language == "R"]
  if(is.infinite(n)) n <- nrow(out)

  out$updated_at <- as.Date(out$updated_at)
  out$created_at <- as.Date(out$created_at)
  out[1:n]

}
