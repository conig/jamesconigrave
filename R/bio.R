#' bio
#'
#' James's bio
#' @export

bio = function(){
  paste(
    "James is a Research Fellow at the Centre for Research Excellence: Indigenous Health and Alcohol",
    "He received a PhD in psychology from ACU",
    "He enjoys statistics, programming, and automation",
    "James is interested in the relationship between alcohol consumption and basic psychological need satisfaction.",
    sep = ". ")
}

#' years_since_conferral
#'
#' Returns years since PhD Conferral
#' @export

years_since_conferral <- function(){
  conferral <- as.Date("2019-05-22")
  years <- as.numeric(difftime(Sys.Date(), conferral, units = "days") / 365.25)
  names(years) <- paste0("Years since 2019-05-22")
  round(years,2)
}
