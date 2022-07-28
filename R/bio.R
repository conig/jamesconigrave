#' bio
#'
#' James's bio
#' @export

bio = function(){
  glue::glue("I am a Research Fellow ({round(years_since_conferral()[[1]],1)} years since conferral) at the University of Sydney. I am interested in understanding the motivational underpinnings of risk taking behaviour. I am passionate about reproducible research, and enjoy writing software packages to help researchers clearly and efficiently communicate their findings."
    )
}

#' years_since_conferral
#'
#' Returns years since PhD Conferral
#' @export

years_since_conferral <- function(end_date = NULL) {
  if (is.null(end_date))
    end_date <- Sys.Date()
  end_date <- as.Date(end_date)
  conferral <- as.Date("2019-05-22")
  completion <- as.Date("2018-10-10")
  conferral_years <-
    as.numeric(difftime(end_date, conferral, units = "days") / 365.25)
  completion_years <-
    as.numeric(difftime(end_date, completion, units = "days") / 365.25)
  years <- c(conferral_years, completion_years)

  names(years) <-
    c("Since conferral (2019-05-22)",
      "Since completion (2018-10-10)")
  out <- t(t(round(years, 2)))
  colnames(out) <- "years"
  out
}
