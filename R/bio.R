#' bio
#'
#' James's bio
#' @export

bio = function(){
  glue::glue("Research Fellow at the Centre for Alcohol Policy Research (CAPR), La Trobe University, with a background in psychology. My work examines motivation, behaviour change, and health risk, with a focus on substance use, public health, and practical digital tools such as the Grog Survey App. I combine psychology, biostatistics, and programming to build reproducible evidence for policy and practice."
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

#' academic_letters
#' 
#' Returns string which includes academic letters
#' @param include_area logical. If TRUE, includes the area of study in the string and separates each entry by commas.
#' @export

academic_letters <- function(include_area = FALSE){
  letters <- "PhD (Psychology), MPH, BA (Psych), BHlth"
  if(!include_area){
    letters <- gsub("\\s\\(.*?\\)", "", letters)
    letters <- gsub(",", "", letters)
  }
  letters
}
