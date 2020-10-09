#' affiliation
#'
#' My affiliation
#' @param papaja if true, affiliations are printed so they can be pasted into a papaja manuscript.
#' @export

affiliation = function(papaja = FALSE){
  out <- c("1" = "University of Sydney, Faculty of Medicine and Health, Central Clinical School, Discipline of Addiction Medicine, New South Wales Australia",
    "2" = "NHMRC Centre of Research Excellence in Indigenous Health and Alcohol")

  if(papaja){
    return(glue::glue(r"(
    - id            : "1"
    institution   : "{out[1]}"
    - id            : "2"
    institution   : "{out[2]}"
    )"))
  }
  out
  }
