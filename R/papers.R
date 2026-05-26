#' papers
#'
#' select and open first author papers
#' @export

papers = function(){
  dir <- system.file("papers",  package= "conig")
  papers <- list.files(dir)


  path <- utils::select.list(
    title = "What paper would you like to open?",
    choices = papers
  )

  open_with_default_app(glue::glue("{dir}/{path}"))

}
