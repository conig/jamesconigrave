#' papers
#'
#' select and open first author papers
#' @export

papers = function(){
  dir <- system.file("papers",  package= "jamesconigrave")
  papers <- list.files(dir)

  path <- select.list(
    title = "What paper would you like to open?",
    choices = papers
  )

  shell.exec(glue::glue("{dir}/{path}"))

}
