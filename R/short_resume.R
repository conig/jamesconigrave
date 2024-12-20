#' short_resume
#'
#' Creates a short resume at path
#' @param path path to output file
#' @export

short_resume <- function(path = "short_resume.pdf", n_pubs = 8) {
  update_resume(
    push = FALSE,
    path = path,
    education = 2,
    experience = 3,
    professional = FALSE,
    line_height = 1.09,
    block_margin = .05,
    n_pubs = n_pubs,
    peer_review = FALSE,
    workshops = FALSE,
    commentaries = FALSE,
    preprints = FALSE,
    conferences = FALSE,
    supervision = TRUE,
    committees = FALSE,
  )
}
