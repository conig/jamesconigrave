#xaringan::inf_mr("james resume.rmd")

#' resume
#'
#' open resume
#' @param path If provided, the resume is copied to that destination. In this instance, the file is not opened
#' @export resume

resume = function(path = NULL) {
  location = system.file("to_github/resume/jamesconigrave_resume.pdf", package = "conig")

  if (!is.null(path)) {
    file.copy(from = location,
              to = path,
              overwrite = T)
  } else{
    shell.exec(location)
  }
}

#' website
#'
#' open resume
#' @export website

website = function() {
  shell.exec("https://jconigrave.github.io/")
}

#' update_resume
#'
#' update resume and optionally push changes to github
#' @param education render education
#' @param experience render experience
#' @param workshop include workshops
#' @param supervision include supervision
#' @param committees include committees
#' @param pacakge include packages
#' @param n_pubs numeric, top n cited publications, defaults to Inf
#' @param commentaries include commentaries by others
#' @param peer_review include peer review activity
#' @param preprint include pre-prints
#' @param conferences include conference contributions
#' @param grants include grants
#' @param path if included, resume is additionally copied to path specified
#' @param push if TRUE, changes are pushed to github
#' @param ... additional arguments passed to css
#' @export update_resume

update_resume <- function(push = FALSE,
                         education = TRUE,
                         experience = TRUE,
                         workshops = FALSE,
                         supervision = TRUE,
                         committees = TRUE,
                         packages = TRUE,
                         n_pubs = Inf,
                         commentaries = TRUE,
                         peer_review = TRUE,
                         preprints = TRUE,
                         conferences = TRUE,
                         grants = TRUE,
                         path = NULL,
                         ...) {
  css(path = system.file("resume_files/style-rules.css", package = "conig"),
      ...)

  md <- system.file("resume_files/jamesconigrave_resume.rmd", package = "conig")
  gh <- system.file("to_github", package = "conig")
  root <- system.file("", package = "conig")

  # If no gh file...
  if(gh == ""){
    dir.create(paste0(root, "/to_github/resume"), recursive = TRUE)
    gh <- system.file("to_github", package = "conig")
  } 

  # Check if git has already been init
  if (!dir.exists(paste0(gh, "/.git")) & push == TRUE) {
    message("git dir doesn't exist, initialising... ",
            glue::glue("{gh}"))

  # If resume folder already exists, delete it
  if(dir.exists(paste0(gh, "/resume"))){
    unlink(paste0(gh, "/resume"), recursive = TRUE)
  }

  # Clone the repo using gert
  gert::git_clone(url = "https://github.com/conig/resume.git",
    path = gh)

  }

  resume_html <- paste0(root, "to_github/docs/index.html")
  resume_pdf <- paste0(root, "resume_files/jamesconigrave_resume.pdf")

  rmarkdown::render(md,
                    output_file = resume_html)


  pagedown::chrome_print(input = resume_html,
                         output = resume_pdf)

  file.copy(
    from = resume_pdf,
    to = system.file("to_github/jamesconigrave_resume.pdf", package = "conig"),
    overwrite = TRUE
  )

  if (!is.null(path)) {
    if (tools::file_ext(path) == "pdf") {
      file.copy(from = resume_pdf,
                to = path,
                overwrite = TRUE)
    }
    if (tools::file_ext(path) == "html") {
      file.copy(from = resume_html,
                to = path,
                overwrite = TRUE)
    }

  }

  if (push) {
    gert::git_add(files = "*", repo = glue::glue("{gh}"))
    gert::git_commit(
      repo = glue::glue("{gh}"),
      message = glue::glue("auto update {Sys.time()}")
    )
    gert::git_push(repo = glue::glue("{gh}"))
  }
}


fix_case = function(string,
                    words = c(
                      "aboriginal",
                      "torres",
                      "strait",
                      "islander",
                      "australian",
                      "australia",
                      "australian's"
                    )) {
  words2 = stringr::str_to_title(words)

  for (i in seq_along(words)) {
    string = gsub(words[i], words2[i], string)
  }

  return(string)

}

#' short_resume
#'
#' Creates a short resume at path
#' @param path path to output file

short_resume <- function(path = "short_resume.pdf", n_pubs = 8) {
  update_resume(
    path = path,
    line_height = 1.09,
    block_margin = .05,
    n_pubs = n_pubs,
    experience = FALSE,
    peer_review = FALSE,
    workshops = FALSE,
    commentaries = FALSE,
    preprints = FALSE,
    conferences = FALSE,
    supervision = TRUE
  )
}
