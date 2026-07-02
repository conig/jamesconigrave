#xaringan::inf_mr("james resume.rmd")

#' resume
#'
#' open resume
#' @param path If provided, the resume is copied to that destination. In this instance, the file is not opened
#' @export resume

resume <- function(path = NULL) {
  location = conig_file(
    "to_github/jamesconigrave_resume.pdf",
  )

  if (!is.null(path)) {
    file.copy(from = location, to = path, overwrite = TRUE)
  } else {
    open_with_default_app(location)
  }
}

#' update_alt_resume
#'
#' Create an alternate HTML resume using the canonical resume content.
#' @inheritParams update_resume
#' @return The generated HTML path, invisibly.
#' @export
update_alt_resume <- function(
  education = TRUE,
  experience = TRUE,
  professional = TRUE,
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
  path = NULL
) {
  md <- conig_file(
    "resume_files/jamesconigrave_resume.rmd",
    mustWork = TRUE
  )
  root <- conig_file("", mustWork = TRUE)
  docs_dir <- file.path(root, "to_github", "docs")
  if (!dir.exists(docs_dir)) {
    dir.create(docs_dir, recursive = TRUE)
  }

  resume_html <- file.path(docs_dir, "index.html")
  render_alt_resume(md, resume_html, envir = environment())

  if (!is.null(path)) {
    file.copy(from = resume_html, to = path, overwrite = TRUE)
  }

  invisible(resume_html)
}

alt_resume_format <- function() {
  rmarkdown::html_document(
    theme = NULL,
    self_contained = TRUE,
    css = conig_file(
      "resume_files/style-rules-alt.css",
      mustWork = TRUE
    ),
    includes = rmarkdown::includes(
      after_body = conig_file(
        "resume_files/resume-alt-script.html",
        mustWork = TRUE
      )
    )
  )
}

render_alt_resume <- function(input, output_file, envir) {
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  rmarkdown::render(
    input,
    output_format = alt_resume_format(),
    output_file = output_file,
    output_dir = output_dir,
    envir = envir
  )

  invisible(output_file)
}

render_pdf_resume_html <- function(input, output_file, envir) {
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  rmarkdown::render(
    input,
    output_file = output_file,
    output_dir = output_dir,
    envir = envir
  )

  invisible(output_file)
}

#' website
#'
#' open resume
#' @export website

website <- function() {
  open_with_default_app("https://www.conigrave.com/")
}

open_with_default_app <- function(
  target,
  os_type = .Platform$OS.type,
  sys_name = unname(Sys.info()[["sysname"]]),
  which_fn = Sys.which,
  system2_fn = system2,
  shell_exec_fn = shell.exec,
  browse_url_fn = utils::browseURL
) {

  if (os_type == "windows") {
    shell_exec_fn(target)
    return(invisible(target))
  }

  if (sys_name == "Darwin") {
    system2_fn("open", args = target, wait = FALSE)
    return(invisible(target))
  }

  if (os_type == "unix") {
    if (nzchar(which_fn("xdg-open"))) {
      system2_fn("xdg-open", args = target, wait = FALSE)
    } else {
      browse_url_fn(target)
    }
    return(invisible(target))
  }

  stop("Unsupported operating system")
}

conig_file <- function(..., mustWork = FALSE) {
  system.file(..., package = "conig", mustWork = mustWork)
}

conig_resume_checkout_dir <- function() {
  file.path(tools::R_user_dir("conig", "data"), "resume")
}

#' update_resume
#'
#' update resume and optionally push changes to github
#' @param education render education
#' @param experience render experience
#' @param professional render professional experience
#' @param workshops include workshops
#' @param supervision include supervision
#' @param committees include committees
#' @param packages include packages
#' @param n_pubs numeric, top n cited publications, defaults to Inf
#' @param commentaries include commentaries by others
#' @param peer_review include peer review activity
#' @param preprints include pre-prints
#' @param conferences include presentations
#' @param grants include grants
#' @param path if included, resume is additionally copied to path specified
#' @param push if TRUE, changes are pushed to github
#' @param ... additional arguments passed to css
#' @export update_resume

update_resume <- function(
  push = TRUE,
  education = TRUE,
  experience = TRUE,
  professional = TRUE,
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
  ...
) {
  css_path <- conig_file(
    "resume_files/style-rules.css",
    mustWork = TRUE
  )
  css(path = css_path, ...)

  md <- conig_file(
    "resume_files/jamesconigrave_resume.rmd",
    mustWork = TRUE
  )
  root <- conig_file("", mustWork = TRUE)
  gh <- if (push) {
    conig_resume_checkout_dir()
  } else {
    file.path(root, "to_github")
  }
  docs_dir <- file.path(gh, "docs")

  # If no gh file...

  # Check if git has already been init
  if (push && !dir.exists(file.path(gh, ".git"))) {
    message("git dir doesn't exist, initialising... ", glue::glue("{gh}"))
    dir.create(dirname(gh), recursive = TRUE, showWarnings = FALSE)

    if (dir.exists(gh)) {
      gh_files <- list.files(gh, all.files = TRUE, no.. = TRUE)
      if (length(gh_files) > 0) {
        unlink(gh, recursive = TRUE)
      }
    }

    # Clone the repo using gert
    gert::git_clone(url = "ssh://git@github.com/conig/resume.git", path = gh)
  }

  if (!dir.exists(gh)) {
    dir.create(gh, recursive = TRUE)
  }
  if (!dir.exists(docs_dir)) {
    dir.create(docs_dir, recursive = TRUE)
  }

  resume_html <- file.path(docs_dir, "index.html")
  resume_pdf_html <- tempfile("jamesconigrave-resume-print-", fileext = ".html")
  resume_pdf <- if (push) {
    file.path(gh, "jamesconigrave_resume.pdf")
  } else {
    file.path(root, "resume_files", "jamesconigrave_resume.pdf")
  }

  render_pdf_resume_html(md, resume_pdf_html, envir = environment())
  pagedown::chrome_print(input = resume_pdf_html, output = resume_pdf)

  render_alt_resume(md, resume_html, envir = environment())

  if (!push) {
    file.copy(
      from = resume_pdf,
      to = file.path(gh, "jamesconigrave_resume.pdf"),
      overwrite = TRUE
    )
  }

  if (!is.null(path)) {
    if (tools::file_ext(path) == "pdf") {
      file.copy(from = resume_pdf, to = path, overwrite = TRUE)
    }
    if (tools::file_ext(path) == "html") {
      file.copy(from = resume_html, to = path, overwrite = TRUE)
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


fix_case = function(
  string,
  words = c(
    "aboriginal",
    "torres",
    "strait",
    "islander",
    "australian",
    "australia",
    "australian's"
  )
) {
  words2 = stringr::str_to_title(words)

  for (i in seq_along(words)) {
    string = gsub(words[i], words2[i], string)
  }

  return(string)
}
