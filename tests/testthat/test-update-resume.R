local_fake_conig_install <- function() {
  env <- parent.frame()
  root <- tempfile("conig-install-")
  dir.create(root)
  resume_dir <- file.path(root, "resume_files")
  dir.create(resume_dir, recursive = TRUE)
  writeLines("", file.path(resume_dir, "style-rules.css"))
  writeLines("body { color: #17222d; }", file.path(resume_dir, "style-rules-alt.css"))
  writeLines("<script></script>", file.path(resume_dir, "resume-alt-script.html"))
  writeLines("", file.path(resume_dir, "jamesconigrave_resume.rmd"))

  local_mocked_bindings(
    conig_file = function(..., mustWork = FALSE) {
      path_parts <- unlist(list(...), use.names = FALSE)
      path <- if (length(path_parts) == 0 || identical(path_parts, "")) {
        root
      } else {
        do.call(file.path, as.list(c(root, path_parts)))
      }

      if (mustWork && !file.exists(path) && !dir.exists(path)) {
        stop("no file found", call. = FALSE)
      }

      path
    },
    .package = "conig",
    .env = env
  )

  root
}

local_mock_resume_renderers <- function(
  root,
  output_root = file.path(root, "to_github"),
  pdf_path = file.path(root, "resume_files", "jamesconigrave_resume.pdf")
) {
  env <- parent.frame()
  expected_md <- file.path(root, "resume_files", "jamesconigrave_resume.rmd")
  expected_web_html <- file.path(output_root, "docs", "index.html")
  expected_pdf <- pdf_path

  print_rendered <- FALSE
  web_rendered <- FALSE
  printed <- FALSE
  print_html <- NULL

  local_mocked_bindings(
    render = function(input, output_file, output_dir = NULL, output_format = NULL, ...) {
      expect_equal(input, expected_md)
      expect_true(dir.exists(dirname(output_file)))

      if (is.null(output_format)) {
        print_rendered <<- TRUE
        print_html <<- output_file
        expect_false(identical(output_file, expected_web_html))
        writeLines("<html>pagedown resume</html>", output_file)
      } else {
        web_rendered <<- TRUE
        expect_s3_class(output_format, "rmarkdown_output_format")
        expect_equal(output_file, expected_web_html)
        expect_equal(output_dir, dirname(expected_web_html))
        writeLines("<html>alternate resume</html>", output_file)
      }

      invisible(output_file)
    },
    .package = "rmarkdown",
    .env = env
  )
  local_mocked_bindings(
    chrome_print = function(input, output, ...) {
      printed <<- TRUE
      expect_equal(input, print_html)
      expect_false(identical(input, expected_web_html))
      expect_equal(output, expected_pdf)
      expect_true(file.exists(input))
      writeBin(charToRaw("%PDF-1.4\n"), output)
      invisible(output)
    },
    .package = "pagedown",
    .env = env
  )

  list(
    print_rendered = function() print_rendered,
    web_rendered = function() web_rendered,
    printed = function() printed
  )
}

test_that("update_resume creates CV outputs from an installed package root", {
  # Fresh installs may not have generated output directories yet.
  root <- local_fake_conig_install()
  calls <- local_mock_resume_renderers(root)

  update_resume(push = FALSE)

  expect_true(calls$print_rendered())
  expect_true(calls$web_rendered())
  expect_true(calls$printed())
  expect_true(file.exists(file.path(root, "to_github", "docs", "index.html")))
  expect_true(file.exists(file.path(root, "resume_files", "jamesconigrave_resume.pdf")))
  expect_true(file.exists(file.path(root, "to_github", "jamesconigrave_resume.pdf")))
})

test_that("update_alt_resume renders a separate web-native HTML resume", {
  root <- local_fake_conig_install()
  resume_rmd <- file.path(root, "resume_files", "jamesconigrave_resume.rmd")
  writeLines(
    c(
      "---",
      "output:",
      "  pagedown::html_resume:",
      "    css: [\"style-rules.css\", \"resume\"]",
      "---",
      "",
      "# Main",
      "",
      "## JAMES H. CONIGRAVE {#title}",
      "",
      "Same content"
    ),
    resume_rmd
  )

  rendered <- FALSE
  expected_html <- file.path(root, "to_github", "docs", "index.html")

  local_mocked_bindings(
    render = function(input, output_format, output_file, output_dir, envir, ...) {
      rendered <<- TRUE
      expect_equal(input, resume_rmd)
      expect_s3_class(output_format, "rmarkdown_output_format")
      expect_equal(output_file, expected_html)
      expect_equal(output_dir, dirname(expected_html))
      writeLines("<html>alternate resume</html>", output_file)
      invisible(output_file)
    },
    .package = "rmarkdown"
  )

  result <- update_alt_resume()

  expect_true(rendered)
  expect_equal(result, expected_html)
  expect_true(file.exists(expected_html))
})

test_that("update_resume bootstraps a fresh push checkout before rendering", {
  # A source or binary package can include stale generated files but no .git
  # repo. Push mode should clone into user data, not into the installed package.
  root <- local_fake_conig_install()
  checkout <- file.path(tempfile("conig-user-data-"), "resume")
  stale_docs <- file.path(root, "to_github", "docs")
  dir.create(stale_docs, recursive = TRUE)
  writeLines("stale", file.path(stale_docs, "index.html"))
  calls <- local_mock_resume_renderers(
    root,
    output_root = checkout,
    pdf_path = file.path(checkout, "jamesconigrave_resume.pdf")
  )

  cloned <- FALSE
  added <- FALSE
  committed <- FALSE
  pushed <- FALSE

  local_mocked_bindings(
    conig_resume_checkout_dir = function() checkout,
    .package = "conig"
  )

  local_mocked_bindings(
    git_clone = function(url, path, ...) {
      cloned <<- TRUE
      expect_equal(url, "ssh://git@github.com/conig/resume.git")
      expect_equal(path, checkout)
      entries <- if (dir.exists(path)) {
        list.files(path, all.files = TRUE, no.. = TRUE)
      } else {
        character()
      }
      expect_length(entries, 0)
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(path, ".git"), recursive = TRUE)
      invisible(path)
    },
    git_add = function(files, repo, ...) {
      added <<- TRUE
      expect_equal(files, "*")
      expect_true(dir.exists(file.path(as.character(repo), ".git")))
      invisible(NULL)
    },
    git_commit = function(repo, message, ...) {
      committed <<- TRUE
      expect_true(dir.exists(file.path(as.character(repo), ".git")))
      invisible(NULL)
    },
    git_push = function(repo, ...) {
      pushed <<- TRUE
      expect_true(dir.exists(file.path(as.character(repo), ".git")))
      invisible(NULL)
    },
    .package = "gert"
  )

  update_resume()

  expect_true(cloned)
  expect_true(calls$print_rendered())
  expect_true(calls$web_rendered())
  expect_true(calls$printed())
  expect_true(added)
  expect_true(committed)
  expect_true(pushed)
  expect_true(file.exists(file.path(checkout, "docs", "index.html")))
  expect_true(file.exists(file.path(checkout, "jamesconigrave_resume.pdf")))
})
