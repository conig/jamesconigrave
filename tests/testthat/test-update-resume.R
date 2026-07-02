local_fake_conig_install <- function() {
  env <- parent.frame()
  root <- tempfile("conig-install-")
  dir.create(root)
  withr::local_options(list(conig.source_dir = NA), .local_envir = env)
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
      expect_true(file.exists(input))
      expect_true(dir.exists(dirname(output_file)))

      if (is.null(output_format)) {
        print_rendered <<- TRUE
        print_html <<- output_file
        expect_equal(basename(input), basename(expected_md))
        expect_false(identical(input, expected_md))
        expect_false(identical(output_file, expected_web_html))
        writeLines("<html>pagedown resume</html>", output_file)
      } else {
        web_rendered <<- TRUE
        expect_equal(input, expected_md)
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
  expect_true(file.exists(file.path(root, "to_github", "docs", "jamesconigrave_resume.pdf")))
  expect_true(file.exists(file.path(root, "to_github", "docs", "james-h-conigrave-cv.pdf")))
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

test_that("web resume PDF link stays inside the published docs tree", {
  # GitHub Pages publishes docs/index.html, so the browser-visible PDF must be
  # addressed beside that page rather than through its unserved parent folder.
  script <- paste(
    readLines(
      conig_file("resume_files/resume-alt-script.html", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(script, 'action\\.href = "james-h-conigrave-cv\\.pdf";')
  expect_false(grepl("../jamesconigrave_resume.pdf", script, fixed = TRUE))
})

test_that("publication search has an empty state when no records match", {
  # The web CV should keep the Publications section visible when search filters
  # every publication out, with a clear empty state in the list.
  script <- paste(
    readLines(
      conig_file("resume_files/resume-alt-script.html", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )
  css <- paste(
    readLines(
      conig_file("resume_files/style-rules-alt.css", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(script, "publication-empty-state")
  expect_match(script, "No publications match")
  expect_match(css, "\\.publication-empty-state")
})

test_that("publication search uses an expanding material line field", {
  script <- paste(
    readLines(
      conig_file("resume_files/resume-alt-script.html", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )
  css <- paste(
    readLines(
      conig_file("resume_files/style-rules-alt.css", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(script, 'field\\.className = "publication-search-field";')
  expect_match(script, 'icon\\.className = "publication-search-icon";')
  expect_match(
    script,
    'tools\\.classList\\.toggle\\("has-query", Boolean\\(query\\)\\);'
  )
  expect_match(css, "\\.publication-search-field::after")
  expect_match(css, "\\.publication-tools:focus-within \\.publication-search-field")
  expect_match(css, "\\.publication-tools\\.has-query \\.publication-search-field")
  expect_match(css, "transform:\\s*scaleX\\(1\\);")
  expect_match(css, "flex:\\s*1 1 16rem;")
  expect_false(grepl("\\.publication-tools input:focus", css))
  expect_false(grepl("grid-template-columns:\\s*1fr auto", css))
})

test_that("alternate resume renders dash-prefixed notes without repeated labels", {
  script <- paste(
    readLines(
      conig_file("resume_files/resume-alt-script.html", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )
  css <- paste(
    readLines(
      conig_file("resume_files/style-rules-alt.css", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(script, "isEntryNoteText")
  expect_match(script, "stripEntryNotePrefix")
  expect_match(script, "isEntrySidecarText")
  expect_match(script, "buildEntryYear")
  expect_match(script, "appendSidecarItemContent")
  expect_match(script, "fullLink\\.textContent = p\\.textContent\\.trim\\(\\)")
  expect_match(script, "entry-sidecar")
  expect_match(script, "entry-support-note")
  expect_match(script, "mergeEntryRole")
  expect_match(css, "\\.entry-sidecar")
  expect_match(css, "\\.entry-support-note")
  expect_match(css, "\\.entry-context-line")
  expect_false(grepl("entryMetaLabel", script))
  expect_false(grepl("entry-meta-label", css))
  expect_false(grepl("entry-meta-list", css))
  expect_false(grepl("\\.resume-entry \\.entry-note", css))
})

test_that("alternate resume links and section states use SC1 accent semantics", {
  css <- paste(
    readLines(
      conig_file("resume_files/style-rules-alt.css", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(css, "--sc-accent-base:\\s*#0f4a32;")
  expect_match(css, "--sc-accent-display:\\s*#287552;")
  expect_match(css, "--sc-accent-highlight:\\s*#e8f2ed;")
  expect_match(css, "--sc-accent-dim:\\s*#1d5f43;")
  expect_match(css, "--spectrum-amber:\\s*#ebd88d;")
  expect_match(css, "--spectrum-brown:\\s*#d08a56;")
  expect_match(css, "--link-text:\\s*var\\(--sc-accent-base\\);")
  expect_match(css, "--link-hover:\\s*var\\(--sc-accent-display\\);")
  expect_match(css, "--section-accent:\\s*var\\(--sc-accent-dim\\);")
  expect_match(css, "a:visited")
  expect_match(css, "a:active")
  expect_match(css, "\\.hero-download:visited")
  expect_match(css, "\\.hero-download:active")
  expect_match(
    css,
    "\\.resume-nav a:hover,\\s*\\.resume-nav a\\.is-active \\{\\s*color:\\s*var\\(--section-accent\\);"
  )
  expect_match(
    css,
    "\\.resume-nav a\\.is-active::before \\{\\s*background:\\s*var\\(--spectrum-brown\\);"
  )
  expect_match(
    css,
    "\\.publication-year-nav a\\.is-year-active \\{\\s*color:\\s*var\\(--section-accent\\);"
  )
  expect_match(
    css,
    "\\.publication-year-nav li\\.is-year-active::after \\{\\s*background:\\s*var\\(--spectrum-brown\\);"
  )
  expect_match(css, "var\\(--spectrum-amber\\)\\s+50%")
  expect_match(css, "\\.section-kicker \\{[^}]*color:\\s*var\\(--spectrum-brown\\);")
  expect_match(css, "\\.metric-value \\{[^}]*color:\\s*var\\(--ink\\);")
  expect_match(css, "\\.entry-year \\{[^}]*color:\\s*var\\(--section-accent\\);")
  expect_match(
    css,
    "\\.publication-year-break span:first-child \\{[^}]*color:\\s*var\\(--section-accent\\);"
  )
  expect_match(css, "@keyframes accent-brown-shift")
  expect_match(css, "\\.resume-hero::before \\{[^}]*animation:\\s*accent-brown-shift")
  expect_match(css, "\\.hero-download \\{[^}]*color:\\s*var\\(--surface\\);[^}]*background:\\s*var\\(--link-text\\);")
  expect_false(grepl("hue-rotate\\(360deg\\)", css))
  expect_false(grepl("--spectrum-cool|--spectrum-warm|--spectrum-yellow", css))
  expect_false(grepl("background-image:\\s*linear-gradient\\(var\\(--link-hover\\)", css))
  expect_false(grepl("\\.resume-entry:hover\\s+h3", css))
  expect_false(grepl("\\.citation-record:hover\\s+\\.citation-title", css))
  expect_false(grepl("#metrics\\s*\\{\\s*grid-template-columns:\\s*1fr;", css))
})

test_that("sidebar progress track is bounded by the primary section dots", {
  # When the publication year tree collapses while scrolling, the rail fill must
  # not keep an old height beyond the final section dot. The base track should
  # also end at the last primary section node rather than at the nav bottom.
  script <- paste(
    readLines(
      conig_file("resume_files/resume-alt-script.html", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )
  css <- paste(
    readLines(
      conig_file("resume_files/style-rules-alt.css", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(script, "navDotCenter")
  expect_match(script, "setProgressTrack")
  expect_match(script, "requestProgressRefresh")
  expect_match(script, "Math\\.min\\(trackHeight, Math\\.max\\(0,")
  expect_match(css, "\\.resume-progress \\{[^}]*overflow:\\s*hidden;")
  expect_false(grepl("inset:\\s*1\\.9rem auto 0 0;", css))
})

test_that("research output group is labelled Publications", {
  rmd <- paste(
    readLines(
      conig_file("resume_files/jamesconigrave_resume.rmd", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(rmd, 'alt_group_title = "Publications"')
  expect_false(grepl('alt_group_title = "Research outputs"', rmd, fixed = TRUE))
})

test_that("date ranges are ordered and rendered with en dashes", {
  rmd <- paste(
    readLines(
      conig_file("resume_files/jamesconigrave_resume.rmd", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )
  script <- paste(
    readLines(
      conig_file("resume_files/resume-alt-script.html", mustWork = TRUE),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(rmd, 'paste\\(years, collapse = "–"\\)')
  expect_match(rmd, "student_year_range")
  expect_false(grepl('collapse = " - "', rmd, fixed = TRUE))
  expect_false(grepl("{year_end} - {year_start}", rmd, fixed = TRUE))
  expect_false(grepl("\\b\\d{4} - \\d{4}\\b", rmd))
  expect_match(script, "split\\(/\\\\s\\*\\[-–\\]\\\\s\\*/\\)")
  expect_match(script, 'firstYear > secondYear \\? second \\+ "–" \\+ first')
  expect_false(grepl('" - Present"', script, fixed = TRUE))
})

test_that("PDF render source excludes the web-only profile heading", {
  root <- local_fake_conig_install()
  resume_rmd <- file.path(root, "resume_files", "jamesconigrave_resume.rmd")
  writeLines(
    c(
      "---",
      "title: Test",
      "output:",
      "  pagedown::html_resume:",
      "    css: [\"style-rules.css\", \"resume\"]",
      "---",
      "",
      "## JAMES H. CONIGRAVE {#title}",
      "",
      "### **Motivation, substance use, and public health**",
      "",
      "## Profile {data-icon=user}",
      "",
      "Research Fellow profile text."
    ),
    resume_rmd
  )
  rendered_input <- NULL
  rendered_output <- tempfile(fileext = ".html")

  local_mocked_bindings(
    render = function(input, output_file, output_dir, envir, ...) {
      rendered_input <<- input
      lines <- readLines(input, warn = FALSE)
      expect_false(any(grepl("^## Profile\\b", lines)))
      expect_true(any(grepl("Research Fellow profile text\\.", lines)))
      writeLines("<html>pagedown resume</html>", output_file)
      invisible(output_file)
    },
    .package = "rmarkdown"
  )

  render_pdf_resume_html(resume_rmd, rendered_output, envir = environment())

  expect_false(identical(rendered_input, resume_rmd))
  expect_true(file.exists(rendered_output))
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
  expect_true(file.exists(file.path(checkout, "docs", "jamesconigrave_resume.pdf")))
  expect_true(file.exists(file.path(checkout, "docs", "james-h-conigrave-cv.pdf")))
})

test_that("update_resume push mode refreshes every resume asset copy", {
  # The publish checkout is not the only durable copy: the package assets are
  # committed here too, so push mode must not leave them stale.
  root <- local_fake_conig_install()
  checkout <- file.path(tempfile("conig-user-data-"), "resume")
  source_root <- tempfile("conig-source-")
  dir.create(source_root)
  writeLines(
    c(
      "Package: conig",
      "Version: 0.0.0"
    ),
    file.path(source_root, "DESCRIPTION")
  )
  dir.create(file.path(source_root, "inst", "resume_files"), recursive = TRUE)
  dir.create(file.path(source_root, "inst", "to_github", "docs"), recursive = TRUE)

  stale_files <- c(
    file.path(root, "to_github", "docs", "index.html"),
    file.path(root, "resume_files", "jamesconigrave_resume.pdf"),
    file.path(root, "to_github", "jamesconigrave_resume.pdf"),
    file.path(root, "to_github", "docs", "jamesconigrave_resume.pdf"),
    file.path(root, "to_github", "docs", "james-h-conigrave-cv.pdf"),
    file.path(source_root, "inst", "to_github", "docs", "index.html"),
    file.path(source_root, "inst", "resume_files", "jamesconigrave_resume.pdf"),
    file.path(source_root, "inst", "to_github", "jamesconigrave_resume.pdf"),
    file.path(source_root, "inst", "to_github", "docs", "jamesconigrave_resume.pdf"),
    file.path(source_root, "inst", "to_github", "docs", "james-h-conigrave-cv.pdf")
  )
  for (stale_file in stale_files) {
    dir.create(dirname(stale_file), recursive = TRUE, showWarnings = FALSE)
    writeLines("stale", stale_file)
  }

  withr::local_options(list(conig.source_dir = source_root))

  calls <- local_mock_resume_renderers(
    root,
    output_root = checkout,
    pdf_path = file.path(checkout, "jamesconigrave_resume.pdf")
  )

  local_mocked_bindings(
    conig_resume_checkout_dir = function() checkout,
    .package = "conig"
  )

  local_mocked_bindings(
    git_clone = function(url, path, ...) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(path, ".git"), recursive = TRUE)
      invisible(path)
    },
    git_add = function(files, repo, ...) invisible(NULL),
    git_commit = function(repo, message, ...) invisible(NULL),
    git_push = function(repo, ...) invisible(NULL),
    .package = "gert"
  )

  update_resume()

  expect_true(calls$print_rendered())
  expect_true(calls$web_rendered())
  expect_true(calls$printed())

  html_files <- c(
    file.path(checkout, "docs", "index.html"),
    file.path(root, "to_github", "docs", "index.html"),
    file.path(source_root, "inst", "to_github", "docs", "index.html")
  )
  for (html_file in html_files) {
    expect_equal(readLines(html_file, warn = FALSE), "<html>alternate resume</html>")
  }

  pdf_files <- c(
    file.path(checkout, "jamesconigrave_resume.pdf"),
    file.path(checkout, "docs", "jamesconigrave_resume.pdf"),
    file.path(checkout, "docs", "james-h-conigrave-cv.pdf"),
    file.path(root, "resume_files", "jamesconigrave_resume.pdf"),
    file.path(root, "to_github", "jamesconigrave_resume.pdf"),
    file.path(root, "to_github", "docs", "jamesconigrave_resume.pdf"),
    file.path(root, "to_github", "docs", "james-h-conigrave-cv.pdf"),
    file.path(source_root, "inst", "resume_files", "jamesconigrave_resume.pdf"),
    file.path(source_root, "inst", "to_github", "jamesconigrave_resume.pdf"),
    file.path(source_root, "inst", "to_github", "docs", "jamesconigrave_resume.pdf"),
    file.path(source_root, "inst", "to_github", "docs", "james-h-conigrave-cv.pdf")
  )
  for (pdf_file in pdf_files) {
    expect_equal(readChar(pdf_file, 8, useBytes = TRUE), "%PDF-1.4")
  }
})
