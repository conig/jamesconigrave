test_that("get_complete_authors_batched fetches authors in 30-item batches", {
  calls <- list()
  sleep_calls <- numeric()

  local_mocked_bindings(
    get_complete_authors = function(id, pubids) {
      calls[[length(calls) + 1]] <<- list(id = id, pubids = pubids)
      paste("author", pubids)
    },
    .package = "scholar"
  )

  authors <- conig:::get_complete_authors_batched(
    id = "abc123",
    pubids = as.character(seq_len(65)),
    batch_size = 30,
    batch_delay = 0.25,
    sleep_fn = function(seconds) {
      sleep_calls <<- c(sleep_calls, seconds)
      invisible(NULL)
    }
  )

  expect_length(calls, 3)
  expect_equal(lengths(lapply(calls, `[[`, "pubids")), c(30L, 30L, 5L))
  expect_equal(vapply(calls, `[[`, character(1), "id"), rep("abc123", 3))
  expect_equal(sleep_calls, c(0.25, 0.25))
  expect_equal(authors, paste("author", as.character(seq_len(65))))
})

test_that("get_complete_authors_batched returns empty output for empty input", {
  local_mocked_bindings(
    get_complete_authors = function(...) {
      fail("scholar::get_complete_authors should not be called for empty input")
    },
    .package = "scholar"
  )

  expect_equal(
    conig:::get_complete_authors_batched("abc123", character()),
    character()
  )
})

test_that("open_with_default_app uses xdg-open on Linux when available", {
  calls <- list()

  result <- conig:::open_with_default_app(
    target = "/tmp/example.pdf",
    os_type = "unix",
    sys_name = "Linux",
    which_fn = function(command) {
      expect_equal(command, "xdg-open")
      "/usr/bin/xdg-open"
    },
    system2_fn = function(command, args, wait) {
      calls[[length(calls) + 1]] <<- list(command = command, args = args, wait = wait)
      0
    },
    shell_exec_fn = function(...) fail("shell.exec should not be used on Linux"),
    browse_url_fn = function(...) fail("browseURL should not be used when xdg-open exists")
  )

  expect_equal(result, "/tmp/example.pdf")
  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$command, "xdg-open")
  expect_equal(calls[[1]]$args, "/tmp/example.pdf")
  expect_false(calls[[1]]$wait)
})

test_that("open_with_default_app falls back to browseURL when xdg-open is unavailable", {
  opened <- character()

  result <- conig:::open_with_default_app(
    target = "https://www.conigrave.com/",
    os_type = "unix",
    sys_name = "Linux",
    which_fn = function(command) {
      expect_equal(command, "xdg-open")
      ""
    },
    system2_fn = function(...) fail("system2 should not be used without xdg-open"),
    shell_exec_fn = function(...) fail("shell.exec should not be used on Linux"),
    browse_url_fn = function(target) {
      opened <<- c(opened, target)
      invisible(NULL)
    }
  )

  expect_equal(result, "https://www.conigrave.com/")
  expect_equal(opened, "https://www.conigrave.com/")
})
