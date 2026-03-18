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
