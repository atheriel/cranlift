testthat::test_that("The server can respond to /_ping requests", {
  port <- httpuv::randomPort()
  p <- start_server("/tmp/repo1", port)

  response <- httr::GET(sprintf("127.0.0.1:%d/_ping", port), httr::timeout(3))
  testthat::expect_equal(httr::status_code(response), 200)
  testthat::expect_false(httr::has_content(response))

  p$kill()
})
