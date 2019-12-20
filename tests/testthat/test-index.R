testthat::test_that("The server produces a valid PACKAGES.rds for src/contrib", {
  port <- httpuv::randomPort()
  p <- start_server("/tmp/repo1", port)

  pkgs <- available.packages(
    repos = sprintf("127.0.0.1:%d/", port), type = "source"
  )
  testthat::expect_gte(nrow(pkgs), 0)

  pkgs <- available.packages(
    repos = sprintf("127.0.0.1:%d/", port), type = "win.binary"
  )
  testthat::expect_gte(nrow(pkgs), 0)

  pkgs <- available.packages(
    repos = sprintf("127.0.0.1:%d/", port), type = "mac.binary"
  )
  testthat::expect_gte(nrow(pkgs), 0)

  # Bad R version.
  response <- httr::GET(
    sprintf("127.0.0.1:%d/bin/windows/2.6/PACKAGES.rds", port), httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 404)

  p$kill()
})
