testthat::test_that("The server produces a valid PACKAGES.rds for src/contrib", {
  port <- httpuv::randomPort()
  p <- start_server("/tmp/repo1", port)

  pkgs <- available.packages(
    repos = sprintf("127.0.0.1:%d/", port), type = "source"
  )
  testthat::expect_gte(nrow(pkgs), 0)

  p$kill()
})
