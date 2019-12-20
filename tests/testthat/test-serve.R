testthat::test_that("Package upload/deletion works as expected", {
  # Clean repository.
  repo <- file.path(tempfile("cranium"), "repo")
  port <- httpuv::randomPort()
  p <- start_server(repo, port)

  pkg <- "cranium2_0.0.0.9000.tar.gz"
  pkgfile <- testthat::test_path(file.path("pkg", pkg))

  response <- httr::POST(
    sprintf("127.0.0.1:%d/", port),
    body = list(file = httr::upload_file(pkgfile)),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 201)
  testthat::expect_false(httr::has_content(response))

  # Same package again, should see a conflict.
  response <- httr::POST(
    sprintf("127.0.0.1:%d/", port),
    body = list(file = httr::upload_file(pkgfile)),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 409)
  testthat::expect_true(httr::has_content(response))

  # Overwrite the package directly with PUT.
  response <- httr::PUT(
    sprintf("127.0.0.1:%d/src/contrib/%s", port, pkg),
    body = list(file = httr::upload_file(pkgfile)),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 200)
  testthat::expect_false(httr::has_content(response))

  # And again, because this is idempotent.
  response <- httr::PUT(
    sprintf("127.0.0.1:%d/src/contrib/%s", port, pkg),
    body = list(file = httr::upload_file(pkgfile)),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 200)
  testthat::expect_false(httr::has_content(response))

  # And now we delete it.
  response <- httr::DELETE(
    sprintf("127.0.0.1:%d/src/contrib/%s", port, pkg),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 200)
  testthat::expect_false(httr::has_content(response))

  # Which only works once.
  response <- httr::DELETE(
    sprintf("127.0.0.1:%d/src/contrib/%s", port, pkg),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 404)
  testthat::expect_false(httr::has_content(response))

  # And now it's in the Archive.
  response <- httr::HEAD(
    sprintf("127.0.0.1:%d/src/contrib/Archive/cranium2/%s", port, pkg),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 200)
  testthat::expect_false(httr::has_content(response))

  # Which we can also DELETE.
  response <- httr::DELETE(
    sprintf("127.0.0.1:%d/src/contrib/Archive/cranium2/%s", port, pkg),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 200)
  testthat::expect_false(httr::has_content(response))

  p$kill()
})

testthat::test_that("Non-package files can't be DELETEd.", {
  port <- httpuv::randomPort()
  p <- start_server("/tmp/repo1", port)

  response <- httr::DELETE(
    sprintf("127.0.0.1:%d/src/contrib/PACKAGES.rds", port),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 400)
  testthat::expect_true(httr::has_content(response))

  response <- httr::DELETE(
    sprintf("127.0.0.1:%d/src/contrib/PACKAGES", port),
    httr::timeout(3)
  )
  testthat::expect_equal(httr::status_code(response), 400)
  testthat::expect_true(httr::has_content(response))

  p$kill()
})
