testthat::test_that("We can serve repositories created with drat", {
  dir <- tempfile("cranlift")
  dir.create(dir, FALSE)
  drat::initRepo("drat", basepath = dir)

  # Drat does not create a binary package path, so we have to do it manually.
  dir.create(
    contrib.url(file.path(dir, "drat"), type = "win.binary"), FALSE, TRUE
  )

  # Insert a binary package.
  pkg <- "miniCRAN_0.2.12.zip"
  pkgfile <- testthat::test_path(file.path("pkg", pkg))
  drat::insertPackage(pkgfile, repodir = file.path(dir, "drat"))

  port <- httpuv::randomPort()
  p <- start_server(file.path(dir, "drat"), port)

  pkgs <- available.packages(
    repos = sprintf("127.0.0.1:%d/", port), type = "win.binary"
  )
  testthat::expect_equal(nrow(pkgs), 1)

  p$kill()
})

testthat::test_that("We can serve repositories created with miniCRAN", {
  repo <- file.path(tempfile("cranlift"), "miniCRAN")
  dir.create(repo, FALSE, TRUE)
  miniCRAN::makeRepo(
    "foreach", path = repo, repos = c("CRAN" = "https://cloud.r-project.org/"),
    type = c("source", "win.binary"), quiet = TRUE
  )

  port <- httpuv::randomPort()
  p <- start_server(repo, port)

  pkgs <- available.packages(
    repos = sprintf("127.0.0.1:%d/", port), type = "source"
  )
  testthat::expect_equal(nrow(pkgs), 1)

  pkgs <- available.packages(
    repos = sprintf("127.0.0.1:%d/", port), type = "win.binary"
  )
  testthat::expect_equal(nrow(pkgs), 1)

  p$kill()
})
