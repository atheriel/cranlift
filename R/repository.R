repository <- function(path, fields = NULL) {
  if (!is.character(path) || length(path) != 1) {
    stop("A repository must be represented by a valid path.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("No repository exists at '", path, "'.", call. = FALSE)
  }
  out <- structure(
    list(path = path, fields = fields, contrib_urls = character()),
    class = "cranlift_repository"
  )

  # Ensure that the repository has a functioning index for source and binary
  # packages (that match the local R version).

  for (type in c("source", "mac.binary", "win.binary")) {
    url <- contrib.url(out$path, type = type)
    if (!dir.exists(url)) {
      dir.create(url, recursive = TRUE, showWarnings = FALSE)
    }
    out$contrib_urls <- c(out$contrib_urls, url)

    if (!file.exists(file.path(url, "PACKAGES.rds"))) {
      cranlike::create_empty_PACKAGES(url, fields = out$fields)
    }
  }

  # Catch binary packages for R versions that don't match the local one. These
  # indexes need to be up-to-date as well. Since PACKAGES is the oldest index
  # format, look for that one in particular.

  index_dirs <- dirname(
    list.files(out$path, "PACKAGES$", recursive = TRUE, full.names = TRUE)
  )
  for (url in setdiff(index_dirs, out$contrib_urls)) {
    type <- pkgtype_from_path(url)
    cranlike::update_PACKAGES(url, fields = out$fields, type = type)
  }

  out$contrib_urls <- index_dirs
  out
}

print.cranlift_repository <- function(x, ...) {
  print(summary(x))
}

summary.cranlift_repository <- function(object, ...) {
  out <- list(path = object$path, pkgs = integer(), archived = logical())

  out$pkgs <- vapply(object$contrib_urls, FUN = function(url) {
    regex <- pkg_regex(pkgtype_from_path(url))
    length(list.files(url, pattern = regex))
  }, FUN.VALUE = integer(1))

  out$archived <- vapply(object$contrib_urls, FUN = function(url) {
    if (!file.exists(file.path(url, "Archive"))) return(0L)
    regex <- pkg_regex(pkgtype_from_path(url))
    length(
      list.files(file.path(url, "Archive"), pattern = regex, recursive = TRUE)
    )
  }, FUN.VALUE = integer(1))

  structure(out, class = "cranlift_repository_summary")
}

print.cranlift_repository_summary <- function(x, ...) {
  cat("CRAN-Like Repository at '", x$path, "'\n", sep = "")
  cat("Archives: ", if (any(x$archived > 0)) "yes" else "no", "\n", sep = "")
  urls <- names(x$pkgs)
  headings <- vapply(urls, pkgtype_from_path, character(1))
  version_regex <- ".*([0-9\\.]{3})$"
  has_rversion <- grepl(version_regex, urls)
  cat("Packages\n")
  for (i in 1:length(urls)) {
    cat(
      "  ", headings[i], if (has_rversion[i]) {
        sprintf(" (for R %s)", sub(version_regex, "\\1", urls[i]))
      }, ": ", x$pkgs[i], "\n", sep = ""
    )
  }
}
