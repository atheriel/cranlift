repository <- function(path, fields = NULL) {
  if (!is.character(path) || length(path) != 1) {
    stop("A repository must be represented by a valid path.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("No repository exists at '", path, "'.", call. = FALSE)
  }
  out <- structure(
    list(path = path, fields = fields, contrib_urls = character()),
    class = "cranium_repository"
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
    type <- if (grepl("bin/windows", url, fixed = TRUE)) {
      "win.binary"
    } else if (grepl("bin/macosx/el-capitan", url, fixed = TRUE)) {
      "mac.binary.el-capitan"
    } else {
      "mac.binary"
    }
    cranlike::update_PACKAGES(url, fields = out$fields, type = type)
  }

  out$contrib_urls <- index_dirs
  out
}
