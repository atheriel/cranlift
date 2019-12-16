repository <- function(path, fields = NULL) {
  if (!is.character(path) || length(path) != 1) {
    stop("A repository must be represented by a valid path.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("No repository exists at '", path, "'.", call. = FALSE)
  }
  out <- structure(
    list(path = path, fields = fields), class = "cranium_repository"
  )

  # Ensure that the repository has a functioning index.
  # TODO: Support binary packages.

  src_url <- contrib.url(out$path, type = "source")
  if (!dir.exists(src_url)) {
    dir.create(src_url, recursive = TRUE, showWarnings = FALSE)
  }

  if (!file.exists(file.path(src_url, "PACKAGES.rds"))) {
    cranlike::create_empty_PACKAGES(src_url, fields = out$fields)
  } else {
    cranlike::update_PACKAGES(src_url, fields = out$fields, type = "source")
  }
}
