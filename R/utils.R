#' Conversion from Paths to PkgType
#'
#' @param path A path string.
#' @return One of the standard package types, or \code{NA}.
#'
#' @noRd
pkgtype_from_path <- function(path) {
  if (grepl("bin/windows", path, fixed = TRUE)) {
    "win.binary"
  } else if (grepl("bin/macosx/el-capitan", path, fixed = TRUE)) {
    "mac.binary.el-capitan"
  } else if (grepl("bin/macosx", path, fixed = TRUE)) {
    "mac.binary"
  } else if (grepl("src/", path, fixed = TRUE)) {
    "source"
  } else {
    NA_character_
  }
}

#' Regexe for a PkgType
#'
#' @param type One of the standard package types.
#' @return A regular expression to match packages, or \code{NA}.
#'
#' @noRd
pkg_regex <- function(type) {
  switch(
    type, "source" = "\\.tar\\..*$", "win.binary" = "\\.zip$",
    "mac.binary" = "\\.tgz$", "mac.binary.el-capitan" = "\\.tgz$", NA_character_
  )
}
