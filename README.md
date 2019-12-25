
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cranlift: Serve CRAN-like Repositories as RESTful APIs

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/cranlift)](https://cran.r-project.org/package=cranlift)
<!-- badges: end -->

Like the `miniCRAN` and `drat` packages, `cranlift` allows users to
manage their own CRAN-like R package archives. However, `cranlift` does
so by running a fully-featured API server that exposes packages as
resources that can be added, deleted, and modified using HTTP requests,
in line with RESTful design principles.

`cranlift` supports both source and binary packages, as well as CRAN’s
Archive layout for storing old versions of packages.

## Installation

`cranlift` is not yet available on [CRAN](https://CRAN.R-project.org).
For now, you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atheriel/cranlift")
```

## Usage

To serve an (empty) CRAN-like repository, call `cranlift::serve()`:

``` r
# You could use two R processes for this instead of callr::r_bg().
p <- callr::r_bg(function() {
  dir.create("/tmp/myrepo", FALSE)
  cranlift::serve("/tmp/myrepo", port = 8000)
}, supervise = TRUE)

nrow(available.packages(repos = "http://127.0.0.1:8000"))
#> [1] 0
```

Now let’s add an example package to the repository, say `miniCRAN`.
Because we’re interacting with a server, we do this with an HTTP
request:

``` r
path <- download.packages("miniCRAN", tempdir(), type = "source")[1,2]
httr::PUT(
  sprintf("127.0.0.1:8000/src/contrib/%s", basename(path)),
  body = list(file = httr::upload_file(path))
)
#> Response [http://127.0.0.1:8000/src/contrib/miniCRAN_0.2.12.tar.gz]
#>   Date: 2019-12-20 18:27
#>   Status: 201
#>   Content-Type: text/plain; charset=utf-8
#> <EMPTY BODY>

nrow(available.packages(
  repos = "http://127.0.0.1:8000", ignore_repo_cache = TRUE
))
#> [1] 1
```

Deleting a package (say, because a new version is available) works
similarly:

``` r
httr::DELETE(
  sprintf("127.0.0.1:8000/src/contrib/%s", basename(path))
)
#> Response [http://127.0.0.1:8000/src/contrib/miniCRAN_0.2.12.tar.gz]
#>   Date: 2019-12-20 18:27
#>   Status: 200
#>   Content-Type: text/plain; charset=utf-8
#> <EMPTY BODY>

nrow(available.packages(
  repos = "http://127.0.0.1:8000", ignore_repo_cache = TRUE
))
#> [1] 0
```

By default, deleted packages will be moved to the archive:

``` r
httr::HEAD(sprintf(
  "127.0.0.1:8000/src/contrib/Archive/miniCRAN/%s", basename(path)
))
#> Response [http://127.0.0.1:8000/src/contrib/Archive/miniCRAN/miniCRAN_0.2.12.tar.gz]
#>   Date: 2019-12-20 18:27
#>   Status: 200
#>   Content-Type: x-gzip
#> <EMPTY BODY>
```

## Usage with `drat` and `miniCRAN`

`cranlift` can serve repositories created with `drat` and `miniCRAN`
(and has a strong emphasis on compatibility with the common repository
layout they use), but be warned that it may modify the `PACKAGES` files
these packages create.

## License

`cranlift` is made available under the terms of the MIT license. See the
`LICENSE.md` file for details.
