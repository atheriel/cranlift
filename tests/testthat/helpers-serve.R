start_server <- function(repo, port = 8000) {
  p <- callr::r_bg(function(repo, port) {
    cranlift::serve(repo = repo, port = port)
  }, args = list(repo = repo, port = port), supervise = TRUE)

  Sys.sleep(1)
  testthat::expect_true(p$is_alive())

  p
}
