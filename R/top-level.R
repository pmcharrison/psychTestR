#' @export
make_test <- function(elts, options = pt_options()) {
  stopifnot(is.list(options))
  check_dirs(options)
  check_elts(elts)
  shiny::shinyApp(
    ui = ui(options = options),
    server = server(elts = elts, options = options))
}
