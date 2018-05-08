#' @export
make_test <- function(elts, opt = pt_options()) {
  stopifnot(is.list(opt))
  check_dirs(opt)
  check_elts(elts)
  shiny::shinyApp(
    ui = ui(opt = opt),
    server = server(elts = elts, opt = opt))
}
