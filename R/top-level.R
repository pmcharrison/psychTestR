#' @export
make_test <- function(elts, title,
                      options = pt_options()
                      # side_panel = new_side_panel()
                      ) {
  stopifnot(is.scalar.character(title), is.list(options))
  check_dirs(options)
  check_elts(elts)
  shiny::shinyApp(
    ui = ui(title = title, options = options),
    server = server(elts = elts, options = options))
}
