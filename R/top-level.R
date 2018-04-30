#' @export
make_test <- function(elts, title,
                      options = psychTest_options()
                      # side_panel = new_side_panel()
                      ) {
  stopifnot(is.scalar.character(title), is.list(options))
  check_dirs(options)
  shiny::shinyApp(
    ui = ui(title = title, options = options),
    server = server(elts = elts, options = options))
}
