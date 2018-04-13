#' @export
make_test <- function(elts, title,
                      options = psychTest_options(),
                      side_panel = side_panel()) {
  stopifnot(is.scalar.character(title), is.list(options))
  shiny::shinyApp(
    ui = ui(title = title, options = options),
    server = server(elts = elts, side_panel = side_panel, options = options))
}
