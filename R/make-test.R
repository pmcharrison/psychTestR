#' @export
make_test <- function(elts, opt = demo_options(),
                      custom_admin_panel = NULL) {
  stopifnot(is.list(opt), is.null.or(custom_admin_panel, is.function))
  check_dirs(opt)
  check_elts(elts)
  shiny::shinyApp(
    ui = ui(opt = opt),
    server = server(elts = elts, opt = opt,
                    custom_admin_panel = custom_admin_panel))
}
