#' Make psychTestR test
#'
#' The top-level function that defines a psychTestR test.
#' This should be the final line of your app script,
#' typically entitled \code{app.R}.
#' @param elts List of test elements defining the test's timeline.
#' @param opt Options list as created by \code{test_options()}.
#' @param custom_admin_panel This argument supports the construction of a custom
#' admin panel. The argument should be a function that runs during the
#' Shiny server function. The argument list of this function should
#' include \code{...}; it may also include
#' \code{state}, \code{input}, \code{output}, and \code{session},
#' all with their traditional meanings from Shiny or psychTestR.
#' This function should include an expression of the form
#' \code{output$custom_admin_panel <- shiny::renderUI(...)}
#' where the code inside \code{renderUI()} defines the UI of the admin panel
#' using standard Shiny UI vocabulary.
#' Further documentation for this option should be forthcoming.
#' @export
make_test <- function(elts, opt = demo_options(),
                      custom_admin_panel = NULL) {
  stopifnot(is.list(opt), is.null.or(custom_admin_panel, is.function))
  check_dirs(opt)
  if (is.list(elts)) elts <- new_timeline(elts)
  check_elts(elts)
  check_opt(opt, elts)
  shiny::shinyApp(
    ui = ui(opt = opt),
    server = server(elts = elts, opt = opt,
                    custom_admin_panel = custom_admin_panel)
    )
}



check_opt <- function(opt, elts) {
  stopifnot(is(elts, "timeline"))
  unsupported_languages <- setdiff(opt$languages, elts$languages)
  if (length(unsupported_languages) > 0L)
    stop("the following language(s) specified in `opt$languages` ",
         "are not supported by 'elts': ",
         paste(unsupported_languages, collapse = ", "),
         ". Consider removing some languages from `opt$languages`, ",
         " or otherwise adding relevant language support to `elts`.")
}
