#' Demo: Getting URL parameters
#'
#' Demonstrates how psychTestR can get URL parameters.
#' @export
demo.get_url_params <- function() {
  elts <- c(
    psychTestR::one_button_page("We will now demonstrate getting URL parameters."),
    psychTestR::reactive_page(function(state, ...) {
      msg <- psychTestR::get_url_params(state)$print
      psychTestR::one_button_page(
        if (is.null(msg))
          shiny::p(
            "Try deleting 'p_id=...' from your URL and adding 'print=MyMessage'",
            "instead. Load the resulting URL."
          ) else
            shiny::div(shiny::p("Your message should display here:", msg))
      )}),
    psychTestR::final_page("That's the end of the demo.")
  )
  psychTestR::make_test(
    elts,
    opt = psychTestR::pt_options(title = "Demo URL params",
                                 admin_password = "demo",
                                 researcher_email = "p.m.c.harrison@qmul.ac.uk"))
}
