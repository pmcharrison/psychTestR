#' @export
demo.get_url_params <- function() {
  elts <- c(
    psychTest::one_button_page("We will now demonstrate getting URL parameters."),
    psychTest::reactive_page(function(state, ...) {
      msg <- psychTest::get_url_params(state)$print
      psychTest::one_button_page(
        if (is.null(msg))
          shiny::p(
            "Try deleting 'p_id=...' from your URL and adding 'print=MyMessage'",
            "instead. Load the resulting URL."
          ) else
            shiny::div(shiny::p("Your message should display here:", msg))
                       # shiny::p("Your p_id is", p_id(state)))
      )}),
    psychTest::final_page("That's the end of the demo.")
  )
  psychTest::make_test(
    elts,
    opt = psychTest::pt_options(title = "Demo URL params",
                                admin_password = "demo",
                                researcher_email = "p.m.c.harrison@qmul.ac.uk"))
}
