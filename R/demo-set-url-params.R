#' Demo: Getting URL parameters
#'
#' Demonstrates how psychTestR can set URL parameters.
#' @export
demo.set_url_params <- function() {
  elts <- join(
    psychTestR::one_button_page("We will now demonstrate the setting of URL parameters."),
    psychTestR::text_input_page(
      label = "input",
      prompt = "Enter something to be added to your URL.",
      on_complete = function(state, answer, session, ...) {
        psychTestR::set_url_param(key = "input", value = answer, session, state)
      }),
    psychTestR::final_page("You should now see your text added to the URL."))
  psychTestR::make_test(
    elts,
    opt = psychTestR::pt_options(title = "Demo set URL params",
                                admin_password = "demo",
                                researcher_email = "p.m.c.harrison@qmul.ac.uk"))
}
