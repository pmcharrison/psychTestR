#' @export
demo.set_url_params <- function() {
  elts <- c(
    psychTest::one_button_page("We will now demonstrate the setting of URL parameters."),
    psychTest::text_input_page(
      label = "input",
      prompt = "Enter something to be added to your URL.",
      on_complete = function(state, answer, session, ...) {
        psychTest::set_url_param(key = "input", value = answer, session, state)
      }),
    psychTest::final_page("You should now see your text added to the URL."))
  psychTest::make_test(
    elts,
    opt = psychTest::pt_options(title = "Demo set URL params",
                                admin_password = "demo",
                                researcher_email = "p.m.c.harrison@qmul.ac.uk"))
}
