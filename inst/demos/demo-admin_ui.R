elts <- join(
  psychTestR::one_button_page(
    shiny::div(shiny::p("This demo demonstrates the dynamic admin UI functionality."),
               shiny::p("If you login to the admin panel (password = demo),",
                        "you should be able to see extra information about each page.",
                        "This information is not available outside of admin mode.")),
    admin_ui = "This is where the dynamic admin UI is located."),
  psychTestR::one_button_page(
    "You should see 'Hello' at the top of the admin panel.",
    admin_ui = "Hello"
  ),
  psychTestR::NAFC_page(
    label = "nafc",
    prompt = "What's my favourite colour?",
    choices = c("Red", "Green"),
    admin_ui = "My favourite colour is green."
  ),
  psychTestR::reactive_page(function(answer, ...) {
    psychTestR::one_button_page(
      if (answer == "Green") "Correct!" else "Incorrect."
    )
  }),
  psychTestR::code_block(function(state, ...) {
    psychTestR::set_global("fave_number", sample.int(1e3, 1), state)
  }),
  psychTestR::reactive_page(function(state, ...) {
    x <- psychTestR::get_global("fave_number", state)
    psychTestR::text_input_page("fave_number",
                               "What's my favourite number?",
                               validate = function(answer, ...) {
                                 ans <- as.numeric(answer)
                                 if (ans == x) TRUE else "Incorrect. Try again."
                               },
                               admin_ui = sprintf("It's %i.", x))
  }),
  psychTestR::one_button_page("Correct!"),
  psychTestR::final_page("That's the end of the demo.")
)

test <- psychTestR::make_test(elts,
                             opt = psychTestR::pt_options(title = "Demo admin UI",
                                                         admin_password = "demo",
                                                         researcher_email = "p.m.c.harrison@qmul.ac.uk"))
shiny::runApp(test)
