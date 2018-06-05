elts <- c(
  psychTest::one_button_page(
    shiny::div(shiny::p("This demo demonstrates the dynamic admin UI functionality."),
               shiny::p("If you login to the admin panel (password = demo),",
                        "you should be able to see extra information about each page.",
                        "This information is not available outside of admin mode.")),
    admin_ui = "This is where the dynamic admin UI is located."),
  psychTest::one_button_page(
    "You should see 'Hello' at the top of the admin panel.",
    admin_ui = "Hello"
  ),
  psychTest::NAFC_page(
    label = "nafc",
    prompt = "What's my favourite colour?",
    choices = c("Red", "Green"),
    admin_ui = "My favourite colour is green."
  ),
  psychTest::reactive_page(function(answer, ...) {
    psychTest::one_button_page(
      if (answer == "Green") "Correct!" else "Incorrect."
    )
  }),
  psychTest::code_block(function(state, ...) {
    psychTest::set_global("fave_number", sample.int(1e3, 1), state)
  }),
  psychTest::reactive_page(function(state, ...) {
    x <- psychTest::get_global("fave_number", state)
    psychTest::text_input_page("fave_number",
                               "What's my favourite number?",
                               validate = function(answer, ...) {
                                 ans <- as.numeric(answer)
                                 if (ans == x) TRUE else "Incorrect. Try again."
                               },
                               admin_ui = sprintf("It's %i.", x))
  }),
  psychTest::one_button_page("Correct!"),
  psychTest::final_page("That's the end of the demo.")
)

test <- psychTest::make_test(elts,
                             opt = psychTest::pt_options(title = "Demo admin UI",
                                                         admin_password = "demo",
                                                         researcher_email = "p.m.c.harrison@qmul.ac.uk"))
shiny::runApp(test)
