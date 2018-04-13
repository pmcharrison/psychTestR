library(shiny)
library(psychTest)

options(shiny.error = browser)

elts <- list(
  one_button_page("Welcome to the test!"),
  NAFC_page("What's your favourite colour?",
            choices = c("Red", "Green", "Blue"),
            set_global = "colour"),
  reactive_page(function(state) {
    prompt <- sprintf("Your favourite colour is %s.",
                      get_global("colour", state))
    one_button_page(prompt)
  }),
  final_page("Thanks for taking part!")
)
sp <- side_panel()

test <- make_test(
  elts = elts,
  title = "psychTest Example",
  options = psychTest_options(),
  side_panel = sp
)
runApp(test)
