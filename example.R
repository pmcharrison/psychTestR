library(shiny)
library(psychTest)

options(shiny.error = browser)

colour <- list(
  NAFC_page("What's your favourite colour?",
            choices = c("Red", "Green", "Blue"),
            set_global = "colour"),
  reactive_page(function(state) {
    prompt <- sprintf("Your favourite colour is %s.",
                      get_global("colour", state))
    one_button_page(prompt)
  })
)

simpsons <- list(
  dropdown_page("Who's your favourite Simpsons character?",
                choices = c("Marge", "Homer", "Bart", "Lisa"),
                alternative_choice = TRUE,
                set_global = "simpsons"),
  reactive_page(function(state) {
    prompt <- sprintf("Your favourite Simpsons character is %s.",
                      get_global("simpsons", state))
    one_button_page(prompt)
  })
)

elts <- c(
  one_button_page("Welcome to the test!"),
  colour,
  simpsons,
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
