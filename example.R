library(shiny)
library(psychTest)

options(shiny.error = browser)

colour <- list(
  NAFC_page("What's your favourite colour?",
            choices = c("Red", "Green", "Blue"),
            save_options = get_save_options(global_key = "colour")),
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
                save_options = get_save_options(result_key = "Favourite Simpsons",
                                                global_key = "simpsons")),
  reactive_page(function(state) {
    prompt <- sprintf("Your favourite Simpsons character is %s.",
                      get_global("simpsons", state))
    one_button_page(prompt)
  })
)

elts <- c(
  one_button_page("Welcome to the test!"),
  new_section("Trivia"),
  colour,
  simpsons,
  new_section("Serious questions"),
  NAFC_page("What is 1 + 1?", choices = c("1", "2", "3")),
  NAFC_page("What is 2 + 2?", choices = c("2", "3", "4")),
  code_block(function(state) {
    x <- results(state); x
    browser()
  }),
  final_page("Thanks for taking part!")
)
sp <- new_side_panel()

test <- make_test(
  elts = elts,
  title = "psychTest Example",
  options = psychTest_options(),
  side_panel = sp
)
runApp(test)
