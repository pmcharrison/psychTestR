library(shiny)
library(psychTest)
options(shiny.error = browser)

# example audio http://research.pmcharrison.com/studies/HarmonyDissonance/chords/piano/48_66/48_66.mp3"

elts <- list(
  new_section("Introduction"),
  one_button_page("Welcome to the test!"),
  one_button_page("You will answer some questions."),
  NAFC_page("What's your favourite colour?",
            choices = c("Orange", "Green", "Blue")),
  # reactive_page(function(state) {
  #   one_button_page(sprintf("Your favourite colour is %s.",
  #                           get_global("colour", state)))
  # }),
  audio_NAFC_page(
    "Do you like this chord?",
    choices = c("Yes", "No"),
    url = "http://research.pmcharrison.com/studies/HarmonyDissonance/chords/piano/48_66/48_66.mp3",
    wait = FALSE),
  new_section("Follow up"),
  NAFC_page("Are you bored yet?", choices = c("Yes", "No")),
  NAFC_page("Are you ready to finish?", choices = c("Yes", "No")),
  save_data_locally(),
  final_page("You finished the test!")
)

test <- make_test(
  elts = elts,
  title = "The test"
)
runApp(test)
