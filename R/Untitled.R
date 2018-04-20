library(shiny)
library(psychTest)

elts <- list(
  one_button_page("Welcome to the test!"),
  NAFC_page("What's your favourite colour?",
            choices = c("Red", "Green", "Blue")),
  audio_NAFC_page("Do you like this chord?",
                  choices = c("Yes", "No"),
                  url = "http://research.pmcharrison.com/studies/HarmonyDissonance/chords/piano/48_66/48_66.mp3"),
  save_data_locally(),
  final_page("You finished the test!")
)

test <- make_test(elts = elts, title = "Manu's test")
