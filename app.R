library(shiny)
library(psychTest)
options(shiny.error = browser)

elts <- list(
  one_button_page("Welcome to the test!"),
  # get_p_id_page(),
  NAFC_page("What's your favourite colour?",
            choices = c("Yellow", "Red", "Green")),
  audio_NAFC_page("Do you like this chord?",
                  url = "http://research.pmcharrison.com/studies/HarmonyDissonance/chords/piano/48_66/48_66.mp3",
                  choices = c("Yes", "No")),
  save_data_locally(),
  final_page("You finished the test!")
)

test <- make_test(elts, title = "Daniel and Klaus's test",
                  options = psychTest_options(auto_p_id = TRUE))
runApp(test)
