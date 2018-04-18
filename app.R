library(shiny)
library(psychTest)

elts <- list(
  one_button_page(
    tags$div(
      tags$h2("Hello hello hello"),
      tags$p(
        "Welcome to the test, ",
        tags$strong("Maddy!")
      )
    )
  ),
  one_button_page("You will answer some questions."),
  NAFC_page("What's your favourite colour?",
            choices = c("Orange", "Green", "Blue"),
            save_options = get_save_options(global_key = "colour")),
  reactive_page(function(state) {
    one_button_page(sprintf("Your favourite colour is %s.",
                            get_global("colour", state)))
  }),
  audio_NAFC_page(
    "Do you like this chord?",
    choices = c("Yes", "No"),
    url = "http://research.pmcharrison.com/studies/HarmonyDissonance/chords/piano/48_66/48_66.mp3"),
  final_page("You finished the test!", final = TRUE)
)

test <- make_test(
  elts = elts,
  title = "Maddy's test"
)
runApp(test)
