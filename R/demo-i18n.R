demo.i18n <- function() {
  dict <- i18n_dict$new(data.frame(
    key = c("welcome", "weather", "sun", "rain", "bye", "sorry", "good"),
    en = c("Hello!", "What's the weather like today?",
           "It's sunny", "It's rainy", "Goodbye!",
           "I'm sorry...", "That's good!"),
    fr = c("Bonjour!", "Quel temps fait-il?",
           "Il fait beau", "Il pleut", "Au revoir!",
           "Je suis desole...", "C'est bon!"),
    stringsAsFactors = FALSE
  ))
  timeline <- new_timeline(x = {
    list(
      one_button_page(i18n("welcome")),
      NAFC_page("weather", i18n("weather"),
                choices = c("sun", "rain"),
                labels = i18n(c("sun", "rain")),
                on_complete = function(answer, ...) {
                  shinyjs::alert(i18n(if (answer == "rain") "sorry" else "good"))
                }),
      final_page(i18n("bye"))
    )
  }, dict = dict)
  # t2 <- new_timeline(x = {
  #   list(
  #     one_button_page("Hi"),
  #     one_button_page("welcome"),
  #     NAFC_page("weather", "weather", choices = c("sun", "rain")),
  #     final_page("bye")
  #   )
  # }, dict = NULL)

  # French
  make_test(timeline, opt = demo_options(languages = "fr"))

  # # English
  # make_test(timeline, opt = demo_options(languages = "en"))
  #
  # # Fail
  # make_test(timeline, opt = demo_options(languages = "de"))
}
