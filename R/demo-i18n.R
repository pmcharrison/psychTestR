demo.i18n <- function() {
  dict <- i18n_dict$new(data.frame(
    key = c("welcome", "weather", "sun", "rain", "bye"),
    GB = c("Hello!", "What's the weather like today?",
           "It's sunny", "It's rainy", "Goodbye!"),
    FR = c("Bonjour!", "Quel temps fait-il?",
           "Il fait beau", "Il pleut", "Au revoir!"),
    stringsAsFactors = FALSE
  ))
  timeline <- new_i18n_timeline(dict = dict, x = {
    list(
      one_button_page("Hi"),
      one_button_page(i18n("welcome")),
      NAFC_page("weather", i18n("weather"), choices = i18n("sun", "rain")),
      final_page(i18n("bye"))
    )
  })
  make_test(timeline)
}
