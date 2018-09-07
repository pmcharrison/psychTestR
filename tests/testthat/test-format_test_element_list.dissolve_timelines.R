context("format_test_element_list.dissolve_timelines")

test_that("example", {
  dict <- i18n_dict$new(data.frame(
    key = c("welcome", "weather", "sun", "rain", "bye", "sorry", "good"),
    EN = c("Hello!", "What's the weather like today?",
           "It's sunny", "It's rainy", "Goodbye!",
           "I'm sorry...", "That's good!"),
    FR = c("Bonjour!", "Quel temps fait-il?",
           "Il fait beau", "Il pleut", "Au revoir!",
           "Je suis desole...", "C'est bon!"),
    DE = c("Willkommen", "Wie ist das Wetter heute?",
           "Es ist sonnig", "Es ist regnerisch",
           "Auf Wiedersehen", "Es tut mir Leid",
           "Das ist gut"),
    stringsAsFactors = FALSE
  ))

  l <- list(
    one_button_page("Hello"),
    one_button_page("Hello again"),
    new_timeline(c(
      one_button_page(i18n("rain")),
      one_button_page(i18n("bye"))),
      dict = dict)
  )
  l2 <- psychTestR:::format_test_element_list.dissolve_timelines(l, "EN")
  expect_equal(l2, list(
    one_button_page("Hello"),
    one_button_page("Hello again"),
    one_button_page("It's rainy"),
    one_button_page("Goodbye!")
  ), check.attributes = FALSE)
})
