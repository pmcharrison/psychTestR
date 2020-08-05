context("timeline")

test_that("drop_languages", {
  dict <- i18n_dict$new(data.frame(
    key = c("welcome", "weather", "sun", "rain", "bye", "sorry", "good"),
    en = c("Hello!", "What's the weather like today?",
           "It's sunny", "It's rainy", "Goodbye!",
           "I'm sorry...", "That's good!"),
    fr = c("Bonjour!", "Quel temps fait-il?",
           "Il fait beau", "Il pleut", "Au revoir!",
           "Je suis desole...", "C'est bon!"),
    de = c("Willkommen", "Wie ist das Wetter heute?",
           "Es ist sonnig", "Es ist regnerisch",
           "Auf Wiedersehen", "Es tut mir Leid",
           "Das ist gut"),
    stringsAsFactors = FALSE
  ))

  t <- new_timeline(c(
    one_button_page(i18n("sun")),
    one_button_page(i18n("rain"))
  ), dict = dict)

  expect_equal(t$languages, c("de", "en", "fr"))
  t$drop_languages("de")
  expect_equal(t$languages, c("en", "fr"))
  t$drop_languages(c("en", "fr"))
  expect_equal(t$languages, character())
})

test_that("timelines of length 1", {
  p <- one_button_page("Hello")
  t <- new_timeline(list(p))
  expect_equal(
    t$get("en"),
    list(p)
  )
})
