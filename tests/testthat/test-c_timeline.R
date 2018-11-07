context("c_timeline")

library(testthat)

test_that("combining timelines from the same language", {
  t1 <- new_timeline(c(
    one_button_page("Hello"),
    one_button_page("Goodbye")
  ))
  expect_equal(c(t1), t1)

  t2 <- new_timeline(c(
    one_button_page("Hello again"),
    one_button_page("Goodbye again")
  ))
  t1.2 <- c(t1, t2)
  expect_equal(t1.2$length, 4)
  expect_equal(t1.2$languages, "EN")
  expect_equal(t1.2$get("EN", 1), t1$get("EN", 1))
  expect_equal(t1.2$get("EN", 3), t2$get("EN", 1))
  expect_true(identical(t1.2$get("EN", 3),
                        t2$get("EN", 1)))
  expect_false(identical(t1.2$get("EN", 3),
                        t2$get("EN", 2)))

})

test_that("combining timelines from different languages", {
  dict1 <- i18n_dict$new(data.frame(
    key = c("welcome", "weather", "sun", "rain", "bye", "sorry", "good"),
    EN = c("Hello!", "What's the weather like today?",
           "It's sunny", "It's rainy", "Goodbye!",
           "I'm sorry...", "That's good!"),
    FR = c("Bonjour!", "Quel temps fait-il?",
           "Il fait beau", "Il pleut", "Au revoir!",
           "Je suis desole...", "C'est bon!"),
    stringsAsFactors = FALSE
  ))
  dict2 <- i18n_dict$new(data.frame(
    key = c("welcome", "weather", "sun", "rain", "bye", "sorry", "good"),
    EN = c("Hello!", "What's the weather like today?",
           "It's sunny", "It's rainy", "Goodbye!",
           "I'm sorry...", "That's good!"),
    DE = c("Willkommen", "Wie ist das Wetter heute?",
           "Es ist sonnig", "Es ist regnerisch",
           "Auf Wiedersehen", "Es tut mir Leid",
           "Das ist gut"),
    stringsAsFactors = FALSE
  ))
  dict3 <- i18n_dict$new(data.frame(
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

  t1 <- new_timeline(c(
    one_button_page(i18n("sun")),
    one_button_page(i18n("rain"))
  ), dict = dict1)
  t2 <- new_timeline(c(
    one_button_page(i18n("welcome")),
    one_button_page(i18n("bye"))
  ), dict = dict2)
  t3 <- new_timeline(c(
    one_button_page(i18n("sorry")),
    one_button_page(i18n("bye"))
  ), dict = dict3)

  t_all <- c(t1, t2, t3)

  expect_equal(t_all$length, 6)
  expect_equal(t_all$languages, "EN")
  expect_equal(t_all$get("EN", 6), t3$get("EN", 2))
})

test_that("c.timeline() where the second element is a list of timelines", {
  a <- one_button_page("a")
  b <- one_button_page("b")
  c <- one_button_page("c")

  x <- new_timeline(a)
  y <- list(new_timeline(a),
            new_timeline(b),
            new_timeline(c))
  z <- c(x, y)

  expect_equal(z$get("EN"),
               c(a, a, b, c))
})
