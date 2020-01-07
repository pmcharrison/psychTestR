library(psychTestR)

elts <- new_timeline(c(
  one_button_page(i18n("hello")),
  final_page(i18n("goodbye"))
), dict = i18n_dict$new(data.frame(key = c("hello", "goodbye"),
                                   EN = c("Hello", "Goodbye"),
                                   FR = c("Bonjour", "Au revoir"))))


make_test(elts = elts,
          opt = demo_options(
            title = c(EN = "English title",
                      FR = "French title"),
            languages = c("EN"),
            problems_info = c(EN = "English problems info",
                              FR = "French problems info"),
            advance_delay = 0))

