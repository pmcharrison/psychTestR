library(psychTestR)

elts <- new_timeline(c(
  one_button_page(i18n("hello")),
  final_page(i18n("goodbye"))
), dict = i18n_dict$new(data.frame(key = c("hello", "goodbye"),
                                   en = c("Hello", "Goodbye"),
                                   fr = c("Bonjour", "Au revoir"))))


make_test(elts = elts,
          opt = demo_options(
            title = c(en = "English title",
                      fr = "French title"),
            languages = c("en"),
            problems_info = c(en = "English problems info",
                              fr = "French problems info"),
            advance_delay = 0))

