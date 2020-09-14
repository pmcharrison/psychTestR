library(psychTestR)

elts <- new_timeline(c(
  one_button_page(i18n("hello")),
  final_page(i18n("goodbye"))
), dict = i18n_dict$new(data.frame(key = c("hello", "goodbye"),
                                   en = c("Hello", "Goodbye"),
                                   fr = c("Bonjour", "Au revoir"))))


# This should work
make_test(elts = elts,
          opt = demo_options(title = c(en = "English title",
                                       fr = "French title"),
                             languages = c("en", "fr"),
                             problems_info = c(en = "English problems info",
                                               fr = "French problems info")))

# This should work
make_test(elts = elts,
          opt = demo_options(title = c("All-language title"),
                             languages = c("en", "fr"),
                             problems_info = "All-language problems info"))

# This should throw an error
make_test(elts = elts,
          opt = demo_options(title = c(en = "English title"),
                             languages = c("en", "fr"),
                             problems_info = c(en = "English problems info")))

# This should throw an error
make_test(elts = elts,
          opt = demo_options(title = c("All-language title"),
                             languages = c("en", "fr"),
                             problems_info = c(en = "English problems info")))
