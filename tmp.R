library(psychTestR)

elts <- list(
  reactive_page(function(...) one_button_page("Done")),
  reactive_page(function(...) final_page("Done")),
)

make_test(elts)
