library(psychTestR)

display_number <- function(i) {
  one_button_page(paste("Page", i))
}

timeline <- join(
  lapply(1:5, display_number),
  final_page("End.")
)

make_test(timeline, opt = test_options("Minimal test", "password"))
