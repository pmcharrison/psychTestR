library(psychTestR)
make_item <- function(n) {
  NAFC_page(label = paste0("q", n),
            prompt = paste0("Question ", n),
            choices = as.character(1:10),
            save_answer = TRUE,
            on_complete = function(state, opt, ...) save_results_to_disk(
              complete = FALSE, state = state, opt = opt),
            arrange_vertically = FALSE)
}
elts <- join(lapply(1:4, make_item),
             elt_save_results_to_disk(complete = TRUE),
             final_page("End"))

make_test(elts)
