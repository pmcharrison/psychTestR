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


# Admin password: test

# Delete all results directory, then run this test a few times according to the specification below.
# P1: pilot = FALSE, answers = 1, 3, 5, 7
# P2: pilot = TRUE, answers = 1, 2, 3, 4
# P3: pilot = FALSE, answers = 4, 3, 2, 1
# P4: pilot = FALSE, answers = 4, 3 (don't complete the test)

# Check statistics: you should see 2 completes and 1 partial complete registered. Times should be reasonable.
# Download the four types of result outputs, and check that they look reasonable.
# Delete all results, and check that the statistics go down to zero.

elts <- c(lapply(1:4, make_item),
          elt_save_results_to_disk(complete = TRUE),
          final_page("End"))

make_test(elts, pt_options("Testing results", "test", "p.m.c.harrison@qmul.ac.uk"))
