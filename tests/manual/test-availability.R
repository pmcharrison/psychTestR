library(psychTestR)
make_item <- function(n) {
  NAFC_page(label = paste0("q", n),
            prompt = paste0("Question ", n),
            choices = as.character(1:10),
            arrange_vertically = FALSE)
}

elts <- c(lapply(1:4, make_item),
          final_page("End"))

# Try starting the test and then closing the test.
# Can you keep on with the same session?
# What if you refresh the page?
# Now try starting as a new user.
# What if the user started when it was closed, then refreshed?

make_test(elts, pt_options("Testing results", admin_password = "test", "p.m.c.harrison@qmul.ac.uk"))
