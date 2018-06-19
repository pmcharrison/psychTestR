library(psychTestR)

elts <- c(
  one_button_page("The next page will throw an error."),
  code_block(function(state, ...) 1 + as.integer),
  final_page("End")
)

# This test should put you into the browser when an error occurs
make_test(elts, opt = pt_options("Testing error logs",
                                 admin_password = "test",
                                 researcher_email = "p.m.c.harrison@qmul.ac.uk",
                                 debug_locally = TRUE))

# This test should display an error and log it
make_test(elts, opt = pt_options("Testing error logs",
                                 admin_password = "test",
                                 researcher_email = "p.m.c.harrison@qmul.ac.uk",
                                 debug_locally = FALSE,
                                 log_error = TRUE))
# Load and explore the error
load("output/errors/id=2&date=2018-05-08&time=21-43-45&tz=BST.rda") # for example
debugger(`output/errors/id=2&date=2018-05-08&time=21-43-45&tz=BST`) # for example

# Check what happens when you have two simultaneous sessions and one throws an error.
# Does the second one keep on working?

# This test should hide the detailed error message
make_test(elts, opt = pt_options("Testing error logs",
                                 admin_password = "test",
                                 researcher_email = "p.m.c.harrison@qmul.ac.uk",
                                 show_full_error_msg = FALSE))
