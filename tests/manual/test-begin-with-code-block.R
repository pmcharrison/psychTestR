library(psychTest)

elts <- c(
  code_block(function(state, ...) message("Hello!")),
  one_button_page("Please click next."),
  final_page("End")
)

make_test(elts, opt = pt_options("Testing error logs",
                                 admin_password = "test",
                                 researcher_email = "p.m.c.harrison@qmul.ac.uk",
                                 show_full_error_msg = FALSE))
