# This test has its session cleaning interval set very low so that
# you can see it in action.
# Launch the test, and keep an eye on the output/sessions directory.
# New session files should be generated as you progress to each page,
# but after you wait 2.5 seconds you should see the session file deleted.

elts <- c(lapply(paste("Page", 1:50), one_button_page),
          final_page("Done"))

make_test(elts, pt_options("Testing test elements", "test", "p.m.c.harrison@qmul.ac.uk", debug_locally = TRUE,
                           clean_sessions_interval_min = 0.1 / 60, session_timeout_min = 2.5 / 60))
