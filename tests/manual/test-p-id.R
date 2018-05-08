elts <- c(one_button_page("First page"),
          one_button_page("Page 2/4"),
          one_button_page("Page 3/4"),
          final_page("The end of the test"))

# This test autogenerates your participant ID and puts it in the URL.
# 1. Try and give it a fake participant ID.
# 2. Step forward a couple of pages, then try refreshing the page. Do you resume the session?
# 3. Now, partway through the test, use the admin panel (password test) and delete sessions. Can you still resume the sesion?
make_test(elts, pt_options("Testing test elements", "test", "p.m.c.harrison@qmul.ac.uk", debug_locally = TRUE,
                           allow_any_p_id_url = FALSE))

# This test should force you to provide a p_id via URL.
make_test(elts, pt_options("Testing test elements", "test", "p.m.c.harrison@qmul.ac.uk", debug_locally = TRUE,
                           allow_any_p_id_url = TRUE, force_p_id_from_url = TRUE))

# This test should give you a temporary p_id that updates once you enter in your p_id in the test.
make_test(elts = c(get_p_id(),
                   final_page("End of the test")),
          pt_options("Testing test elements", "test", "p.m.c.harrison@qmul.ac.uk", debug_locally = TRUE))

# This test should not give you a p_id until you enter it yourself.
make_test(elts = c(get_p_id(),
                   final_page("End of the test")),
          pt_options("Testing test elements", "test", "p.m.c.harrison@qmul.ac.uk", debug_locally = TRUE,
                     auto_p_id = FALSE))
