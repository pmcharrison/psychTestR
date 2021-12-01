library(psychTestR)

# test 1

make_test(
  elts = join(
    one_button_page("hi"),
    one_button_page("hi2"),
    final_page("end")
  ),
  opt = test_options(title = "test additional_scripts",
                     admin_password = "demo",
                     additional_scripts = "js/hello_world.js")
)

# test 2

make_test(
  elts = join(
    one_button_page("hi"),
    one_button_page("hi2"),
    final_page("end")
  ),
  opt = test_options(title = "test additional_scripts",
                     admin_password = "demo",
                     additional_scripts = c("js/hello_world.js",
                                            "js/hello_world2.js"))
)

