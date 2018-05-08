# library(psychTest)
# library(shiny)
#
# options(shiny.error = browser)
#
# show_x <- reactive_page(
#   function(state, ...) {
#     one_button_page(sprintf("Value of x is '%s'",
#                             format(get_local("x", state))))
#   }
# )
#
# elts <- c(
#   one_button_page("Welcome!"),
#   code_block(function(state, ...) {
#     set_local("x", "parent", state)
#   }),
#   one_button_page(
#     "We begin in the parent environment and define a variable 'x'."
#   ),
#   show_x,
#   begin_module(),
#   one_button_page(
#     "Now we move to the child environment. 'x' should now be invisible."
#   ),
#   show_x,
#   one_button_page(
#     "We can set the variable 'x' to 'child'."
#   ),
#   code_block(function(state, ...) {
#     set_local("x", "child", state)
#   }),
#   show_x,
#   one_button_page(
#     "Now let's go back to the parent environment."
#   ),
#   end_module(),
#   show_x,
#   final_page("OK, that's it!")
# )
#
# make_test(elts, "Module test", pt_options("banana"))
