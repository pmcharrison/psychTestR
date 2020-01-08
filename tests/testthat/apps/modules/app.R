library(psychTestR)

elts <- join(
  one_button_page("We begin in the global environment."),
  module(
    "parent",

    one_button_page("We've now entered the parent environment."),
    code_block(function(state, ...) {
      x <- 42
      set_local("x", x, state)
      save_result(state, "x", x)
    }),
    one_button_page("In the parent environment, we define a local variable, x = 42."),

    module(
      "child",
      one_button_page("Now we enter the child environment."),
      one_button_page("We can't see x any more: x is now NULL."),
      code_block(function(state, ...) {
        x <- 65
        set_local("x", x, state)
        save_result(state, "x", x)
      }),
      one_button_page("We can set it to a new value, though: x = 65.")
    ),

    one_button_page("Now we return to the parent environment."),
    one_button_page("We see that x = 42 again.")
  ),

  one_button_page("Now we return to the global environment."),
  final_page("We see that x is is NULL again.")
)

make_test(elts)
