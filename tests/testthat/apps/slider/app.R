library(psychTestR)

elts <- join(
  slider_page("slider",
              "Try this slider:",
              min = 0,
              max = 100,
              value = 50,
              on_complete = function(state, ...) set_local("x", 42, state)),
  reactive_page(function(answer, ...) {
    final_page(sprintf("You gave a value of %i.", answer))
  })
)

make_test(elts)
