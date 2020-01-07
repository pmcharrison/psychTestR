library(psychTestR)

x <- list(
  one_button_page("Welcome."),
  code_block(function(state, ...) {
    set_local("counter", 1L, state)
  }),
  while_loop(
    test = function(state, ...) get_local(state = state, key = "counter") < 4L,
    new_timeline(x = list(reactive_page(function(state, ...) {
      one_button_page(paste("Page", get_local("counter", state)),
                      on_complete = function(state, ...)
                        set_local("counter",
                                  get_local("counter", state) + 1,
                                  state))
    })))
  ),
  final_page("Demo complete.")
)

make_test(x)
