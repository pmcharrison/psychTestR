library(psychTestR)

elts <- join(
  code_block(function(state, ...) {
    set_global("loop", TRUE, state)
  }),
  while_loop(
    test = function(state, ...) get_global("loop", state),
    logic = NAFC_page("continue", "Keep looping?", choices = c("Yes", "No"),
                      on_complete = function(state, answer, ...) {
                        set_global("loop", answer == "Yes", state = state)
                      })

  ),
  final_page("End.")
)

test <- make_test(elts, opt = demo_options(enable_resume_session = FALSE,
                                           advance_delay = 0))
test
