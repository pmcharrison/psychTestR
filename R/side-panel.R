#' #' @export
#' new_side_panel <- function(password = "sleepfuriously",
#'                            render_ui = NULL,
#'                            render_outputs = NULL,
#'                            render_modals = NULL,
#'                            observe_events = NULL) {
#'   stopifnot(is.scalar.character(password))
#'   for (f in c("render_ui", "render_outputs", "render_modals", "observe_events")) {
#'     if (is.null(get(f))) assign(f, function(state, input, output, session) NULL)
#'   }
#'   for (f in list(render_ui, render_outputs, render_modals, observe_events)) {
#'     if(!identical(names(formals(f)), c("state", "input", "output", "session"))) {
#'       stop("Arguments `render_ui`, `render_outputs`, `render_modals`, and ",
#'            "`observe_events` should be either `NULL` or functions ",
#'            "with the arguments `state`, `input`, `output`, and `session`, ",
#'            "in that order.")
#'     }
#'   }
#'   x <- list(password = password,
#'             render_ui = render_ui,
#'             render_outputs = render_outputs,
#'             render_modals = render_modals,
#'             observe_events = observe_events)
#'   class(x) <- "side_panel"
#'   x
#' }
#'
#' side_panel_server <- function(side_panel, state, input, output, session) {
#'   stopifnot(is(side_panel, "side_panel"))
#'   for (f in c("render_ui", "render_outputs", "render_modals", "observe_events")) {
#'     side_panel[[f]](state, input, output, session)
#'   }
#' }
