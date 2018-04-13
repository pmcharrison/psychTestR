#' @export
psychTest_options <- function(session_dir = "/var/tmp/piat",
                              theme = shinythemes::shinytheme("readable"),
                              session_timeout_min = 120,
                              clean_sessions_interval_min = 15) {
  list(session_dir = session_dir,
       theme = theme,
       session_timeout_min = session_timeout_min,
       clean_sessions_interval_min = clean_sessions_interval_min)
}
