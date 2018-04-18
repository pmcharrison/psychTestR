#' @export
psychTest_options <- function(theme = shinythemes::shinytheme("readable"),
                              enable_resume_session = TRUE,
                              session_dir = NULL,
                              session_timeout_min = 120,
                              clean_sessions_interval_min = 15) {
  stopifnot(is.scalar.numeric(session_timeout_min),
            is.scalar.numeric(clean_sessions_interval_min),
            is.scalar.logical(enable_resume_session))
  if (is.null(session_dir)) session_dir <- get_default_session_dir()

  list(theme = theme,
       enable_resume_session = enable_resume_session,
       session_dir = session_dir,
       session_timeout_min = session_timeout_min,
       clean_sessions_interval_min = clean_sessions_interval_min)
}

default_session_dirs <- c("shiny-sessions", "/var/tmp/shiny-sessions")

get_default_session_dir <- function() {
  for (dir in default_session_dirs) {
    R.utils::mkdirs(dir)
    if (test_permissions(dir)) return(dir)
  }
  tempdir()
}

#' @export
delete_default_session_dirs <- function() {
  for (dir in default_session_dirs) {
    unlink(dir, recursive = TRUE)
  }
}

#' @export
test_permissions <- function(dir) {
  success <- FALSE
  tmp <- file.path(dir, "test.txt")
  val <- sample(1e9, 1)
  tryCatch({
    if (file.exists(tmp)) file.remove(tmp)
    write(val, tmp)
    stopifnot(all.equal(val, as.numeric(readLines(tmp))))
    stopifnot(file.remove(tmp))
    success <- TRUE
  }, error = function(e) NULL, warning = function(w) NULL)
  success
}
