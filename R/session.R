manage_sessions <- function(state,
                            options,
                            session = shiny::getDefaultReactiveDomain()) {
  stopifnot(is.scalar.character(options$session_dir))
  if (options$enable_resume_session) {
    p_id <- start_session(state, session, session_dir = options$session_dir)
    list(save_session(p_id, state, session_dir = options$session_dir),
         clean_session_dir(session = session, options = options))
  }
}

start_session <- function(state, session, session_dir) {
  shiny::isolate({
    p_id = shiny::parseQueryString(session$clientData$url_search)$p_id
    if (is.null(p_id)) {
      p_id <- get_new_p_id(session_dir)
    } else {
      shinyjs::runjs("confirm_resume_session();")
      load_successful <- load_session(state, p_id, session_dir)
      if (!load_successful) p_id <- get_new_p_id(session_dir)
    }
  })
  session$sendCustomMessage("session_start", p_id)
  p_id
}

get_new_p_id <- function(session_dir) {
  p_id <- NA
  R.utils::mkdirs(session_dir)
  while (is.na(p_id)) {
    candidate <- shiny:::createUniqueId(32)
    is_unique <- dir.create(file.path(session_dir, candidate),
                            showWarnings = FALSE)
    if (is_unique) {
      p_id <- candidate
      save_timestamp(session_dir, p_id)
    }
  }
  p_id
}

save_session <- function(p_id, state, session_dir) {
  stopifnot(is(state, "state"), is.scalar.character(p_id),
            is.scalar.character(session_dir))
  path.p_id <- file.path(session_dir, p_id)
  path.data <- file.path(path.p_id, "data.RDS")
  shiny::observe({
    R.utils::mkdirs(path.p_id)
    saveRDS(shiny::reactiveValuesToList(state), path.data)
    save_timestamp(session_dir, p_id)
  })
}

load_session <- function(state, p_id, session_dir) {
  success <- FALSE
  stopifnot(is(state, "state"), is.scalar.character(p_id),
            is.scalar.character(session_dir))
  path <- file.path(session_dir, p_id, "data.RDS")
  tryCatch({
    update_state_from_list(state, readRDS(path))
    success <- TRUE
  }, error = function(e) {
    shinyjs::alert("Failed to load previous session.")
  })
  success
}

save_timestamp <- function(session_dir, p_id) {
  stopifnot(is.scalar.character(p_id), is.scalar.character(session_dir))
  saveRDS(Sys.time(), file.path(session_dir, p_id, "timestamp.RDS"))
}

#' Returns NA if no timestamp found
read_timestamp <- function(session_dir, p_id) {
  stopifnot(is.scalar.character(p_id), is.scalar.character(session_dir))
  path <- file.path(session_dir, p_id, "timestamp.RDS")
  if (file.exists(path)) readRDS(path) else NA
}

#' Deletes all session caches that are older than a certain amount.
clean_session_dir <- function(session, options) {
  stopifnot(is.scalar.numeric(options$session_timeout_min),
            is.scalar.numeric(options$clean_sessions_interval_min),
            is.scalar.character(options$session_dir))
  millis <- options$clean_sessions_interval_min * 60 * 1000
  shiny::observe({
    shiny::invalidateLater(millis, session)
    dirs <- list.dirs(options$session_dir, recursive = FALSE)
    for (dir in dirs) {
      p_id <- basename(dir)
      time <- read_timestamp(options$session_dir, p_id)
      if (!is.na(time) && time < Sys.time() - options$session_timeout_min * 60) {
        unlink(dir, recursive = TRUE)
      }}})}
