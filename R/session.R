manage_sessions <- function(state,
                            options,
                            session = shiny::getDefaultReactiveDomain()) {
  stopifnot(is.scalar.character(options$session_dir))
  if (options$enable_resume_session) {
    ssuid <- start_session(state, session, session_dir = options$session_dir)
    list(save_session(ssuid, state, session_dir = options$session_dir),
         clean_session_dir(session = session, options = options))
  }
}

start_session <- function(state, session, session_dir) {
  shiny::isolate({
    ssuid = shiny::parseQueryString(session$clientData$url_search)$SSUID
    if (is.null(ssuid)) {
      ssuid <- get_new_ssuid(session_dir)
    } else {
      shinyjs::runjs("confirm_resume_session();")
      load_successful <- load_session(state, ssuid, session_dir)
      if (!load_successful) ssuid <- get_new_ssuid(session_dir)
    }
  })
  session$sendCustomMessage("session_start", ssuid)
  ssuid
}

get_new_ssuid <- function(session_dir) {
  ssuid <- NA
  R.utils::mkdirs(session_dir)
  while (is.na(ssuid)) {
    candidate <- shiny:::createUniqueId(16)
    is_unique <- dir.create(file.path(session_dir, candidate),
                            showWarnings = FALSE)
    if (is_unique) {
      ssuid <- candidate
      save_timestamp(session_dir, ssuid)
    }
  }
  ssuid
}

save_session <- function(ssuid, state, session_dir) {
  stopifnot(is(state, "state"), is.scalar.character(ssuid),
            is.scalar.character(session_dir))
  path.ssuid <- file.path(session_dir, ssuid)
  path.data <- file.path(path.ssuid, "data.RDS")
  shiny::observe({
    R.utils::mkdirs(path.ssuid)
    saveRDS(reactiveValuesToList(state), path.data)
    save_timestamp(session_dir, ssuid)
  })
}

load_session <- function(state, ssuid, session_dir) {
  success <- FALSE
  stopifnot(is(state, "state"), is.scalar.character(ssuid),
            is.scalar.character(session_dir))
  path <- file.path(session_dir, ssuid, "data.RDS")
  tryCatch({
    update_state_from_list(state, readRDS(path))
    success <- TRUE
  }, error = function(e) {
    shinyjs::alert("Failed to load previous session.")
  })
  success
}

save_timestamp <- function(session_dir, ssuid) {
  stopifnot(is.scalar.character(ssuid), is.scalar.character(session_dir))
  saveRDS(Sys.time(), file.path(session_dir, ssuid, "timestamp.RDS"))
}

#' Returns NA if no timestamp found
read_timestamp <- function(session_dir, ssuid) {
  stopifnot(is.scalar.character(ssuid), is.scalar.character(session_dir))
  path <- file.path(session_dir, ssuid, "timestamp.RDS")
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
      ssuid <- basename(dir)
      time <- read_timestamp(options$session_dir, ssuid)
      if (!is.na(time) && time < Sys.time() - options$session_timeout_min * 60) {
        unlink(dir, recursive = TRUE)
      }}})}
