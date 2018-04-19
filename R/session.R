manage_sessions <- function(state,
                            options,
                            session = shiny::getDefaultReactiveDomain()) {
  if (options$enable_resume_session) {
    initialise_session(state, session, options)
    list(save_session(state, options = options),
         clean_session_dir(session = session, options = options))
  }
}

initialise_session <- function(state, session, options) {
  shiny::isolate({
    p_id_url <- get_p_id_from_url(session)
    if (!is.null(p_id_url)) {
      try_resume_session(p_id = p_id_url, state, session, options,
                         ask_to_confirm_resume = TRUE,
                         reset_if_resume_fails = TRUE)
    } else {
      if (options$auto_p_id) {
        p_id <- generate_new_p_id(options)
        p_id(state) <- p_id
        session$sendCustomMessage("push_p_id_to_url", p_id)
      }
    }
  })
}

get_p_id_from_url <- function(session) {
  shiny::parseQueryString(session$clientData$url_search)$p_id
}

push_p_id_to_url <- function(p_id, session) {
  session$sendCustomMessage("push_p_id_to_url", p_id)
}

try_resume_session <- function(p_id, state, session, options,
                               ask_to_confirm_resume,
                               reset_if_resume_fails) {
  stopifnot(is.scalar.character(p_id),
            is.scalar.logical(ask_to_confirm_resume),
            is.scalar.logical(reset_if_resume_fails))
  p_id(state) <- p_id
  push_p_id_to_url(p_id, session)
  data <- safe_load_session(p_id, options)
  success <- !is.null(data)
  if (success) {
    if (ask_to_confirm_resume) {
      shinyjs::runjs("confirm_resume_session();")
    } else {
      shiny::showNotification("Resuming previous session.")
    }
    update_state_from_list(state, data)
    increment_num_restarts(state)
  } else {
    if (reset_if_resume_fails) {
      shinyjs::alert(paste0("Couldn't find this user's testing session.\n",
                            "Beginning a new session."))
      shinyjs::runjs("reset_p_id_and_refresh_browser();")
    } else {
      shiny::showNotification("Starting new session.")
    }
  }
}

generate_new_p_id <- function(options) {
  p_id <- NA
  R.utils::mkdirs(options$session_dir)
  while (is.na(p_id)) {
    candidate <- shiny:::createUniqueId(32)
    is_unique <- dir.create(file.path(options$session_dir, candidate),
                            showWarnings = FALSE)
    if (is_unique) {
      p_id <- candidate
      save_timestamp(options, p_id)
    }
  }
  p_id
}

save_session <- function(state, options) {
  shiny::observe({
    stopifnot(is(state, "state"), is.scalar.character(options$session_dir))
    p_id <- p_id(state)
    if (!is.null(p_id)) {
      path.p_id <- file.path(options$session_dir, p_id)
      path.data <- file.path(path.p_id, "data.RDS")
      R.utils::mkdirs(path.p_id)
      saveRDS(shiny::reactiveValuesToList(state), path.data)
      save_timestamp(options, p_id)
    }})
}

load_session <- function(p_id, options) {
  path <- file.path(options$session_dir, p_id, "data.RDS")
  readRDS(path)
}

safe_load_session <- function(p_id, options) {
  res <- NULL
  tryCatch(res <- load_session(p_id, options),
           warning = function(w) NULL,
           error = function(e) NULL)
  res
}

save_timestamp <- function(options, p_id) {
  stopifnot(is.scalar.character(p_id), is.scalar.character(options$session_dir))
  saveRDS(Sys.time(), file.path(options$session_dir, p_id, "timestamp.RDS"))
}

#' Returns NA if no timestamp found
read_timestamp <- function(options, p_id) {
  stopifnot(is.scalar.character(p_id), is.scalar.character(options$session_dir))
  path <- file.path(options$session_dir, p_id, "timestamp.RDS")
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
      time <- read_timestamp(options, p_id)
      if (!is.na(time) && time < Sys.time() - options$session_timeout_min * 60) {
        unlink(dir, recursive = TRUE)
      }}})}
