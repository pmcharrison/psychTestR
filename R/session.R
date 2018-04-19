manage_sessions <- function(state,
                            options,
                            session = shiny::getDefaultReactiveDomain()) {
  stopifnot(is.scalar.character(options$session_dir))
  if (options$enable_resume_session) {
    # Runs once on session load
    shiny::isolate({
      p_id_url = shiny::parseQueryString(session$clientData$url_search)$p_id
      # If a URL p_id is provided, try to load that session...
      if (!is.null(p_id_url)) {
        loaded <- safe_load_session_data(p_id_url, options)
        # load_successful <- load_session(state, p_id_url, options)
        if (!is.null(loaded)) {
          shinyjs::runjs("confirm_resume_session();")
          update_state_from_list(state, loaded)
        } else {
          shinyjs::alert(paste0("Couldn't find this user's testing session.\n",
                                "Beginning a new session."))
          shinyjs::runjs("reset_p_id_and_refresh_browser();")
        }
      } else {
        # ... otherwise make a new p_id, if "auto" p_id mode is selected.
        if (options$auto_p_id) {
          p_id(state) <- get_new_p_id(options$session_dir)
          session$sendCustomMessage("push_p_id_to_url", p_id(state))
        }
      }
    })
    list(save_session(state, session_dir = options$session_dir),
         clean_session_dir(session = session, options = options))
  }
}

    # Once a non-zero p_id is registered, attempts to load any
    # session with the same p_id

      # shiny::observe(if (!is.null(p_id(state))) {
      #   p_id <- p_id(state)
      #   load_successful <- load_session(state, p_id, options$session_dir)
      #   if (load_successful) shinyjs::runjs("confirm_resume_session();")
      #   session$sendCustomMessage("push_p_id_to_url", p_id)
      # }),
# p_id <- start_session(state, session, session_dir = options$session_dir)
#   list(,
#        )
# }

reset_session <- function(state, retain_p_id = FALSE) {
  # Delete stored session file
  # Send call to javascript to clear URL and refresh page

  # Nope this doesn't work.
  # Instead... 'confirm resume session' should be a test page

  # Nope, this is too complicated.
  # instead, make the cancel button send a request to reset the state,
  # but with the p_id already known.
  p_id <- p_id(state)
  initialise_state(state)
  if (retain_p_id) p_id(state) <- p_id
  invisible(TRUE)
}

# start_session <- function(state, session, session_dir) {
#   shiny::isolate({
#     p_id = shiny::parseQueryString(session$clientData$url_search)$p_id
#     if (is.null(p_id)) {
#       p_id <- get_new_p_id(session_dir)
#     } else {
#       shinyjs::runjs("confirm_resume_session();")
#       load_successful <- load_session(state, p_id, session_dir)
#       if (!load_successful) p_id <- get_new_p_id(session_dir)
#     }
#   })
#   session$sendCustomMessage("push_p_id_to_url", p_id)
#   p_id
# }

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

save_session <- function(state, session_dir) {
  shiny::observe({
    stopifnot(is(state, "state"), is.scalar.character(session_dir))
    p_id <- p_id(state)
    if (!is.null(p_id)) {
      path.p_id <- file.path(session_dir, p_id)
      path.data <- file.path(path.p_id, "data.RDS")
      R.utils::mkdirs(path.p_id)
      saveRDS(shiny::reactiveValuesToList(state), path.data)
      save_timestamp(session_dir, p_id)
    }})
}

# can_load_session <- function(state, session_dir) {
#   file.exists(file.path(session_dir))
# }

load_session_data <- function(p_id, options) {
  path <- file.path(options$session_dir, p_id, "data.RDS")
  readRDS(path)
}

safe_load_session_data <- function(p_id, options) {
  res <- NULL
  tryCatch(res <- load_session_data(p_id, options),
           warning = function(w) NULL,
           error = function(e) NULL)
  res
}

# load_session <- function(state, p_id, session_dir) {
#   success <- FALSE
#   stopifnot(is(state, "state"), is.scalar.character(p_id),
#             is.scalar.character(session_dir))
#   tryCatch({
#     data <- load_session_data(p_id, session_dir)
#     update_state_from_list(state, data)
#     success <- TRUE
#   }, error = function(e) {
#     shinyjs::alert("Failed to load previous session.")
#   })
#   success
# }

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
