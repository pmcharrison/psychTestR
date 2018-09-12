manage_sessions <- function(state,
                            opt,
                            session = shiny::getDefaultReactiveDomain()) {
  if (opt$enable_resume_session) {
    initialise_session(state, session, opt)
    list(save_session(state, opt = opt),
         clean_session_dir(session = session, opt = opt))
  }
}

initialise_session <- function(state, session, opt) {
  shiny::isolate({
    url_params(state) <- get_url_params_from_browser(session)
    language <- get_url_params(state)$language
    if (is.null(language) || !language %in% opt$languages) {
      language <- opt$languages[1]
      set_url_param("language", language, session, state)
    }

    language(state) <- language
    p_id_url <- get_url_params(state)$p_id
    if (!is.null(p_id_url)) {
      if (is_p_id_valid(p_id_url)){
        try_resume_session(p_id = p_id_url, state, session, opt,
                           ask_to_confirm_resume = TRUE,
                           reset_if_resume_fails = !opt$allow_any_p_id_url)
      } else {
        error(state) <- describe_valid_p_id()
        allow_session_saving(state) <- FALSE
      }
    } else {
      if (opt$force_p_id_from_url) {
        error(state) <- "p_id parameter must be provided through the URL."
        allow_session_saving(state) <- FALSE
      } else if (opt$auto_p_id) {
        p_id <- generate_new_p_id(opt)
        p_id(state) <- p_id
        set_url_param("p_id", p_id, session, state)
        # session$sendCustomMessage("push_p_id_to_url", p_id)
      }
    }
  })
}

get_url_params_from_browser <- function(session) {
  shiny::parseQueryString(session$clientData$url_search)
}

# get_p_id_from_url <- function(session) {
#   shiny::parseQueryString(session$clientData$url_search)$p_id
# }

#' Set a URL parameter
#'
#' Sets a URL parameter.
#' @param key URL parameter key.
#' @param value URL parameter value.
#' @param session The participant's \code{session} object.
#' @param state The participant's \code{state} object.
#' @export
set_url_param <- function(key, value, session, state) {
  stopifnot(is.scalar(value), is.scalar.character(key))
  # if (key == "p_id") stop("p_id is a protected URL parameter")
  # params <- shiny::parseQueryString(session$clientData$url_search)
  params <- get_url_params(state)
  params[[key]] <- as.character(value)
  set_url_params(params, session, state)
}

set_url_params <- function(params, session, state) {
  stopifnot(is.list(params))
  keys <- vapply(names(params), function(x) utils::URLencode(x, reserved = TRUE,
                                                             repeated = TRUE),
                 character(1))
  values <- vapply(params,
                   function(x) utils::URLencode(x, reserved = TRUE,
                                                repeated = TRUE),
                   character(1))
  str <- paste0("?", paste(paste(keys, values, sep = "="),
                           collapse = "&"))
  shiny::updateQueryString(str, mode = "replace", session = session)
  url_params(state) <- params
}

# push_p_id_to_url <- function(p_id, session) {
#   session$sendCustomMessage("push_p_id_to_url", p_id)
# }

try_resume_session <- function(p_id, state, session, opt,
                               ask_to_confirm_resume,
                               reset_if_resume_fails) {
  stopifnot(is.scalar.character(p_id),
            is.scalar.logical(ask_to_confirm_resume),
            is.scalar.logical(reset_if_resume_fails))
  p_id(state) <- p_id
  set_url_param(key = "p_id", value = p_id, session, state)
  data <- safe_load_session(p_id, opt)
  success <- !is.null(data)
  if (success) {
    if (ask_to_confirm_resume) {
      shinyjs::runjs("confirm_resume_session();")
    } else {
      shiny::showNotification("Resuming previous session.")
    }
    update_state_from_list(state, data)
    set_url_params(get_url_params(state), session, state)
    increment_num_restarts(state)
    if (!is_test_closed(opt)) closed(state) <- FALSE
  } else {
    if (reset_if_resume_fails) {
      shinyjs::alert(paste0("Couldn't find this user's testing session.\n",
                            "Beginning a new session."))
      shinyjs::runjs("reset_p_id_and_refresh_browser();")
      allow_session_saving(state) <- FALSE
    } else {
      shiny::showNotification("Starting new session.")
    }
  }
}

generate_new_p_id <- function(opt) {
  p_id <- NA
  R.utils::mkdirs(opt$session_dir)
  while (is.na(p_id)) {
    candidate <- generate_id(32)
    is_unique <- dir.create(file.path(opt$session_dir, candidate),
                            showWarnings = FALSE)
    if (is_unique) {
      p_id <- candidate
      save_timestamp(opt, p_id)
    }
  }
  p_id
}

save_session <- function(state, opt) {
  shiny::observe({
    stopifnot(is(state, "state"), is.scalar.character(opt$session_dir))
    p_id <- p_id(state)
    if (!is.null(p_id) && allow_session_saving(state)) {
      path.p_id <- file.path(opt$session_dir, p_id)
      path.data <- file.path(path.p_id, "data.RDS")
      R.utils::mkdirs(path.p_id)
      saveRDS(shiny::reactiveValuesToList(state), path.data)
      save_timestamp(opt, p_id)
    }})
}

load_session <- function(p_id, opt) {
  path <- file.path(opt$session_dir, p_id, "data.RDS")
  readRDS(path)
}

safe_load_session <- function(p_id, opt) {
  res <- NULL
  tryCatch(res <- load_session(p_id, opt),
           warning = function(w) NULL,
           error = function(e) NULL)
  res
}

save_timestamp <- function(opt, p_id) {
  stopifnot(is.scalar.character(p_id), is.scalar.character(opt$session_dir))
  saveRDS(Sys.time(), file.path(opt$session_dir, p_id, "timestamp.RDS"))
}

# Returns NA if no timestamp found
read_timestamp <- function(opt, p_id) {
  stopifnot(is.scalar.character(p_id), is.scalar.character(opt$session_dir))
  path <- file.path(opt$session_dir, p_id, "timestamp.RDS")
  if (file.exists(path)) readRDS(path) else NA
}

# Deletes all session caches that are older than a certain amount.
clean_session_dir <- function(session, opt) {
  stopifnot(is.scalar.numeric(opt$session_timeout_min),
            is.scalar.numeric(opt$clean_sessions_interval_min),
            is.scalar.character(opt$session_dir))
  millis <- opt$clean_sessions_interval_min * 60 * 1000
  shiny::observe({
    shiny::invalidateLater(millis, session)
    dirs <- list.dirs(opt$session_dir, recursive = FALSE)
    for (dir in dirs) {
      p_id <- basename(dir)
      time <- read_timestamp(opt, p_id)
      if (!is.na(time) && time < Sys.time() - opt$session_timeout_min * 60) {
        unlink(dir, recursive = TRUE)
      }}})}
