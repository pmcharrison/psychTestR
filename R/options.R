#' @export
pt_options <- function(title, admin_password, researcher_email,
                       max_num_participants = NULL,
                       demo = FALSE,
                       debug_locally = FALSE, #####
                       log_error = TRUE,
                       show_full_error_msg = TRUE,
                       notify_error = FALSE, #####
                       notify_new_participant = FALSE,
                       pushbullet_email = NULL,
                       pushbullet_apikey = NULL,
                       max_participants_msg = NULL,
                       server_closed_msg = NULL,
                       problems_info = NULL,
                       theme = shinythemes::shinytheme("readable"),
                       auto_p_id = TRUE,
                       enable_resume_session = TRUE,
                       enable_admin_panel = TRUE,
                       output_dir = "output",
                       session_timeout_min = 120,
                       clean_sessions_interval_min = 15) {
  stopifnot(is.scalar.character(title),
            is.scalar.character(admin_password),
            is.scalar.character(researcher_email),
            is.scalar.logical(debug_locally),
            is.scalar.logical(log_error),
            is.scalar.logical(show_full_error_msg),
            is.scalar.logical(demo),
            is.scalar.logical(notify_error),
            is.scalar.logical(notify_new_participant),
            is.null.or(pushbullet_email, is.scalar.character),
            is.null.or(pushbullet_apikey, is.scalar.character),
            is.scalar.numeric(session_timeout_min),
            is.scalar.numeric(clean_sessions_interval_min),
            is.scalar.logical(enable_resume_session),
            is.scalar.logical(enable_admin_panel),
            is.scalar.logical(auto_p_id),
            is.scalar.character(output_dir),
            is.null.or(max_num_participants, is.scalar.integerlike),
            is.null.or(max_participants_msg, is.scalar.character),
            is.null.or(server_closed_msg, is.scalar.character),
            is.null.or(problems_info, is.scalar.character))
  # if (is.null(session_dir)) session_dir <- get_default_session_dir()

  if ((notify_new_participant || notify_error) &&
      (is.null(pushbullet_email) || is.null(pushbullet_apikey))) stop(
        "if notify_error or notify_new_participant is TRUE, ",
        "both pushbullet_email and pushbullet_apikey must be provided.")

  if (is.null(max_participants_msg)) {
    max_participants_msg <- paste0(
      "Thank you for your interest in this test. ",
      "Unfortunately the participant quota has now been reached, ",
      "so testing has now finished.")
  }

  if (is.null(problems_info)) {
    problems_info <- paste0(
      "Problems? Contact ", researcher_email, " with a link to this page.")
  }

  if (is.null(server_closed_msg)) {
    server_closed_msg <- paste0(
      "Thank you for your interest in this test. ",
      "Unfortunately, however, participation is now closed.")
  }

  results_dir <- file.path(output_dir, "results")
  session_dir <- file.path(output_dir, "sessions")
  results_archive_dir <- file.path(output_dir, "deleted-results")
  error_dir <- file.path(output_dir, "errors")

  list(title = title,
       admin_password = admin_password,
       researcher_email = researcher_email,
       demo = demo,
       debug_locally = debug_locally,
       log_error = log_error,
       show_full_error_msg = show_full_error_msg,
       notify_error = notify_error,
       notify_new_participant = notify_new_participant,
       pushbullet = list(email = pushbullet_email,
                         apikey = pushbullet_apikey),
       max_num_participants = max_num_participants,
       max_participants_msg = max_participants_msg,
       server_closed_msg = server_closed_msg,
       problems_info = problems_info,
       theme = theme,
       auto_p_id = auto_p_id,
       enable_resume_session = enable_resume_session,
       enable_admin_panel = enable_admin_panel,
       output_dir = output_dir,
       results_dir = results_dir,
       session_dir = session_dir,
       results_archive_dir = results_archive_dir,
       error_dir = error_dir,
       session_timeout_min = session_timeout_min,
       clean_sessions_interval_min = clean_sessions_interval_min)
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

#' @export
check_dirs <- function(opt) {
  dirs <- c("output_dir", "results_dir", "session_dir",
            "results_archive_dir", "error_dir")
  for (d in dirs) {
    dir <- opt[[d]]
    stopifnot(is.scalar.character(dir))
    R.utils::mkdirs(dir)
    if (!test_permissions(dir)) {
      stop("Insufficient permissions to write to directory ",
           dir, ". Perhaps try 'chown -R <username> <dir>'",
           "at the terminal.")
    }
  }
}
