#' Alias for test_options()
#'
#' Alias for \code{test_options()} (deprecated).
#' @param ... Arguments to pass to \code{\link{test_options}()}.
#' @export
pt_options <- function(...) {
  do.call(test_options, list(...))
}


#' Test options
#'
#' Defines the options for running a given test.
#' @param title
#' The test's title.
#' This can either be an unnamed character scalar,
#' in which case the same title will be shown irrespective of internationalisation,
#' or a named vector of titles with the names
#' corresponding to language codes.
#' @param admin_password Password to access the admin panel.
#' @param researcher_email Researcher's email; used in participant help message.
#' @param max_num_participants Maximum number of participant completes
#' before the test automatically closes (excludes pilot participants
#' as indicated through the admin panel).
#' @param demo Whether the test is to be run in demo mode.
#' @param languages Character vector of languages that may be selected
#' via the URL parameter 'language'. If no language is provided by
#' the URL parameter, defaults to the first language in this vector.
#' Languages should be encoded according to ISO 639-2 conventions.
#' @param log_error Whether to log errors
#' (this feature is under development).
#' @param show_full_error_msg Whether to show full error messages.
#' @param notify_error Whether to send a Pushbullet message when
#' the test experiences an error
#' (this feature is under development).
#' @param notify_new_participant Whether to send a Pushbullet message when
#' a new participant completes the test.
#' @param pushbullet_email Email to send Pushbullet notifications to.
#' @param pushbullet_apikey Pushbullet API key (see https://www.pushbullet.com/).
#' @param max_participants_msg Message to display when the participant
#' quota is reached
#' (\code{NULL} gives default).
#' @param server_closed_msg Message to display when the server is closed
#' via the admin panel
#' (\code{NULL} gives default).
#' @param problems_info
#' Message to display at the bottom of the screen
#' with advice about what to do if a problem occurs.
#' The default value, "default", gives
#' a standard English message including the researcher's email (if provided).
#' Alternatively, the argument can be an unnamed character scalar
#' providing an non-internationalised message,
#' or a named vector of internationalised messages with the names
#' corresponding to language codes.
#' @param theme Shiny theme: see e.g. the \code{shinythemes} package.
#' @param auto_p_id Whether or not to automatically generate an
#' ID for each participant.
#' @param enable_resume_session Whether to allow participants to resume
#' sessions by refreshing the page.
#' @param allow_any_p_id_url Whether to allow new participants
#' to give themselves an arbitrary participant ID by passing it
#' as a URL parameter.
#' @param force_p_id_from_url Whether to force new participants
#' to provide their participant ID as a URL parameter.
#' @param enable_admin_panel Wheter to enable the admin panel.
#' @param output_dir String identifying psychTestR's output directory.
#' @param session_timeout_min Minimum time until a participant's
#' session times out (minutes).
#' @param clean_sessions_interval_min How often should psychTestR
#' check for expired participant sessions (minutes).
#' @param logo Path to a logo to display in the header (optional).
#' @param logo_width Logo width, e.g. \code{"100px"}.
#' @param logo_height Logo height, e.g. \code{"50px"}.
#' @export
test_options <- function(title, admin_password,
                         researcher_email = NULL,
                         max_num_participants = NULL,
                         demo = FALSE,
                         languages = "EN",
                         log_error = TRUE,
                         show_full_error_msg = TRUE,
                         notify_error = FALSE,
                         notify_new_participant = FALSE,
                         pushbullet_email = NULL,
                         pushbullet_apikey = NULL,
                         max_participants_msg = NULL,
                         server_closed_msg = NULL,
                         problems_info = "default",
                         theme = shinythemes::shinytheme("yeti"),
                         auto_p_id = TRUE,
                         enable_resume_session = TRUE,
                         allow_any_p_id_url = FALSE,
                         force_p_id_from_url = FALSE,
                         enable_admin_panel = TRUE,
                         output_dir = "output",
                         session_timeout_min = 7 * 24 * 60,
                         clean_sessions_interval_min = 15,
                         logo = NULL,
                         logo_width = NULL,
                         logo_height = NULL,
                         display = display_options()) {
  stopifnot(is.character(title),
            is.scalar.character(admin_password),
            is.null.or(researcher_email, is.scalar.character),
            is.character(languages),
            length(languages) > 0,
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
            is.scalar.logical(allow_any_p_id_url),
            is.scalar.logical(force_p_id_from_url),
            is.scalar.logical(enable_admin_panel),
            is.scalar.logical(auto_p_id),
            is.scalar.character(output_dir),
            is.null.or(max_num_participants, is.scalar.integerlike),
            is.null.or(max_participants_msg, is.scalar.character),
            is.null.or(server_closed_msg, is.scalar.character),
            is.character(problems_info),
            is.null.or(logo, is.scalar.character),
            is.null(logo) ||
              (is.scalar.character(logo_width) && is.scalar.character(logo_height)),
            is.list(display))
  # if (is.null(session_dir)) session_dir <- get_default_session_dir()

  title <- iconv(enc2utf8(title), "UTF-8", "UTF-8", sub = "")
  if (any(nchar(title) > 100L))
    stop("maximum title length is 100 characters")

  if (length(title) > 1 && is.null(names(title)))
    stop("if option 'title' has length > 1, it must be named")
  if (!is.null(names(title)) && !all(languages %in% names(title)))
    stop("titles must be provided for all supported languages")


  if (force_p_id_from_url && !allow_any_p_id_url)
    stop("if force_p_id_from_url is TRUE then allow_any_p_id_url must be TRUE")

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

  if (length(problems_info) == 1 && problems_info == "default") {
    problems_info <- if (is.null(researcher_email)) "" else paste0(
      "Problems? Contact ", researcher_email, " with a link to this page.")
  } else {
    problems_info <- iconv(enc2utf8(problems_info), "UTF-8", "UTF-8", sub = "")
  }

  if (length(problems_info) > 1 && is.null(names(problems_info)))
    stop("if option 'problems_info' has length > 1, it must be named")

  if (!is.null(names(problems_info)) && !all(languages %in% names(problems_info)))
    stop("problem info texts must be provided for all supported languages")

  if (is.null(server_closed_msg)) {
    server_closed_msg <- paste0(
      "Thank you for your interest in this test. ",
      "Unfortunately, however, participation is now closed.")
  }
  server_closed_msg <- enc2utf8(server_closed_msg)

  results_dir <- file.path(output_dir, "results")
  supplementary_results_dir <- file.path(results_dir, "supplementary")
  session_dir <- file.path(output_dir, "sessions")
  results_archive_dir <- file.path(output_dir, "deleted-results")
  error_dir <- file.path(output_dir, "errors")

  list(title = title,
       admin_password = admin_password,
       researcher_email = researcher_email,
       demo = demo,
       languages = languages,
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
       allow_any_p_id_url = allow_any_p_id_url,
       force_p_id_from_url = force_p_id_from_url,
       enable_admin_panel = enable_admin_panel,
       output_dir = output_dir,
       results_dir = results_dir,
       supplementary_results_dir = supplementary_results_dir,
       session_dir = session_dir,
       results_archive_dir = results_archive_dir,
       error_dir = error_dir,
       closed_file = file.path(output_dir, "closed.txt"),
       session_timeout_min = session_timeout_min,
       clean_sessions_interval_min = clean_sessions_interval_min,
       logo = logo,
       logo_width = logo_width,
       logo_height = logo_height,
       display = display)
}

#' @export
display_options <- function(
  full_screen = FALSE,
  content_background_colour = "white",
  content_border_width = "1px",
  show_header = TRUE,
  show_footer = TRUE,
  left_margin = 2L,
  right_margin = 2L,
  css = character()
) {
  checkmate::qassert(show_header, "B1")
  checkmate::qassert(show_footer, "B1")
  checkmate::qassert(left_margin, "X1[0,12]")
  checkmate::qassert(right_margin, "X1[0,12]")
  checkmate::qassert(full_screen, "B1")
  checkmate::qassert(content_background_colour, "S1")
  checkmate::qassert(content_border_width, "S1")
  stopifnot(is.character(css))
  if (left_margin + right_margin >= 12)
    stop("left_margin and right_margin must sum to less than 12")

  arg <- as.list(environment())
  if (full_screen) {
    arg$show_header <- FALSE
    arg$show_footer <- FALSE
    arg$left_margin <- 0L
    arg$right_margin <- 0L
    arg$content_border_width <- "0px"
  }

  arg
}

#' Demo options
#'
#' Test options list for demo purposes.
#'
#' @param title See \code{\link{test_options}}.
#' @param admin_password See \code{\link{test_options}}.
#' @param researcher_email See \code{\link{test_options}}.
#' @param demo See \code{\link{test_options}}.
#' @param ... Arguments to be passed to \code{\link{test_options}()}.
#' @export
demo_options <- function(title = "Demo", admin_password = "demo",
                         researcher_email = "XXX", demo = TRUE, ...) {
  test_options(title = title, admin_password = admin_password,
               researcher_email = researcher_email, demo = demo, ...)
}

#' Test write permissions
#'
#' Test the ability to write to a given directory.
#' @param dir Directory to test.
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

OUTPUT_DIRS <- c("output_dir", "results_dir", "session_dir",
                 "results_archive_dir", "error_dir")

#' Check directories
#'
#' Checks that all directories as specified in the test options
#' are writeable.
#' @param opt Options list as created by \code{test_options()}.
#' @export
check_dirs <- function(opt) {
  for (d in OUTPUT_DIRS) {
    dir <- opt[[d]]
    stopifnot(is.scalar.character(dir))
    R.utils::mkdirs(dir)
    if (!test_permissions(dir)) {
      stop("Insufficient permissions to write to directory ",
           dir, ". Try upgrading permissions by running 'chown -R shiny <dir>'",
           "at the terminal, where <dir> is your app's directory.")
    }
  }
}

# clear_output_dirs <- function(opt) {
#   success <- unlink(opt$output_dir, recursive = TRUE) == 0L
#   if (!success) message("Failed to clear output directory.")
#   Sys.sleep(0.01)
#   check_dirs(opt)
# }
