set_error_handling <- function(opt, session, state) {
  f <- if (opt$debug_locally) {
    browser
  } else {
    function() {
      # err <- geterrmessage()
      # display_error_msg(err, opt)
      # if (opt$notify_error) notify_error(err, opt)
      # if (opt$log_error) log_error(opt)
      shinyjs::alert("Oops")
      error(state) <- "A fatal error occurred."
      # stop()
      # session$close()
    }
  }
  options(shiny.error = f)
}

display_error_msg <- function(err, opt) {
  tryCatch({
    msg <- paste0(
      "A fatal error occurred and the test cannot continue. ",
      "Please contact the researcher for advice.",
      err = if (opt$show_full_error_msg)
        paste0("\nError message: ", err) else "")
    tryCatch(
      shinyjs::alert(msg),
      error = function(e) shinyjs::alert(
        "A fatal error occurred and the test cannot continue."))
  }, error = function(e) {
    # browser()
    message("failed to display error message to user")
  })
}

notify_error <- function(err, opt) {
  tryCatch({
    async_pushbullet(
      title = "Test error: alert (1/2)",
      body = paste0("An error occurred in the test entitled ",
                    "'", opt$title[1], "'.",
                    " A detailed error message will follow shortly."),
      opt)
    async_pushbullet(
      title = "Test error: alert (2/2)",
      body = paste0("Error message: ", err),
      opt
    )
  }, error = function(e) message("failed to send Pushbullet error notification"))
}

log_error <- function(opt) {
  tryCatch({
    dump_file <- file.path(
      opt$error_dir,
      sprintf("id=%i&date=%s",
              id = 1L + length(list.files(opt$error_dir)),
              date = format(
                Sys.time(), format = "%Y-%m-%d&time=%H-%M-%S&tz=%Z")))
    utils::dump.frames(dumpto = dump_file,
                       to.file = TRUE,
                       include.GlobalEnv = TRUE)
  }, error = function(e) message("failed to dump error files"))
}
