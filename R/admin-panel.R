admin_panel.statistics.ui <- shinyBS::bsModal(
  "admin_panel.statistics.ui",
  title = "Statistics",
  trigger = "admin_panel.statistics.open",
  shiny::uiOutput("admin_panel.statistics.num_participants"),
  shiny::uiOutput("admin_panel.statistics.average_time"),
  shiny::uiOutput("admin_panel.statistics.latest_results"),
  shiny::actionButton("admin_panel.statistics.refresh", "Refresh")
)

admin_panel.ui.logged_out <- shiny::tags$div(
  id = "admin_panel.ui.logged_out",
  shinyBS::tipify(
    el = shiny::tags$p(shiny::actionLink("admin_login_trigger", "Admin login")),
    title = "Click here to enter your administrator credentials."
  )
)

admin_panel.ui.results <- shiny::fluidRow(shiny::fluidRow(
  shiny::column(
    6,
    shinyBS::tipify(
      el = shiny::p(shiny::downloadButton("admin_panel.download_current_results.rds",
                                          "Current (RDS)")),
      title = paste0("Downloads current participant&#39;s results as an RDS file. ",
                     "Downloaded results can be read into R using the ",
                     "function readRDS()."),
      placement = "top"
    ),
    shinyBS::tipify(
      el = shiny::p(shiny::downloadButton("admin_panel.download_all_results.rds",
                                          "All (RDS)")),
      title = paste0("Downloads all participants&#39; results as a zip file of RDS files. ",
                     "Individual participant&#39;s files can then be read into R ",
                     "using the function <em>readRDS()</em>."),
      placement = "top")
  ),
  shiny::column(
    6,
    shinyBS::tipify(
      el = shiny::p(shiny::downloadButton("admin_panel.download_current_results.csv",
                                          "Current (CSV)")),
      title = paste0("Downloads current participant&#39;s results as a CSV file. ",
                     "CSV files will typically contain less detailed results ",
                     "than equivalent RDS files."),
      placement = "top"
    ),
    shinyBS::tipify(
      el = shiny::p(shiny::downloadButton("admin_panel.download_all_results.csv",
                                          "All (CSV)")),
      title = paste0("Downloads all participants&#39; results as a single CSV file. ",
                     "CSV files will typically contain less detailed results ",
                     "than equivalent RDS files."),
      placement = "top")
  )),
  shinyBS::tipify(
    shiny::p(shiny::actionButton("admin_panel.delete_results",
                                 "Delete all",
                                 onclick = "confirm_delete_results();")),
    title = "Backs up and then deletes all results.",
    placement = "top"
  ))

admin_panel.ui.errors <- shiny::fluidRow(
  shinyBS::tipify(
    el = shiny::p(shiny::downloadButton("admin_panel.download_errors",
                                        "Download")),
    title = paste0("Downloads error logs as a zip file. ",
                   "Explore individual error logs in R by loading the file ",
                   "with <em>load()</em> and then calling <em>debugger()</em> ",
                   "on the resulting object."),
    placement = "top"),
  shinyBS::tipify(
    shiny::p(shiny::actionButton("admin_panel.delete_errors",
                                 "Delete",
                                 onclick = "confirm_delete_errors();")),
    title = "Deletes all error logs",
    placement = "top"
  ))

admin_panel.ui.pilot <- shinyBS::tipify(
  # periods in the uiOutput label seem to break shinyBS
  el = shiny::tags$div(shiny::uiOutput("admin_panel_pilot_ui")),
  title = paste0("Pilot mode affects only the current participant. ",
                 "In pilot mode, testing proceeds as normal, ",
                 "but the saved results are marked as pilot results."),
  placement = "top"
)

admin_panel.ui.availability <- shiny::uiOutput("admin_panel_open_close_buttons")

admin_panel.ui.misc <- shiny::fluidRow(
  shinyBS::tipify(
    el = shiny::p(shiny::actionButton("admin_panel.statistics.open", "Statistics")),
    title = "Displays participation statistics."
  ),
  shinyBS::tipify(
    shiny::p(shiny::actionButton("admin_panel.clear_sessions",
                                 "Clear sessions",
                                 onclick = "confirm_clear_sessions();")),
    placement = "top",
    title = paste0("Clears session files. ",
                   "Current testing sessions will not be interrupted. ",
                   "However, participants will be not be able to use URLs ",
                   "to resume testing sessions last active ",
                   "before session clearing.")),
  shinyBS::tipify(
    el = shiny::p(shiny::actionButton("admin_logout", "Exit admin mode",
                                      style = "color: white; background-color: #c62121")),
    title = "Signs out of administration mode.",
    placement = "top"
  ))


admin_panel.ui.logged_in <- shiny::fluidRow(
  shiny::fluidRow(
    shiny::h3("Admin"),
    align = "center",
    shiny::uiOutput("page_admin_ui"),
    admin_panel.statistics.ui,
    shiny::column(4, shiny::h4("Results"), admin_panel.ui.results),
    shiny::column(2, shiny::h4("Error logs"), admin_panel.ui.errors),
    shiny::column(2, shiny::h4("Piloting"), admin_panel.ui.pilot),
    shiny::column(2, shiny::h4("Availability"), admin_panel.ui.availability),
    shiny::column(2, shiny::h4("Misc."), admin_panel.ui.misc)
  ),
  shiny::fluidRow(shiny::uiOutput("custom_admin_panel"))
)

admin_panel.render_ui <- function(state, output, elts, opt) {
  output$admin_panel.ui <- shiny::renderUI({
    if (admin(state)) admin_panel.ui.logged_in else admin_panel.ui.logged_out
  })
  output$page_admin_ui <- shiny::renderUI({
    if (admin(state)) get_current_elt(state, elts, opt, eval = TRUE)@admin_ui
  })
}

admin_panel.modals <- shinyBS::bsModal(
  "admin_login_popup", "Admin login",
  "null_trigger", size = "small",
  shiny::wellPanel(
    align = "center",
    shiny::tags$p(shiny::passwordInput("admin_password", label = "Password")),
    shiny::tags$p(shiny::actionButton(inputId = "submit_admin_password", "Submit")),
    onkeypress = paste0("if (event.keyCode == 13) ",
                        "document.getElementById('submit_admin_password').click()")))

admin_panel.observe.admin_login_trigger <- function(input, session) {
  shiny::observeEvent(input$admin_login_trigger, {
    shinyBS::toggleModal(session, "admin_login_popup", toggle = "open")
    # shinyjs::runjs('document.getElementById("admin_password").focus();')
  })
}

admin_panel.observe.submit_admin_password <- function(state, input, session,
                                                      opt) {
  shiny::observeEvent(
    input$submit_admin_password, {
      if (input$admin_password == opt$admin_password) {
        admin(state) <- TRUE
        shinyBS::toggleModal(session, "admin_login_popup", toggle = "close")
      } else {
        shinyjs::alert("Incorrect password.")
      }
    })
}

admin_panel.observe.admin_logout <- function(state, input, session) {
  shiny::observeEvent(input$admin_logout, {
    admin(state) <- FALSE
    print(admin(state))
    shiny::updateTextInput(session, "admin_password", value = "")
  })
}

admin_panel.observers <- function(state, input, output, session, opt) {
  list(
    admin_panel.observe.admin_login_trigger(input, session),
    admin_panel.observe.submit_admin_password(state, input, session, opt),
    admin_panel.observe.admin_logout(state, input, session),
    admin_panel.observe_open_close_buttons(input, output, session, opt),
    admin_panel.delete_results.observers(state, input, opt),
    admin_panel.delete_errors.observers(state, input, opt),
    admin_panel.clear_sessions.observers(state, input, opt),
    admin_panel.statistics.num_participants(input, output, opt),
    admin_panel.statistics.latest_results(input, output, opt),
    admin_panel.statistics.average_time(input, output, opt),
    admin_panel.statistics.open(input, session),
    admin_panel.observe.pilot_mode(state, input, output, session)
  )
}

admin_panel.observe.pilot_mode <- function(state, input, output, session) {
  output$admin_panel_pilot_ui <- shiny::renderUI({
    pilot <- pilot(state)

    highlight_style <- "color: white; background-color: #c62121"

    btn.pilot <- shiny::actionButton("admin_panel.pilot_mode", "Pilot",
                                     style = if (pilot) highlight_style)
    btn.live <- shiny::actionButton("admin_panel.live_mode", "Live",
                                    style = if (!pilot) highlight_style)

    shiny::div(
      shiny::p(btn.pilot), shiny::p(btn.live)
    )
  })
  list(
    shiny::observeEvent(input$admin_panel.pilot_mode, {
      if (pilot(state)) {
        shiny::showNotification("Already in pilot mode.", type = "warning")
      } else {
        pilot(state) <- TRUE
        shiny::showNotification("Entering pilot mode.", type = "message")
      }
    }),
    shiny::observeEvent(input$admin_panel.live_mode, {
      if (!pilot(state)) {
        shiny::showNotification("Already in live mode.", type = "warning")
      } else {
        pilot(state) <- FALSE
        shiny::showNotification("Entering live mode.", type = "message")
      }
    })
  )
}

admin_panel.statistics.open <- function(input, session) {
  shiny::observeEvent(input$admin_panel.statistics.open,
                      shinyBS::toggleModal(session,
                                           "admin_panel.statistics.ui",
                                           toggle = "open"))
}

admin_panel.statistics.num_participants <- function(input, output, opt) {
  output$admin_panel.statistics.num_participants <- shiny::renderUI({
    input$admin_panel.statistics.refresh
    input$admin_panel.statistics.open
    shiny::showNotification("Refreshing statistics...")
    df <- tabulate_results(opt, include_pilot = FALSE)
    n_complete <- sum(df$complete)
    n_part_complete <- sum(!df$complete)
    shiny::p(
      "The output directory contains results for ",
      shiny::strong(format(n_complete, scientific = FALSE)),
      " completed ",
      ngettext(n_complete, "session", "sessions"),
      " and ",
      shiny::strong(format(n_part_complete, scientific = FALSE)),
      " partly completed ",
      ngettext(n_part_complete, "session.", "sessions.")
    )
  })
}

admin_panel.statistics.latest_results <- function(input, output, opt) {
  output$admin_panel.statistics.latest_results <- shiny::renderUI({
    input$admin_panel.statistics.refresh
    input$admin_panel.statistics.open
    files <- tabulate_results(opt, include_pilot = FALSE)
    if (nrow(files) > 0L) {
      latest_file <- files$file[[which.max(files$id)]]
      latest_path <- file.path(opt$results_dir, latest_file)
      latest_data <- readRDS(latest_path)
      latest_time <- as.list(latest_data)$session$current_time
      if (!is.null(latest_time)) {
        time_diff <- Sys.time() - latest_time
        time_diff_formatted <- sprintf("(%s %s ago)",
                                       format(as.numeric(time_diff), digits = 3),
                                       units(time_diff))
        shiny::p("Last data saved at: ",
                 shiny::strong(format(latest_time, format = "%Y-%m-%d %H:%M:%S %Z")),
                 time_diff_formatted)
      }
    }
  })
}

admin_panel.statistics.average_time <- function(input, output, opt) {
  output$admin_panel.statistics.average_time <- shiny::renderUI({
    input$admin_panel.statistics.refresh
    input$admin_panel.statistics.open
    files <- tabulate_results(opt, include_pilot = FALSE)
    files <- files[files$complete, ]
    if (nrow(files) > 0L) {
      data <- lapply(files$full_file, readRDS)
      time_taken <- vapply(data, function(x) {
        difftime(x$session$current_time, x$session$time_started, units = "mins")
      }, numeric(1))
      M <- mean(time_taken)
      SD <- sd(time_taken)
      shiny::p(shiny::HTML(sprintf(
        "Mean completion time: <strong>%s</strong> min. (SD = <strong>%s</strong>)",
        format(M, digits = 3L),
        format(SD, digits = 3L))))
    }
  })
}

admin_panel.delete_results.observers <- function(state, input, opt) {
  shiny::observeEvent(
    input$admin_panel.confirm_delete_results,
    if (admin(state)) admin_panel.delete_results.actual(opt))
}

admin_panel.delete_errors.observers <- function(state, input, opt) {
  shiny::observeEvent(
    input$admin_panel.confirm_delete_errors,
    if (admin(state)) admin_panel.delete_errors.actual(opt))
}

admin_panel.clear_sessions.observers <- function(state, input, opt) {
  shiny::observeEvent(
    input$admin_panel.confirm_clear_sessions,
    if (admin(state)) admin_panel.clear_sessions.actual(opt))
}

admin_panel.delete_results.actual <- function(opt) {
  dir <- opt$results_archive_dir
  R.utils::mkdirs(dir)
  file <- paste0(format(Sys.time(),
                        format = "date=%Y-%m-%d&time=%H-%M-%S&tz=%Z"),
                 ".zip")
  path <- file.path(dir, file)
  shiny::showNotification("Creating results backup...")
  zip_dir(dir = opt$results_dir, output_file = path)
  if (file.exists(path)) {
    shiny::showNotification("Backup created.")
    unlink(opt$results_dir, recursive = TRUE)
    Sys.sleep(0.01)
    dir.create(opt$results_dir)
    dir.create(opt$supplementary_results_dir)
    shiny::showNotification("Deleted results.")
  } else {
    shiny::showNotification(
      "Backup failed, deleting cancelled.")
  }
}

admin_panel.delete_errors.actual <- function(opt) {
  unlink(opt$error_dir, recursive = TRUE)
  Sys.sleep(0.01)
  dir.create(opt$error_dir)
  shiny::showNotification("Deleted error logs.")
}

admin_panel.clear_sessions.actual <- function(opt) {
  dir <- opt$session_dir
  unlink(dir, recursive = TRUE)
  Sys.sleep(0.01)
  dir.create(dir)
  shiny::showNotification("Successfully cleared session files.")
}

admin_panel.observe_open_close_buttons <- function(input, output, session, opt) {
  output$admin_panel_open_close_buttons <- shiny::renderUI({
    shiny::invalidateLater(500, session)
    input$admin_panel.close_test
    input$admin_panel.open_test

    highlight_style <- "color: white; background-color: #c62121"

    closed <- is_test_closed(opt)

    btn.open <- shiny::actionButton("admin_panel.open_test", "Open test",
                                    style = if (!closed) highlight_style)
    btn.close <- shiny::actionButton("admin_panel.close_test", "Close test",
                                     style = if (closed) highlight_style)

    btn.open <- shinyBS::tipify(
      el = btn.open,
      title = "Allows new participants to take the test.",
      placement = "top"
    )

    btn.close <- shinyBS::tipify(
      el = btn.close,
      title = "Prevents new participants from starting the test.",
      placement = "top"
    )

    shiny::div(shiny::p(btn.open),
               shiny::p(btn.close))
  })

  list(
    shiny::observeEvent(input$admin_panel.close_test, close_test(opt)),
    shiny::observeEvent(input$admin_panel.open_test, open_test(opt))
  )
}

admin_panel.handle_downloads <- function(state, output, opt) {
  admin_panel.handle_downloads.current_results.rds(state, output)
  admin_panel.handle_downloads.all_results.rds(state, output, opt)
  admin_panel.handle_downloads.current_results.csv(state, output)
  admin_panel.handle_downloads.all_results.csv(state, output, opt)
  admin_panel.handle_download.errors(state, output, opt)
}

admin_panel.handle_downloads.current_results.rds <- function(state, output) {
  output$admin_panel.download_current_results.rds <- shiny::downloadHandler(
    filename = "results.rds",
    content = function(file) saveRDS(get_results(state, add_session_info = TRUE,
                                                 complete = FALSE),
                                     file = file)
  )
}

admin_panel.handle_downloads.current_results.csv <- function(state, output) {
  output$admin_panel.download_current_results.csv <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      df <- tryCatch({
        as.data.frame(get_results(
          state, complete = FALSE, add_session_info = TRUE)) %>%
          list_cols_to_json()
      }, error = function(e) {
        msg <- "Failed to create csv file. Try saving an RDS file instead."
        shiny::showNotification(msg, type = "error")
        data.frame(result = msg)
      })
      write.csv(df, file, row.names = FALSE)
    }
  )
}

admin_panel.handle_download.errors <- function(state, output, opt) {
  output$admin_panel.download_errors <- shiny::downloadHandler(
    filename = "errors.zip",
    content = function(file) zip_dir(opt$error_dir, file)
  )
}

admin_panel.handle_downloads.all_results.rds <- function(state, output, opt) {
  output$admin_panel.download_all_results.rds <- shiny::downloadHandler(
    filename = "results.zip",
    content = function(file) zip_dir(opt$results_dir, file)
  )
}

admin_panel.handle_downloads.all_results.csv <- function(state, output, opt) {
  output$admin_panel.download_all_results.csv <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      df <- tryCatch({
        df_all_results(opt$results_dir) %>%
          list_cols_to_json()
      }, error = function(e) {
        print(e)
        msg <- "Failed to create csv file. Try saving an RDS file instead."
        shiny::showNotification(msg, type = "error")
        data.frame(result = msg)
      })
      write.csv(df, file, row.names = FALSE)
    }
  )
}

list_cols_to_json <- function(df) {
  which_list <- purrr::map_lgl(df, is.list) %>% which()
  for (i in which_list) {
    df[[i]] <- purrr::map_chr(df[[i]], jsonlite::toJSON)
  }
  df
}

zip_dir <- function(dir, output_file) {
  if (!dir.exists(dir)) stop("`dir` doesn't exist")
  old_wd <- getwd()
  dir <- gsub("/$", "", dir)
  dir_parent <- dirname(dir)
  dir_name <- basename(dir)
  output_full_path <- file.path(normalizePath(dirname(output_file)),
                                basename(output_file))
  tryCatch({
    setwd(dir_parent)
    utils::zip(zipfile = output_full_path, files = dir_name)
    setwd(old_wd)
  }, error = function(e) {
    setwd(old_wd)
    shinyjs::alert("failed to create zip file")
  }
  )
}

df_all_results <- function(results_dir) {
  files <- list_results_files(results_dir, full.names = TRUE)
  if (length(files) == 0L) return(data.frame())
  df <- purrr::map_dfr(files, function(results){
      results %>%
      readRDS() %>%
      as.list() %>%
      as.data.frame(stringsAsFactors = F)
  })
  session_cols <- grep("^session", names(df), value = TRUE)
  cols_in_order <- c(session_cols, setdiff(names(df), session_cols))
  df[order(df$session.current_time), cols_in_order]
}

admin_panel.server <- function(state, input, output, session, opt, elts) {
  if (opt$enable_admin_panel) {
    admin_panel.render_ui(state, output, elts, opt)
    admin_panel.handle_downloads(state, output, opt)
    admin_panel.observers(state, input, output, session, opt)
  }
}
