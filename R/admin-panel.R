admin_panel.statistics.ui <- shinyBS::bsModal(
  "admin_panel.statistics.ui",
  title = "Statistics",
  trigger = "admin_panel.statistics.open",
  shiny::uiOutput("admin_panel.statistics.num_participants"),
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

admin_panel.ui.logged_in <-
  shiny::div(
    shiny::h3("Admin"),
    align = "center",
    admin_panel.statistics.ui,
    shiny::fluidRow(
      shiny::column(
        2,
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
        2,
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
      ),
      shiny::column(
        2,
        shinyBS::tipify(
          # periods in the uiOutput label seem to break shinyBS
          el = shiny::tags$div(shiny::uiOutput("admin_panel_pilot_ui")),
          title = paste0("Pilot mode affects only the current participant. ",
                         "In pilot mode, testing proceeds as normal, ",
                         "but the saved results are marked as pilot results."),
          placement = "top"
        )
      ),
      shiny::column(
        2,
        # periods in the uiOutput label seem to break shinyBS
        shiny::uiOutput("admin_panel_open_close_buttons")
      ),
      shiny::column(
        2,
        shinyBS::tipify(
          shiny::p(shiny::actionButton("admin_panel.delete_results",
                                       "Delete results",
                                       onclick = "confirm_delete_results();")),
          title = "Backs up and then deletes all results.",
          placement = "top"
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
                         "before session clearing.")
        )
      ),
      shiny::column(
        2,
        shinyBS::tipify(
          el = shiny::p(shiny::actionButton("admin_panel.statistics.open", "Statistics")),
          title = "Displays participation statistics."
        ),
        shinyBS::tipify(
          el = shiny::p(shiny::actionButton("admin_logout", "Exit admin mode",
                                            style = "color: white; background-color: #c62121")),
          title = "Signs out of administration mode.",
          placement = "top"
        )
      )
    )
  )

admin_panel.render_ui <- function(state, output) {
  output$admin_panel.ui <- shiny::renderUI({
    if (admin(state)) admin_panel.ui.logged_in else admin_panel.ui.logged_out
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
                                                      options) {
  shiny::observeEvent(
    input$submit_admin_password, {
      if (input$admin_password == options$admin_password) {
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

admin_panel.observers <- function(state, input, output, session, options) {
  list(
    admin_panel.observe.admin_login_trigger(input, session),
    admin_panel.observe.submit_admin_password(state, input, session, options),
    admin_panel.observe.admin_logout(state, input, session),
    admin_panel.observe_open_close_buttons(input, output, session),
    admin_panel.delete_results.observers(input, options),
    admin_panel.clear_sessions.observers(input, options),
    admin_panel.statistics.num_participants(input, output, options),
    admin_panel.statistics.latest_results(input, output, options),
    admin_panel.statistics.open(input, session),
    admin_panel.observe.pilot_mode(state, input, output, session)
  )
}

admin_panel.observe.pilot_mode <- function(state, input, output, session) {
  output$admin_panel_pilot_ui <- shiny::renderUI({
    state$pilot
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

admin_panel.statistics.num_participants <- function(input, output, options) {
  output$admin_panel.statistics.num_participants <- shiny::renderUI({
    input$admin_panel.statistics.refresh
    # input$admin_panel.delete_results
    input$admin_panel.statistics.open
    shiny::showNotification("Counting participants...")
    n_complete <- length(list.files(options$results_dir,
                                    pattern = "final=true\\.rds$"))
    n_part_complete <- length(list.files(options$results_dir,
                                         pattern = "final=false\\.rds$"))
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

admin_panel.statistics.latest_results <- function(input, output, options) {
  output$admin_panel.statistics.latest_results <- shiny::renderUI({
    input$admin_panel.statistics.refresh
    # input$admin_panel.delete_results
    input$admin_panel.statistics.open
    files <- list.files(options$results_dir, pattern = "\\.rds$")
    if (length(files) > 0L) {
      ids <- gsub("&p_id=.*", "", files)
      ids <- gsub("id=", "", ids)
      ids <- as.integer(ids)
      latest_file <- files[[which.max(ids)]]
      latest_path <- file.path(options$results_dir, latest_file)
      latest_data <- readRDS(latest_path)
      latest_time <- as.list(latest_data)$session$current_time
      if (!is.null(latest_time)) {
        shiny::p("Last data saved at: ",
                 shiny::strong(format(latest_time)))
      }
    }
  })
}

admin_panel.delete_results.observers <- function(input, options) {
  shiny::observeEvent(input$admin_panel.confirm_delete_results,
                      admin_panel.delete_results.actual(options))
}

admin_panel.clear_sessions.observers <- function(input, options) {
  shiny::observeEvent(input$admin_panel.confirm_clear_sessions,
                      admin_panel.clear_sessions.actual(options))
}

admin_panel.delete_results.actual <- function(options) {
  dir <- options$results_archive_dir
  R.utils::mkdirs(dir)
  file <- paste0(format(Sys.time(),
                        format = "date=%Y-%m-%d&time=%H-%M-%S&tz=%Z"),
                 ".zip")
  path <- file.path(dir, file)
  shiny::showNotification("Creating results backup...")
  zip_all_results(output_file = path, results_dir = options$results_dir)
  if (file.exists(path)) {
    shiny::showNotification("Backup created.")
    unlink(options$results_dir, recursive = TRUE)
    Sys.sleep(0.01)
    dir.create(options$results_dir)
    shiny::showNotification("Deleted results.")
  } else {
    shiny::showNotification(
      "Backup failed, deleting cancelled.")
  }
}

admin_panel.clear_sessions.actual <- function(options) {
  dir <- options$session_dir
  unlink(dir, recursive = TRUE)
  Sys.sleep(0.01)
  dir.create(dir)
  shiny::showNotification("Successfully cleared session files.")
}

admin_panel.observe_open_close_buttons <- function(input, output, session) {
  output$admin_panel_open_close_buttons <- shiny::renderUI({
    shiny::invalidateLater(500, session)
    input$admin_panel.close_test
    input$admin_panel.open_test

    highlight_style <- "color: white; background-color: #c62121"

    closed <- is_test_closed()

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
    shiny::observeEvent(input$admin_panel.close_test, close_test()),
    shiny::observeEvent(input$admin_panel.open_test, open_test())
  )
}

admin_panel.handle_downloads <- function(state, output, options) {
  admin_panel.handle_downloads.current_results.rds(state, output)
  admin_panel.handle_downloads.all_results.rds(state, output, options)
  admin_panel.handle_downloads.current_results.csv(state, output)
  admin_panel.handle_downloads.all_results.csv(state, output, options)
}

admin_panel.handle_downloads.current_results.rds <- function(state, output) {
  output$admin_panel.download_current_results.rds <- shiny::downloadHandler(
    filename = "results.rds",
    content = function(file) saveRDS(get_results(state, add_session_info = TRUE),
                                     file = file)
  )
}

admin_panel.handle_downloads.current_results.csv <- function(state, output) {
  output$admin_panel.download_current_results.csv <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      df <- tryCatch({
        as.data.frame(get_results(state, add_session_info = TRUE))
      }, error = function(e) {
        msg <- "Failed to create csv file. Try saving an RDS file instead."
        shiny::showNotification(msg, type = "error")
        data.frame(result = msg)
      })
      write.csv(df, file, row.names = FALSE)
    }
  )
}

admin_panel.handle_downloads.all_results.rds <- function(state, output, options) {
  output$admin_panel.download_all_results.rds <- shiny::downloadHandler(
    filename = "results.zip",
    content = function(file) zip_all_results(file, options$results_dir)
  )
}

admin_panel.handle_downloads.all_results.csv <- function(state, output, options) {
  output$admin_panel.download_all_results.csv <- shiny::downloadHandler(
    filename = "results.csv",
    content = function(file) {
      df <- tryCatch({
        df_all_results(options$results_dir)
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

zip_all_results <- function(output_file, results_dir) {
  if (!dir.exists(results_dir)) stop("<results_dir> doesn't exist")
  old_wd <- getwd()
  results_dir <- gsub("/$", "", results_dir)
  results_dir_parent <- dirname(results_dir)
  results_dir_name <- basename(results_dir)
  output_full_path <- file.path(normalizePath(dirname(output_file)),
                                basename(output_file))
  tryCatch({
    setwd(results_dir_parent)
    utils::zip(zipfile = output_full_path, files = results_dir_name)
    setwd(old_wd)
  }, error = function(e) {
    setwd(old_wd)
    shinyjs::alert("failed to create zip file")
  }
  )
}

df_all_results <- function(results_dir) {
  files <- list.files(results_dir, pattern = "\\.rds", full.names = TRUE)
  data <- lapply(files, readRDS)
  data_df <- lapply(data, as.data.frame)
  any_cols_duplicated <- any(vapply(data_df,
                                    function(df) anyDuplicated(names(df)),
                                    integer(1)) > 0L)
  if (any_cols_duplicated) {
    msg <- "CSV export cannot cope with duplicated fields in results objects."
    shiny::showNotification(msg, type = "error")
    stop(msg)
  }
  do.call(plyr::rbind.fill, data_df)
}

admin_panel.server <- function(state, input, output, session, options) {
  if (options$enable_admin_panel) {
    admin_panel.render_ui(state, output)
    admin_panel.handle_downloads(state, output, options)
    admin_panel.observers(state, input, output, session, options)
  }
}

# admin_panel.download_all_results
