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
    # shinyBS::tipify(el = shiny::div(shiny::actionButton("test", "Hello")), title = "Download current participant&#39;s results.")
    shinyBS::tipify(
      el = shiny::p(shiny::downloadButton("admin_panel.download_current_results", "Download current results")),
      title = paste0("Download current participant&#39;s results. ",
                     "Downloaded results can be read into R using the ",
                     "function readRDS().")
    ),
    shinyBS::tipify(
      el = shiny::p(shiny::downloadButton("admin_panel.download_all_results", "Download all results")),
      title = paste0("Download all participants&#39; results as a zip file. ",
                     "Individual participant&#39;s files can then be read into R ",
                     "using the function <em>readRDS()</em>.")),
    shinyBS::tipify(
      el = shiny::p(shiny::actionButton("admin_logout", "Exit admin mode",
                                        style = "color: white; background-color: #c62121")),
      title = "Click to sign out of administration mode."
    ),
    # shiny::p("The test is currently ", shiny::em(shiny::textOutput("admin_panel.text_closed")), "."),
    shiny::p(shiny::actionButton("admin_panel.close_test", "Close test")),
    shiny::p(shiny::actionButton("admin_panel.open_test", "Open test")),
    shinyBS::tipify(
      shiny::p(shiny::actionButton("admin_panel.delete_results",
                                   "Delete results",
                                   onclick = "confirm_delete_results();")),
      title = "Backs up and then deletes all results."
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
    ))

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

admin_panel.observers <- function(state, input, session, options) {
  list(
    admin_panel.observe.admin_login_trigger(input, session),
    admin_panel.observe.submit_admin_password(state, input, session, options),
    admin_panel.observe.admin_logout(state, input, session),
    admin_panel.observe_open_close_buttons(input),
    admin_panel.delete_results.observers(input, options),
    admin_panel.clear_sessions.observers(input, options)
  )
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

admin_panel.observe_open_close_buttons <- function(input) {
  list(
    shiny::observeEvent(input$admin_panel.close_test, close_test()),
    shiny::observeEvent(input$admin_panel.open_test, open_test())
  )
}

admin_panel.handle_downloads <- function(state, output, options) {
  admin_panel.handle_downloads.current_results(state, output)
  admin_panel.handle_downloads.all_results(state, output, options)
}

admin_panel.handle_downloads.current_results <- function(state, output) {
  output$admin_panel.download_current_results <- shiny::downloadHandler(
    filename = "results.rds",
    content = function(file) saveRDS(get_results(state, add_session_info = TRUE),
                                     file = file)
  )
}

admin_panel.handle_downloads.all_results <- function(state, output, options) {
  output$admin_panel.download_all_results <- shiny::downloadHandler(
    filename = "output.zip",
    content = function(file) zip_all_results(file, options$results_dir)
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

admin_panel.server <- function(state, input, output, session, options) {
  if (options$enable_admin_panel) {
    admin_panel.render_ui(state, output)
    admin_panel.handle_downloads(state, output, options)
    admin_panel.observers(state, input, session, options)
  }
}

# admin_panel.download_all_results
