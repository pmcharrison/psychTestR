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
    shiny::p(shiny::actionButton("admin_panel.open_test", "Open test"))
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
    admin_panel.observe_open_close_buttons(input)
  )
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
  utils::zip(zipfile = output_file, files = results_dir)
}

admin_panel.server <- function(state, input, output, session, options) {
  if (options$enable_admin_panel) {
    admin_panel.render_ui(state, output)
    admin_panel.handle_downloads(state, output, options)
    admin_panel.observers(state, input, session, options)
  }
}

# admin_panel.download_all_results
