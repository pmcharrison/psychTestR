library(magrittr)
library(shinythemes)

title <- "Test CAT"

admin <- list(state = FALSE,
              password = "ringo")
admin_mode <- FALSE

display_options <- list(theme = shinytheme("readable"),
                        cols_round_digits = 3,
                        cols_to_round = c("discrimination", "difficulty",
                                          "guessing", "inattention",
                                          "information",
                                          "theta_ML", "theta_ML_sem",
                                          "theta_BM", "theta_BM_sem",
                                          "theta_EAP", "theta_EAP_sem",
                                          "theta_WL", "theta_WL_sem"),
                        col_labels = c(
                          num = "Position",
                          item_id = "Item ID",
                          discrimination = "Discrimination",
                          difficulty = "Difficulty",
                          guessing = "Guessing",
                          inattention = "Inattention",
                          information = "Information",
                          criterion = "Selection criterion",
                          response = "Response",
                          correct_answer = "Correct answer",
                          score = "Score",
                          theta_ML = "Ability (ML)",
                          theta_ML_sem = "Ability SEM (ML)",
                          theta_BM = "Ability (BM)",
                          theta_BM_sem = "Ability SEM (BM)",
                          theta_EAP = "Ability (EAP)",
                          theta_EAP_sem = "Ability SEM (EAP)",
                          theta_WL = "Ability (WL)",
                          theta_WL_sem = "Ability SEM (WL)"
                        ))

renderOutputs <- function(rv, input, output) {
  output$item_info <- DT::renderDataTable({
    # Induce a dependency on rv$current_page, because
    # for some reason changes aren't detected in rv$results$piat$items.
    rv$current_page 
    # Get the data to display
    df <- rv$params$cat@results.by_item
    # Rename the dataframe columns
    names(df) <- 
      plyr::revalue(names(df),
                    rv$params$display_options$col_labels,
                    warn_missing = FALSE)
    # Identify the columns to round, bearing in mind the new column abels
    cols_to_round <- 
      plyr::revalue(rv$params$display_options$cols_to_round,
                    rv$params$display_options$col_labels,
                    warn_missing = FALSE) %>%
      intersect(., names(df))
    # Get the number of digits that each column should be rounded to
    cols_round_digits <- rv$params$display_options$cols_round_digits
    # Construct the datatable
    DT::datatable(
      data = df,
      options = list(scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatRound(table = .,
                      columns = cols_to_round,
                      digits = cols_round_digits)
  },
  server = TRUE
  )
  output$download_results <- downloadHandler(
    filename = "results.RDS",
    content = function(file) {
      saveRDS(rv$params$cat, file)
    }
  )
  output$admin_side_panel <- renderUI({
    div(
      id = "admin_side_panel",
      # if (rv$params$admin$state) {
      if (rv$admin) {
        div(
          id = "admin_side_panel_active",
          h3("Admin panel"),
          align = "center",
          shinyBS::tipify(
            el = tags$p(actionButton("item_info_trigger", "Show item info")),
            title = "This popup table describes the items that the participant will take during the testing session, as well as holding the results to the items already administered."
          ),
          shinyBS::bsModal("item_info_popup", "Item info",
                           "item_info_trigger", size = "large",
                           wellPanel(DT::dataTableOutput("item_info"))),
          shinyBS::tipify(
            el = tags$p(downloadButton("download_results", "Download results")),
            title = "Downloaded results can be read into R using the function <em>readRDS()</em>."
          )
        )
      } else {
        div(
          id = "admin_side_panel_inactive",
          shinyBS::tipify(
            el = tags$p(actionButton("admin_login_trigger", "Admin login")),
            title = "Click here to enter your administrator credentials."
          )
        )
      }
    )
  })
}

renderModals <- function(rv, input, output, session) {
  output$modals <- renderUI(tags$div(
    id = "modals",
    shinyBS::bsModal("admin_login_popup", "Admin login",
                     "null_trigger", size = "small",
                     wellPanel(
                       align = "center",
                       tags$p(textInput("admin_password", label = "Password",
                                        placeholder = "Enter your password here")),
                       tags$p(actionButton(inputId = "submit_admin_password", "Submit"))
                     ))
  ))
}

observeEvents <- function(rv, input, session) {
  list(
    observeEvent(input$admin_login_trigger, toggleModal(session, "admin_login_popup", toggle = "open")),
    observeEvent(input$submit_admin_password,
                 if (input$admin_password == rv$params$admin$password) {
                   rv$admin <- TRUE
                   toggleModal(session, "admin_login_popup", toggle = "close")
                 } else {
                   shinyjs::alert("Incorrect password.")
                 })
  )
}

side_panel_ui <- div(
  uiOutput("admin_side_panel")
)

item_bank <- read.csv("/Users/peter/Dropbox/Academic/projects/musical-tests/test-materials/bat-cat/materials/v1/psychometric_data/BAT_pysch_data.csv", stringsAsFactors = FALSE)
itemPar <- as.matrix(data.frame(discrimination = item_bank$discrimination,
                                difficulty = item_bank$difficultypartial,
                                guessing = 0.5,
                                inattention = 1))

cbGroup <- item_bank$audio_name
cbControlNames <- names(table(cbGroup))
cbControlProps <- rep(x = 1/(length(cbControlNames)), times = length(cbControlNames))
cbControl <- list(names = cbControlNames, props = cbControlProps)

test_modules <- list()
test_modules$cat <- new(
  "AudioCAT",
  itemPar = itemPar,
  audio_paths = item_bank$file_name,
  audio_type = "mp3",
  response_options = c("First was on the beat", "Second was on the beat"),
  answers = ifelse(item_bank$answer == 1, "First was on the beat", "Second was on the beat"),
  cbControl = cbControl, cbGroup = cbGroup,
  intro = list(), params_id = "cat",
  item.prompt = tags$p("In which extract was the beep-track on the beat?")
)

cat <- new("AudioCATParams",
           audio_root = "stimuli_v1",
           test_length = 5,
           next_item.criterion = "bOpt",
           next_item.estimator = "BM")

pages <- list(
  new("one_btn_page", body = tags$p("Welcome to the test CAT!")),
  test_modules$cat,
  new("final_page", body = tags$p("You've finished!"))
)
