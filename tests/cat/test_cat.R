library(magrittr)
library(shinythemes)

title <- "Test CAT"

display_options <- list(theme = shinytheme("readable"),
                        cols_round_digits = 3,
                        cols_to_round = c("discrimination", "difficulty",
                                          "guessing", "inattention",
                                          "information",
                                          "theta_ML", "theta_ML_sem",
                                          "theta_BM", "theta_BM_sem",
                                          "theta_EAP", "theta_EAP_sem",
                                          "theta_WL", "theta_WL_sem"))

renderOutputs <- function(rv, output) {
  output$item_info <- DT::renderDataTable({
    rv$current_page # for some reason changes aren't detected in rv$results$piat$items
    rv$params$cat@results.by_item %>%
      DT::datatable(
        data = .,
        options = list(scrollX = TRUE),
        rownames = FALSE
      ) %>%
      DT::formatRound(table = .,
                      columns = rv$params$display_options$cols_to_round,
                      digits = rv$params$display_options$cols_round_digits)
  },
  server = TRUE
  )
  output$download_results <- downloadHandler(
    filename = "results.RDS",
    content = function(file) {
      saveRDS(rv$params$cat, file)
    }
  )
}

side_panel_ui <- div(
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
  ))

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
