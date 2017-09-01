library(shinythemes)

title <- "Test CAT"

display_options <- list(theme = shinytheme("readable"))

renderOutputs <- function(rv, output) {
}

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
