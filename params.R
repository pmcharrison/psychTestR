title <- "Music imagery test"

side_panel_ui <- div(
  h3("Admin panel"),
  align = "center",
  actionButton("item_info_trigger", "Show item info"),
  shinyBS::bsModal("item_info_popup", "Item info",
                   "item_info_trigger", size = "large",
                   wellPanel(DT::dataTableOutput("item_info"))))

piat <- list()
piat$items <- getStimuli()

test_modules <- list()
test_modules$intro <- withTags(
  list(new("one_btn_page",
           body = p("The Pitch Imagery Arrow Task has been designed to teach you to successfully imagine musical tones from a visual prompt.")),
       new("one_btn_page",
           body = p("Each trial starts with the word “Begin” on the screen, and you will hear an ascending major scale, which provides the key or context for that trial. You will then see a dot on the screen and hear a start note. Press 'Next' for an example of this.")),
       new("video_stimulus_NAFC",
           prompt = p("Here is an example context:"),
           source = "training/Scale_C_ton.mp4",
           type = "mp4",
           response_options = "Next",
           wait = TRUE),
       new("one_btn_page",
           body = p("A variable number of up and/or down arrows will then appear in a sequence, with a corresponding tone, that is stepping up or down the scale. Press 'Next' for an example of these arrows appearing after the ascending scale and start note.")),
       new("video_stimulus_NAFC",
           prompt = p("Here is an example of arrows appearing after the ascending scale and start note:"),
           source = "training/Example_Trial_sounded_arr.mp4",
           type = "mp4",
           response_options = "Next",
           wait = TRUE),
       new("one_btn_page",
           body = p("At some point in the trial, an arrow is shown with no tone heard. Your job is to imagine that exact missing tone. Initially there is one tone to be imagined per trial, but the number of tones to be imagined increases over the task, up to 5 tones. The word “hold” will appear with the last silent arrow of the sequence. Hold in your mind the sound of this last tone as you prepare to hear a test tone. Press 'Next' for an example of a single silent arrow added to our trial example.")),
       new("video_stimulus_NAFC",
           prompt = p("Here is an example:"),
           source = "training/Example_Trial_all_arr.mp4",
           type = "mp4",
           response_options = "Next",
           wait = TRUE),
       new("one_btn_page",
           body = p("To test the accuracy of your imagery, a test tone will be sounded and a white fixation cross will display. The tone will either match the note you are imagining, or will be incorrect. Your task will be to determine which is the case. Press 'Next' for the full example trial and try to respond correctly.")),
       new("video_stimulus_NAFC",
           prompt = p("Here is an example complete trial:"),
           source = "training/Example_Trial_complete.mp4",
           type = "mp4",
           response_options = c("Match", "No match"),
           wait = TRUE),
       new("one_btn_page",
           body = p("Most importantly, there is to be no cheating. This is a pitch imagery task, so no humming or moving is allowed to help you imagine. Your goal is to as vividly as possible, imagine these tones and keep the rest of your body still.")),
       new("one_btn_page",
           body = p("There are 3 practice trials in which you will receive feedback. You are free to attempt these as many times as you wish to familiarise yourself with the task. Once you are ready to start click <Start>. There are 30 trials to complete, starting with one imagined tone per trial and increasing gradually up to five imagined tones per trial. After completing the task, there are a few feedback questions to respond to."))))

test_modules$practice_questions <-
  lapply(
    list(list(id = "Prac_Trial_Lvl1",
              answer = "Match"),
         list(id = "Prac_Trial_Lvl2",
              answer = "No match"),
         list(id = "Prac_Trial_Lvl3",
              answer = "Match")),
    function(x) {
      new("video_stimulus_NAFC",
          prompt = tags$p("Did the final tone match the note you were imagining?"),
          source = paste0("training/", x$id, ".mp4"),
          type = "mp4",
          response_options = c("Match", "No match"),
          wait = TRUE,
          on_complete = function(rv, input) {
            answer <- if (input$Match == 1) {
              "Match"
            } else if (input$`No match` == 1) {
              "No match"
            } else stop("This shouldn't happen!")
            practice_correct <- answer == x$answer
            rv$test_stack <- c(list(new("one_btn_page",
                                        body = tags$p(if (practice_correct) {
                                          "You answered correctly!"
                                        } else "You answered incorrectly."))),
                               rv$test_stack)
          })})

test_modules$repeatable_practice_questions <-
  c(test_modules$practice_questions,
    new("page_NAFC",
        prompt = tags$p("Would you like to try the practice examples again?"),
        response_options = c("Yes", "No"),
        on_complete = function(rv, input) {
          try_again <- if (input$Yes == 1) {
            TRUE
          } else if (input$No == 1) {
            FALSE
          } else stop()
          if (try_again) {
            rv$test_stack <- c(test_modules$practice_questions,
                               rv$test_stack)
          }
        }))

test_modules$main_piat <-
  c(list(new("code_block",
             fun = function(rv, input) {
               intro <- new("one_btn_page",
                            body = tags$p(sprintf("You are about to proceed to the main test, where you will answer %i questions similar to the ones you just tried. Some of these may be very difficult, but don't worry, you're not expected to get everything right. If you really don't know the answer, just give your best guess.", nrow(rv$params$piat$items))))
               rv$test_stack <- c(list(intro),
                                  rv$test_stack)
               rv$piat$progress <- 1
             })),
    lapply(seq_len(nrow(piat$items)),
           function(n) {
             new("video_stimulus_NAFC",
                 prompt = tags$div(
                   tags$strong(sprintf("Question %i out of %i:", n, nrow(piat$items))),
                   tags$p("Did the final tone match the note you were imagining?")),
                 source = paste0("main/mp4/", piat$items$Filename[n], ".mp4"),
                 type = "mp4",
                 response_options = c("Match", "No match"),
                 wait = TRUE,
                 on_complete = function(rv, input) {
                   ParticipantResponse <- if (input$Match == 1) {
                     "Match"
                   } else if (input$`No match` == 1) {
                     "No match"
                   } else stop("This shouldn't happen!")
                   correct_answer <- if (piat$items$ProbeAcc[n] == 1) {
                     "Match"
                   } else if (piat$items$ProbeAcc[n] == 0) {
                     "No match"
                   } else stop()
                   ParticipantCorrect <- ParticipantResponse == correct_answer
                   rv$params$piat$items$ParticipantResponse[n] <- ParticipantResponse
                   rv$params$piat$items$ParticipantCorrect[n] <- ParticipantCorrect
                   print(as.data.frame(rv$params$piat$items))
                 })}),
    new("one_btn_page",
        body = tags$div(tags$p("Congratulations, you finished the main test!"),
                        tags$p("All that's left is a few questions for you to answer."))))

test_modules$piat_debrief <- 
  c(
    list(
      new("code_block",
          fun = function(rv, input) {
            rv$piat$debrief <- list()
          }),
      new("one_btn_page",
          body = tags$div(
            tags$p("How vivid were the musical images (the sound of the music on your mind) you formed during the task on a scale of 1 – 7 (1 – Not Vivid at all, 7 – Very Vivid)?"),
            sliderInput("slider", label = NULL, value = 4, min = 1, max = 7, step = 1)),
          on_complete = function(rv, input) {
            rv$piat$debrief$how_vivid <- input$slider
          }),
      new("one_btn_page",
          body = tags$p("Different strategies can be used to complete this task. Please rate for each of the following strategies how much of the time you used each strategy whilst completing the task."))),
    lapply(list(list(id = "sang_pitch", 
                     q = "Sang pitch in my head"),
                list(id = "used_intuition",
                     q = "Used intuition when judging probe"),
                list(id = "imagined_seeing",
                     q = "Imagined seeing something (e.g. staircase or piano keyboard)"),
                list(id = "heard_sound_in_my_head",
                     q = "Heard sound in my head"),
                list(id = "counted_arrows",
                     q = "Counted arrows and guess probe"),
                list(id = "music_theory",
                     q = "Used music theory to work it out")),
           function(x) {
             new("one_btn_page",
                 body = tags$div(
                   tags$p(paste(x$q, "(1 – Never, 7 – All the time)")),
                   sliderInput("slider", label = NULL, value = 4,
                               min = 1, max = 7, step = 1)),
                 on_complete = function(rv, input) {
                   rv$piat$debrief[[x$id]] <- input$slider
                 })
           }),
    list(new("one_btn_page",
             body = tags$div(
               tags$p("Do you have any other comments about your strategy?"),
               textAreaInput("text", label = NULL, width = "80%", height = "100px")),
             on_complete = function(rv, input) {
               rv$piat$debrief$other_comments <- input$text
               print(rv$piat$debrief)
             })))
  

test_modules$final <- 
  list(new("final_page",
           body = p("You completed the test! You may now close the browser window.")))

pages <- c( # test_modules$repeatable_practice_questions,
  new("one_btn_page",
      body = tags$div(tags$p("Congratulations, you finished the main test!"),
                      tags$p("All that's left is a few questions for you to answer."))),
  test_modules$piat_debrief,
  # test_modules$main_piat,
  test_modules$final)
