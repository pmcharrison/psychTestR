loadNamespace("GMSIData")

title <- "Music imagery"
study_id <- 11
pilot <- TRUE

message("Connecting to the GMSI database")
db <- GMSIData::db_connect()
server_quit_fun <- function() {
  message("Disconnecting from the GMSI database")
  GMSIData::db_disconnect(db)
}

display_options <- list(theme = shinytheme("readable"))

media_dir <- "http://media.gold-msi.org/test_materials/PIAT/stimuli" %>%
  gsub("/$", "", .) # just in case someone puts in a trailing slash

volume_calibration_source <- file.path(media_dir, "volume_calibration.mp3")

admin <- list(state = FALSE,
              password = "robinhood")

setup <- function(rv) {
  rv$admin <- list(logged_in = FALSE,
                   num_items = 30)
  rv$results <- list(time_started = Sys.time())
  rv$piat_item_index <- 1
}

side_panel_ui_admin_false <- 
  div(
    id = "admin_side_panel_inactive",
    shinyBS::tipify(
      el = tags$p(actionButton("admin_login_trigger", "Admin login")),
      title = "Click here to enter your administrator credentials."
    )
  )

side_panel_ui_admin_true <- 
  div(
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
    ),
    shinyBS::tipify(
      el = tags$p(uiOutput("admin_num_items_slider", inline = TRUE)),
      title = "You can set the number of items in the test here. Once the main test has started, changes here will have no effect."
    ),
    shinyBS::tipify(
      el = tags$p(actionButton("admin_logout", "Exit admin mode",
                               style = "color: white; background-color: #c62121")),
      title = "Click to sign out of administration mode."
    )
  )

renderOutputs <- function(rv, input, output) {
  # Item info
  output$item_info <- DT::renderDataTable({
    rv$current_page # for some reason changes aren't detected in rv$results$piat$items
    rv$results$piat$items
  },
  server = TRUE,
  options = list(scrollX = TRUE),
  rownames = FALSE)
  # Download results
  output$download_results <- downloadHandler(
    filename = "results.RDS",
    content = function(file) {
      saveRDS(rv$results$piat$items, file)
    }
  )
  # Admin panel
  output$side_panel_ui <-
    renderUI(
      if (!is.null(rv$admin) && rv$admin$logged_in) side_panel_ui_admin_true else side_panel_ui_admin_false
    )
  # Num items slider
  output$admin_num_items_slider <- renderUI(
    shiny::sliderInput("admin_num_items", "Num. items",
                       min = 1, max = 30, value = rv$admin$num_items,
                       step = 1, animate = TRUE, ticks = FALSE) 
  )
}

renderModals <- function(rv, input, output, session) {
  output$modals <- renderUI(tags$div(
    id = "modals",
    shinyBS::bsModal("admin_login_popup", "Admin login",
                     "null_trigger", size = "small",
                     wellPanel(
                       align = "center",
                       tags$p(passwordInput("admin_password", label = "Password")),
                       tags$p(actionButton(inputId = "submit_admin_password", "Submit"))
                     ))
  ))
}

observeEvents <- function(rv, input, session, params) {
  list(
    observeEvent(input$admin_login_trigger, toggleModal(session, "admin_login_popup", toggle = "open")),
    observeEvent(input$submit_admin_password,
                 if (input$admin_password == params$admin$password) {
                   rv$admin$logged_in <- TRUE
                   toggleModal(session, "admin_login_popup", toggle = "close")
                 } else {
                   shinyjs::alert("Incorrect password.")
                 }),
    observeEvent(input$admin_logout, {
      rv$admin$logged_in <- FALSE
      updateTextInput(session, "admin_password", value = "")
    }),
    observeEvent(input$admin_num_items, {
      if (!is.null(input$admin_num_items)) {
        assertthat::assert_that(is.numeric(input$admin_num_items))
        rv$admin$num_items <- input$admin_num_items
      }
    })
  )
}

piat <- list()

test_modules <- list()

test_modules$generic_intro <- 
  list(new("one_btn_page",
           body = tags$div(
             tags$p("Please enter your participant ID."),
             textInput("participant_id", label = NULL, placeholder = "e.g. ph93", width = "150px")),
           on_complete = function(rv, input) rv$participant_id <- input$participant_id,
           validate = function(rv, input) {
             if (input$participant_id == "") {
               shinyjs::alert("You must enter a participant ID to continue.")
               FALSE
             } else TRUE
           }),
       new("one_btn_page",
           body = tags$div(
             tags$p("When you click 'Next', you will be played some audio. You will be asked to adjust the volume to a comfortable level. Please make sure you are wearing your headphones."))),
       new("volume_calibration",
           prompt = tags$p("Please adjust the volume to a comfortable level."),
           source = volume_calibration_source,
           type = "mp3"))

test_modules$piat_intro <- withTags(list(
  new("one_btn_page",
      body = p("The Pitch Imagery Arrow Task has been designed to teach you to successfully imagine musical tones from a visual prompt.")),
  new("one_btn_page",
      body = p("Each trial starts with the word “Begin” on the screen, and you will hear an ascending major scale, which provides the key or context for that trial. You will then see a dot on the screen and hear a start note. Press 'Next' for an example of this.")),
  new("video_stimulus_NAFC",
      prompt = p("Here is an example context:"),
      source = file.path(media_dir, "training/Scale_C_ton.mp4"),
      type = "mp4",
      response_options = "Next",
      wait = TRUE),
  new("one_btn_page",
      body = p("A variable number of up and/or down arrows will then appear in a sequence, with a corresponding tone, that is stepping up or down the scale. Press 'Next' for an example of these arrows appearing after the ascending scale and start note.")),
  new("video_stimulus_NAFC",
      prompt = p("Here is an example of arrows appearing after the ascending scale and start note:"),
      source = file.path(media_dir, "training/Example_Trial_sounded_arr.mp4"),
      type = "mp4",
      response_options = "Next",
      wait = TRUE),
  new("one_btn_page",
      body = p("At some point in the trial, an arrow is shown with no tone heard. Your job is to imagine that exact missing tone. The number of tones to be imagined in each trial will vary from 1 to 5 tones. The word “hold” will appear with the last silent arrow of the sequence. Hold in your mind the sound of this last tone as you prepare to hear a test tone. Press 'Next' for an example of a single silent arrow added to our trial example.")),
  new("video_stimulus_NAFC",
      prompt = p("Here is an example:"),
      source = file.path(media_dir, "training/Example_Trial_all_arr.mp4"),
      type = "mp4",
      response_options = "Next",
      wait = TRUE),
  new("one_btn_page",
      body = p("To test the accuracy of your imagery, a test tone will be sounded and a white fixation cross will display. The tone will either match the note you are imagining or it won't match. Your task will be to determine which is the case. Press 'Next' for the full example trial and try to respond correctly.")),
  new("video_stimulus_NAFC",
      prompt = p("Here is an example complete trial:"),
      source = file.path(media_dir, "training/Example_Trial_complete.mp4"),
      type = "mp4",
      response_options = c("Match", "No match"),
      wait = TRUE),
  new("one_btn_page",
      body = p("We encourage you to just use your imagery to play the missing notes in your head, and don’t hum or move as you imagine. From earlier tests we know that using only your imagery gives the best results on the test.")),
  new("one_btn_page",
      body = p("There are 3 practice trials in which you will receive feedback. You are free to attempt these as many times as you wish to familiarise yourself with the task."))))

test_modules$practice_questions <-
  lapply(
    list(list(id = "Prac_Trial_Lvl1",
              answer = "Match"),
         list(id = "Prac_Trial_Lvl2",
              answer = "No match"),
         list(id = "Prac_Trial_Lvl3",
              answer = "Match")
    ),
    function(x) {
      list(
        new("video_stimulus_NAFC",
            prompt = tags$p("Did the final tone match the note you were imagining?"),
            source = file.path(media_dir, paste0("training/", x$id, ".mp4")),
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
              setMessage(rv, tags$p(if (practice_correct) {
                "You answered correctly!"
              } else "You answered incorrectly."))
            }),
        new("message_page")
      )}) %>% unlist

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
            decrementPageIndex(rv, by = 1 + length(test_modules$practice_questions))
          }
        }))

PIATitems <- new(
  "reactive_test_element",
  fun = function(rv) {
    items_df <- rv$results$piat$items
    item_position <- rv$piat_item_index
    new("video_stimulus_NAFC",
        prompt = tags$div(
          tags$strong(sprintf("Question %i out of %i:",
                              item_position, nrow(items_df))),
          tags$p("Did the final tone match the note you were imagining?")),
        source = file.path(media_dir,
                           paste0("main/mp4/",
                                  items_df$Filename[item_position], ".mp4")),
        type = "mp4",
        response_options = c("Match", "No match"),
        wait = TRUE,
        on_complete = function(rv, input) {
          ParticipantResponse <- if (input$Match == 1) {
            "Match"
          } else if (input$`No match` == 1) {
            "No match"
          } else stop("This shouldn't happen!")
          correct_answer <- if (items_df$ProbeAcc[item_position] == 1) {
            "Match"
          } else if (items_df$ProbeAcc[item_position] == 0) {
            "No match"
          } else stop()
          ParticipantCorrect <- ParticipantResponse == correct_answer
          rv$results$piat$items$ParticipantResponse[item_position] <- ParticipantResponse
          rv$results$piat$items$ParticipantCorrect[item_position] <- ParticipantCorrect
          if (item_position < nrow(items_df)) {
            decrementPageIndex(rv)
            rv$piat_item_index <- rv$piat_item_index + 1
          }
        })
  }
)

test_modules$main_piat <- list(
  new("code_block",
      fun = function(rv, input) {
        rv$results$piat <- list(items = getStimuli(num_items = rv$admin$num_items))
      }),
  new("reactive_test_element",
      fun = function(rv) {
        new("one_btn_page",
            body = tags$p(sprintf("You are about to proceed to the main test, where you will answer %i questions similar to the ones you just tried. You won't receive any feedback on these questions. Some might be very difficult, but don't worry, you're not expected to get everything right. If you really don't know the answer, just give your best guess.",
                                  nrow(rv$results$piat$items))))
      }),
  PIATitems
)

test_modules$piat_debrief <- 
  c(
    list(
      new(
        "one_btn_page",
        body = tags$div(tags$p("Congratulations, you finished the main test!"),
                        tags$p("All that's left is a few questions for you to answer."))),
      new("code_block",
          fun = function(rv, input) {
            rv$results$piat$debrief <- list()
          }),
      new("one_btn_page",
          body = tags$div(
            tags$p("How vivid were the musical images (the sound of the music in your mind) you formed during the task on a scale of 1 – 7 (1 – Not Vivid at all, 7 – Very Vivid)?"),
            sliderInput("slider", label = NULL, value = 4, min = 1, max = 7, step = 1)),
          on_complete = function(rv, input) {
            rv$results$piat$debrief$how_vivid <- input$slider
          }),
      new("one_btn_page",
          body = tags$p("Different strategies can be used to complete this task. Please rate for each of the following strategies how much of the time you used each strategy whilst completing the task."))),
    lapply(list(list(id = "sang_pitch_in_head", 
                     q = "Sang pitch in my head"),
                list(id = "sang_or_hummed_out_loud",
                     q = "Sang or hummed out loud"),
                list(id = "used_intuition",
                     q = "Used my gut feeling when judging whether the final note matched or not"),
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
                   rv$results$piat$debrief[[x$id]] <- input$slider
                 })
           }),
    list(new("one_btn_page",
             body = tags$div(
               tags$p("Do you have any other comments about your strategy?"),
               textAreaInput("text", label = NULL, width = "80%", height = "100px")),
             on_complete = function(rv, input) {
               rv$results$piat$debrief$other_comments <- input$text
             })))

test_modules$absolute_pitch <- 
  new("page_NAFC",
      prompt = tags$div(tags$p("Do you have absolute pitch?"),
                        tags$p("Absolute pitch is the ability to name the pitch of a note without being given a reference note, or alternatively to sing a named pitch without a reference note.")),
      response_options = c("Yes", "No"),
      on_complete = function(rv, input) {
        rv$results$absolute_pitch <- input$lastBtnPressed
      })


test_modules$save_data <- 
  new("code_block",
      fun = function(rv, input) {
        message("Saving data...")
        rv$results$time_finished <- Sys.time()
        session_id <- GMSIData::dbNewParticipant(db = db,
                                                 participant_id = rv$participant_id,
                                                 study_id = study_id, 
                                                 pilot = pilot)
        GMSIData::dbUpdateData(db = db,
                               study_id = study_id,
                               session_id = session_id,
                               data = rv$results,
                               finished = TRUE)
        print(rv$results)
      })

test_modules$final <- 
  list(new("final_page",
           body = p("You completed the test! Your responses have been recorded. You may now close the browser window.")))

pages <- c(
  test_modules$setup,
  test_modules$generic_intro,
  test_modules$piat_intro,
  test_modules$repeatable_practice_questions,
  test_modules$main_piat,
  test_modules$piat_debrief,
  test_modules$absolute_pitch,
  getBasicDemographics(),
  getGoldMSI(sub_factors = "All", ask_best_instrument = TRUE),
  test_modules$save_data,
  test_modules$final
)